# Results download + SpatialExperiment conversion.
#
# The platform writes a zarr v3 OME store under <resultBasePath>:
#
#   zarr.json                       root group; one `multiscales` entry per
#                                   modality (H&E + one per predicted marker)
#   he/{level}/zarr.json            H&E source array per pyramid level
#   he/{level}/c/0/{cr}/{cc}        chunk bytes
#   markers/{name}/{level}/...      one [1, H, W] array per marker per level
#
# Chunks are little-endian raw bytes; only the "bytes" codec is supported.

#' Download job results and convert to a SpatialExperiment
#'
#' Walks the OME-Zarr result store via the API-key-authenticated proxy at
#' `/api/v1/jobs/{id}/results/files/{path}`, assembles the per-marker arrays
#' into one matrix (rows = markers, columns = pixels), and returns a
#' [`SpatialExperiment::SpatialExperiment()`] object.
#'
#' For large slides this materializes a dense `n_markers x (H * W)` matrix in
#' memory; pass `path` to mirror the zarr store to disk instead and read
#' selectively with `Rarr`.
#'
#' @param job A `strand_job` from [strand_predict()].
#' @param path Optional output directory. When set, downloads the store to
#'   disk and returns the directory path; no `SpatialExperiment` is built.
#' @param level Pyramid level to read; `0` is full resolution.
#' @param markers Optional character vector restricting which multiscales to
#'   load. Defaults to every multiscale except `"H&E"`.
#'
#' @return A `SpatialExperiment` with `assay = "expression"` and pixel
#'   coordinates in `spatialCoords()`. When `path` is supplied, the directory
#'   path is returned invisibly.
#'
#' @examples
#' \dontrun{
#' job <- strand_predict(client, upload$id, c("CD3", "CD8"))
#' strand_job_wait(job)
#' spe <- strand_download_results(job)
#' }
#' @export
strand_download_results <- function(job, path = NULL, level = 0L,
                                    markers = NULL) {
  if (!inherits(job, "strand_job")) {
    stop("job must be a strand_job (from strand_predict())", call. = FALSE)
  }
  meta <- strand_perform_json(job$client, sprintf("jobs/%s/results", job$id))

  if (!is.null(path)) {
    return(invisible(strand_results_download_to(job, path)))
  }

  root <- strand_get_json(job, "zarr.json")
  names_all <- strand_multiscale_names(root, include_he = FALSE)
  if (is.null(markers)) {
    markers <- names_all
  } else {
    missing <- setdiff(markers, names_all)
    if (length(missing) > 0L) {
      stop("Missing multiscale(s) in result: ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
  }
  if (length(markers) == 0L) {
    stop("No marker multiscales to assemble into SpatialExperiment.",
         call. = FALSE)
  }

  channels <- vector("list", length(markers))
  h_dim <- NA_integer_
  w_dim <- NA_integer_
  for (i in seq_along(markers)) {
    out <- strand_results_to_array(job, name = markers[i], level = level,
                                    root_meta = root)
    arr <- out$array
    dims <- dim(arr)
    if (length(dims) != 3L || dims[1L] != 1L) {
      stop("Expected marker array shape [1, H, W]; got ",
           paste(dims, collapse = " x "), " for ", markers[i], call. = FALSE)
    }
    if (i == 1L) {
      h_dim <- dims[2L]; w_dim <- dims[3L]
    } else if (dims[2L] != h_dim || dims[3L] != w_dim) {
      stop("Mismatched HxW across markers", call. = FALSE)
    }
    channels[[i]] <- arr[1L, , , drop = TRUE]
  }
  stack <- array(unlist(channels, use.names = FALSE),
                  dim = c(h_dim, w_dim, length(markers)))
  stack <- aperm(stack, c(3L, 1L, 2L))   # [C, H, W]

  strand_results_to_spatial_experiment(
    stack, markers,
    base_path = meta$resultBasePath
  )
}

#' Download a single multiscale as a raw `[C, H, W]` numeric array
#'
#' @inheritParams strand_download_results
#' @param name Name of the multiscale to read (e.g. `"CD3"`). When omitted,
#'   defaults to the first marker multiscale (excluding H&E).
#' @param root_meta Optional pre-fetched root group metadata (internal use).
#' @return A list with `array` (3-D `[C, H, W]`) and `meta` (parsed zarr.json).
#' @export
strand_results_to_array <- function(job, name = NULL, level = 0L,
                                    root_meta = NULL) {
  if (is.null(root_meta)) root_meta <- strand_get_json(job, "zarr.json")
  target <- name
  if (is.null(target)) {
    markers <- strand_multiscale_names(root_meta, include_he = FALSE)
    if (length(markers) == 0L) {
      stop("No marker multiscales in result; pass name=... to read H&E",
           call. = FALSE)
    }
    target <- markers[1L]
  }
  path <- strand_dataset_path(root_meta, target, level)
  array_meta <- strand_get_json(job, sprintf("%s/zarr.json", path))
  arr <- strand_decode_array(array_meta, function(chunk_key) {
    strand_get_raw(job, sprintf("%s/%s", path, chunk_key))
  })
  list(array = arr, meta = array_meta)
}

#' Build a `SpatialExperiment` from a `[C, H, W]` array
#'
#' @param array 3-D array `[C, H, W]`.
#' @param markers Character vector of marker / channel names; length equals C.
#' @param base_path Optional GCS path; stored in `metadata(spe)$strand$base_path`.
#' @return A `SpatialExperiment` with `assay = "expression"` and pixel coords
#'   in `spatialCoords`.
#' @export
strand_results_to_spatial_experiment <- function(array, markers,
                                                  base_path = NA_character_) {
  if (!requireNamespace("SpatialExperiment", quietly = TRUE)) {
    stop("strand_download_results() requires the SpatialExperiment package. ",
         "Install with BiocManager::install('SpatialExperiment').",
         call. = FALSE)
  }
  dims <- dim(array)
  c_dim <- dims[1L]; h_dim <- dims[2L]; w_dim <- dims[3L]
  if (length(markers) != c_dim) {
    markers <- paste0("ch", seq_len(c_dim))
  }
  exprs <- matrix(array, nrow = c_dim, ncol = h_dim * w_dim,
                   dimnames = list(markers, NULL))
  # Match the column order produced by flattening [C, H, W] with R's
  # column-major storage: the second axis (H) is the fastest-varying.
  xs <- rep(seq_len(w_dim), each = h_dim) - 1L
  ys <- rep(seq_len(h_dim), times = w_dim) - 1L
  spatial <- cbind(x = xs, y = ys)
  SpatialExperiment::SpatialExperiment(
    assays = list(expression = exprs),
    spatialCoords = spatial,
    metadata = list(strand = list(base_path = base_path,
                                  shape_chw = c(c_dim, h_dim, w_dim)))
  )
}

# ---------- internal helpers ----------

strand_multiscale_names <- function(root_meta, include_he = FALSE) {
  attrs <- root_meta$attributes
  ms <- attrs$multiscales
  if (is.null(ms)) return(character(0))
  out <- vapply(ms, function(m) {
    if (is.null(m$name)) NA_character_ else as.character(m$name)
  }, character(1))
  out <- out[!is.na(out)]
  if (!include_he) out <- out[out != "H&E"]
  out
}

strand_dataset_path <- function(root_meta, name, level) {
  attrs <- root_meta$attributes
  ms <- attrs$multiscales
  for (m in ms %||% list()) {
    if (!identical(as.character(m$name), as.character(name))) next
    datasets <- m$datasets
    if (level < 0L || level >= length(datasets)) {
      stop("Level ", level, " out of range for multiscale ", name, call. = FALSE)
    }
    p <- datasets[[level + 1L]]$path
    if (is.null(p)) stop("Missing path for multiscale ", name, call. = FALSE)
    return(as.character(p))
  }
  stop("No multiscale named ", name, " in result", call. = FALSE)
}

strand_results_download_to <- function(job, target) {
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  root <- strand_get_json(job, "zarr.json")
  writeLines(jsonlite::toJSON(root, auto_unbox = TRUE), file.path(target, "zarr.json"))
  ms <- root$attributes$multiscales %||% list()
  for (m in ms) {
    datasets <- m$datasets %||% list()
    for (ds in datasets) {
      path <- ds$path
      if (is.null(path)) next
      array_meta <- strand_get_json(job, sprintf("%s/zarr.json", path))
      lvl_dir <- file.path(target, path)
      dir.create(lvl_dir, recursive = TRUE, showWarnings = FALSE)
      writeLines(jsonlite::toJSON(array_meta, auto_unbox = TRUE),
                 file.path(lvl_dir, "zarr.json"))
      for (chunk_key in strand_enumerate_chunks(array_meta)) {
        rel <- sprintf("%s/%s", path, chunk_key)
        raw <- strand_get_raw(job, rel)
        out <- file.path(target, rel)
        dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
        writeBin(raw, out)
      }
    }
  }
  invisible(target)
}

strand_get_raw <- function(job, rel_path) {
  strand_perform_raw(job$client,
                     sprintf("jobs/%s/results/files/%s", job$id, rel_path))
}

strand_get_json <- function(job, rel_path) {
  raw <- strand_get_raw(job, rel_path)
  jsonlite::fromJSON(rawToChar(raw), simplifyVector = FALSE)
}

strand_decode_array <- function(array_meta, fetch_chunk) {
  shape <- unlist(array_meta$shape)
  chunks <- unlist(array_meta$chunk_grid$configuration$chunk_shape)
  if (length(shape) != 3L) {
    stop("Expected 3-dim [C, H, W] array; got shape ",
         paste(shape, collapse = " x "), call. = FALSE)
  }
  dtype <- array_meta$data_type
  if (!dtype %in% names(.strand_dtype_size)) {
    stop("Unsupported zarr dtype: ", dtype, call. = FALSE)
  }
  for (c in array_meta$codecs %||% list()) {
    if (!is.null(c$name) && c$name != "bytes") {
      stop("Unsupported zarr codec: ", c$name, call. = FALSE)
    }
  }
  c_dim <- shape[1L]; h_dim <- shape[2L]; w_dim <- shape[3L]
  chunk_c <- chunks[1L]; chunk_h <- chunks[2L]; chunk_w <- chunks[3L]
  if (chunk_c != c_dim) {
    stop("R SDK currently expects all channels in a single chunk along C",
         call. = FALSE)
  }
  full <- array(0, dim = c(c_dim, h_dim, w_dim))
  rows <- ceiling(h_dim / chunk_h)
  cols <- ceiling(w_dim / chunk_w)
  for (cr in seq_len(rows) - 1L) {
    for (cc in seq_len(cols) - 1L) {
      raw_chunk <- fetch_chunk(sprintf("c/0/%d/%d", cr, cc))
      buf <- strand_decode_chunk(raw_chunk, dtype,
                                  c(chunk_c, chunk_h, chunk_w))
      y0 <- cr * chunk_h + 1L; x0 <- cc * chunk_w + 1L
      y1 <- min(y0 + chunk_h - 1L, h_dim); x1 <- min(x0 + chunk_w - 1L, w_dim)
      full[, y0:y1, x0:x1] <- buf[, seq_len(y1 - y0 + 1L), seq_len(x1 - x0 + 1L)]
    }
  }
  full
}

strand_decode_chunk <- function(raw_bytes, dtype, chunk_shape) {
  item_size <- .strand_dtype_size[[dtype]]
  what <- .strand_dtype_what[[dtype]]
  signed <- .strand_dtype_signed[[dtype]]
  expected <- prod(chunk_shape) * item_size
  if (length(raw_bytes) != expected) {
    stop("Chunk byte size mismatch: ", length(raw_bytes), " vs expected ",
         expected, call. = FALSE)
  }
  values <- readBin(raw_bytes, what = what, n = prod(chunk_shape),
                    size = item_size, signed = signed, endian = "little")
  array(values, dim = chunk_shape)
}

strand_enumerate_chunks <- function(array_meta) {
  shape <- unlist(array_meta$shape)
  chunks <- unlist(array_meta$chunk_grid$configuration$chunk_shape)
  grid <- ceiling(shape / chunks)
  enc <- array_meta$chunk_key_encoding$configuration
  sep <- if (is.null(enc$separator)) "/" else enc$separator
  walk <- function(prefix, dim) {
    if (dim > length(grid)) return(paste(prefix, collapse = sep))
    out <- character(0)
    for (i in seq_len(grid[dim]) - 1L) {
      out <- c(out, walk(c(prefix, i), dim + 1L))
    }
    out
  }
  paste0("c/", walk(integer(0), 1L))
}

.strand_dtype_size <- list(
  uint8 = 1L, int8 = 1L,
  uint16 = 2L, int16 = 2L,
  uint32 = 4L, int32 = 4L,
  float32 = 4L, float64 = 8L
)
.strand_dtype_what <- list(
  uint8 = "integer", int8 = "integer",
  uint16 = "integer", int16 = "integer",
  uint32 = "integer", int32 = "integer",
  float32 = "double", float64 = "double"
)
.strand_dtype_signed <- list(
  uint8 = FALSE, int8 = TRUE,
  uint16 = FALSE, int16 = TRUE,
  uint32 = FALSE, int32 = TRUE,
  float32 = TRUE, float64 = TRUE
)
