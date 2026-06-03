# End-to-end tests for strand_run() / predict.strand_client().
#
# Mocks all five REST endpoints plus the GCS resumable PUT so the orchestrator
# exercises upload → submit → wait → download in one go without a live backend.

JOB_ID <- "33333333-3333-3333-3333-333333333333"
UPLOAD_ID <- "11111111-1111-1111-1111-111111111111"
RESULT_BASE <- "predictions/org/33333333"


array_meta <- function(shape, chunk, dtype = "float32") {
  list(
    zarr_format = 3L,
    node_type = "array",
    shape = shape,
    data_type = dtype,
    chunk_grid = list(
      name = "regular",
      configuration = list(chunk_shape = chunk)
    ),
    chunk_key_encoding = list(
      name = "default",
      configuration = list(separator = "/")
    ),
    codecs = list(list(
      name = "bytes",
      configuration = list(endian = "little")
    )),
    fill_value = 0L
  )
}


root_meta <- function(markers) {
  multiscales <- list(list(
    version = "0.5",
    name = "H&E",
    datasets = list(list(path = "he/0"))
  ))
  for (m in markers) {
    multiscales[[length(multiscales) + 1L]] <- list(
      version = "0.5",
      name = m,
      datasets = list(list(path = paste0("markers/", m, "/0")))
    )
  }
  list(
    zarr_format = 3L,
    node_type = "group",
    attributes = list(
      ome = list(version = "0.5"),
      multiscales = multiscales
    )
  )
}


# Builds a webfakes app that fakes the full pipeline. Markers must match what
# the test will request so the mock zarr layout lines up.
build_pipeline_app <- function(markers) {
  app <- webfakes::new_app()

  # The GCS resumable PUT URL — point it back at our mock host so the chunked
  # PUT goes here too. We register the path lazily once the server is up.
  app$locals <- list(
    upload_url = NULL,
    markers = markers
  )

  # 1) Initiate upload — return uploadUrl pointing at our own server.
  app$post("/api/v1/uploads", function(req, res) {
    # Re-build the GCS-like URL using whatever host the request came in on.
    host <- req$get_header("host")
    upload_url <- sprintf("http://%s/_gcs/resumable?upload_id=abc", host)
    app$locals$upload_url <- upload_url
    res$set_status(200L)$send_json(
      list(
        uploadId = UPLOAD_ID,
        uploadUrl = upload_url,
        gcsPath = sprintf("uploads/org/%s/slide.svs", UPLOAD_ID)
      ),
      auto_unbox = TRUE
    )
  })

  # 2) Chunked PUT to GCS — accept any chunk, return 200 for final, 308 mid.
  app$put("/_gcs/resumable", function(req, res) {
    rng <- req$get_header("content-range") %||% ""
    parts <- strsplit(rng, "[/-]")[[1]]
    end_byte <- as.integer(parts[2])
    end_total <- as.integer(parts[3])
    is_final <- !is.na(end_byte) && !is.na(end_total) && end_total == (end_byte + 1L)
    res$set_status(if (is_final) 200L else 308L)$send("")
  })

  # 3) Complete upload.
  app$post("/api/v1/uploads/:id/complete", function(req, res) {
    res$set_status(200L)$send_json(
      list(
        uploadId = UPLOAD_ID,
        status = "ready",
        widthPx = 1024L,
        heightPx = 1024L,
        dimensionsSource = "sharp"
      ),
      auto_unbox = TRUE
    )
  })

  # 4) Submit predict.
  app$post("/api/v1/predict", function(req, res) {
    res$set_status(202L)$send_json(
      list(jobId = JOB_ID, reservedCredits = 42L, status = "queued"),
      auto_unbox = TRUE
    )
  })

  # 5) Job status — return completed straight away so the wait loop exits.
  #    Echoes the canonical v0.X label per design note §4 — the platform
  #    normalizes legacy aliases before persisting, so this field is always
  #    a live v0.X id on responses.
  app$get("/api/v1/jobs/:id", function(req, res) {
    res$set_status(200L)$send_json(
      list(
        id = JOB_ID,
        status = "completed",
        progress = 1.0,
        reservedCredits = 42L,
        markers = as.list(markers),
        model = "v0.5",
        createdAt = NULL,
        startedAt = NULL,
        completedAt = "2026-05-20T10:05:00Z",
        errorMessage = NULL,
        resultsAvailable = TRUE
      ),
      auto_unbox = TRUE
    )
  })

  # 6) Result metadata + per-file proxy.
  app$get("/api/v1/jobs/:id/results", function(req, res) {
    res$set_status(200L)$send_json(
      list(
        resultUrl = "https://storage.googleapis.com/.../zarr.json?sig=...",
        resultBasePath = RESULT_BASE,
        expiresAt = "2026-05-20T11:05:00Z"
      ),
      auto_unbox = TRUE
    )
  })

  root <- root_meta(markers)
  he_arr_meta <- array_meta(c(3L, 2L, 2L), c(3L, 2L, 2L), dtype = "uint8")
  marker_arr_meta <- array_meta(c(1L, 2L, 2L), c(1L, 2L, 2L))
  he_chunk <- as.raw(0:11)
  marker_chunk <- writeBin(c(1, 2, 3, 4), raw(),
                            size = 4L, endian = "little")

  send_json_bytes <- function(res, obj) {
    res$
      set_header("content-type", "application/json")$
      set_status(200L)$
      send(jsonlite::toJSON(obj, auto_unbox = TRUE))
  }
  send_raw <- function(res, bytes) {
    res$
      set_header("content-type", "application/octet-stream")$
      set_status(200L)$
      send(bytes)
  }

  app$get("/api/v1/jobs/:id/results/files/zarr.json",
          function(req, res) send_json_bytes(res, root))
  app$get("/api/v1/jobs/:id/results/files/he/0/zarr.json",
          function(req, res) send_json_bytes(res, he_arr_meta))
  app$get("/api/v1/jobs/:id/results/files/he/0/c/0/0/0",
          function(req, res) send_raw(res, he_chunk))
  for (m in markers) {
    local({
      mm <- m
      app$get(sprintf("/api/v1/jobs/:id/results/files/markers/%s/0/zarr.json", mm),
              function(req, res) send_json_bytes(res, marker_arr_meta))
      app$get(sprintf("/api/v1/jobs/:id/results/files/markers/%s/0/c/0/0/0", mm),
              function(req, res) send_raw(res, marker_chunk))
    })
  }

  app
}


make_slide_file <- function(dir, bytes = 256L * 1024L) {
  path <- file.path(dir, "slide.svs")
  writeBin(raw(bytes), path)
  path
}


test_that("strand_run runs the full pipeline and writes the zarr store", {
  skip_if_no_webfakes()
  markers <- c("CD3", "CD8")
  server <- start_strand_server(build_pipeline_app(markers))
  client <- testing_client(server)

  tmp <- withr::local_tempdir()
  slide <- make_slide_file(tmp)
  out <- file.path(tmp, "out")

  result <- strand_run(client, slide, markers,
                       output_dir = out,
                       poll_interval_sec = 0.05,
                       timeout_sec = 10)

  expect_s3_class(result, "strand_predict_result")
  expect_equal(result$job_id, JOB_ID)
  expect_equal(result$status, "completed")
  expect_equal(result$credits_used, 42L)
  # `result$model` echoes the canonical v0.X label the platform persisted —
  # never a legacy alias, never NULL on a fresh response. This is the §0
  # hard constraint manifesting on the R result list.
  expect_equal(result$model, "v0.5")
  expect_equal(result$output_dir, out)
  expect_setequal(names(result$marker_outputs), c("CD3", "CD8"))
  expect_equal(result$marker_outputs$CD3, file.path(out, "markers", "CD3"))
  expect_true(file.exists(file.path(out, "zarr.json")))
  expect_true(file.exists(file.path(out, "markers", "CD3", "0", "c", "0", "0", "0")))
  expect_true(file.exists(file.path(out, "markers", "CD8", "0", "c", "0", "0", "0")))
})


test_that("predict() S3 method dispatches to strand_run for a client", {
  skip_if_no_webfakes()
  server <- start_strand_server(build_pipeline_app("CD3"))
  client <- testing_client(server)

  tmp <- withr::local_tempdir()
  slide <- make_slide_file(tmp)

  result <- predict(client, slide, markers = "CD3",
                    poll_interval_sec = 0.05, timeout_sec = 10)

  expect_s3_class(result, "strand_predict_result")
  expect_equal(result$status, "completed")
  expect_null(result$output_dir)
  expect_length(result$marker_outputs, 0L)
})


test_that("strand_run skips download when output_dir is NULL", {
  skip_if_no_webfakes()
  server <- start_strand_server(build_pipeline_app("CD3"))
  client <- testing_client(server)

  tmp <- withr::local_tempdir()
  slide <- make_slide_file(tmp)

  result <- strand_run(client, slide, "CD3",
                       poll_interval_sec = 0.05,
                       timeout_sec = 10)
  expect_equal(result$status, "completed")
  expect_null(result$output_dir)
  expect_length(result$marker_outputs, 0L)
  expect_s3_class(result$job, "strand_job")
})


test_that("strand_run reports progress for every pipeline stage", {
  skip_if_no_webfakes()
  server <- start_strand_server(build_pipeline_app("CD3"))
  client <- testing_client(server)

  tmp <- withr::local_tempdir()
  slide <- make_slide_file(tmp)
  out <- file.path(tmp, "out")

  stages <- character()
  strand_run(client, slide, "CD3",
             output_dir = out,
             poll_interval_sec = 0.05,
             timeout_sec = 10,
             on_progress = function(stage, fraction) {
               stages <<- c(stages, stage)
             })

  expect_true(all(c("upload", "submit", "wait", "download") %in% unique(stages)))
})


test_that("strand_run attaches upload_id to errors raised after upload", {
  skip_if_no_webfakes()
  # Build a pipeline app from scratch that succeeds at upload but rejects
  # predict with unknown_markers — exercising the error path that needs
  # upload_id attached so callers can recover via strand_predict().
  app <- webfakes::new_app()
  app$post("/api/v1/uploads", function(req, res) {
    host <- req$get_header("host")
    res$set_status(200L)$send_json(list(
      uploadId = UPLOAD_ID,
      uploadUrl = sprintf("http://%s/_gcs/resumable?upload_id=abc", host),
      gcsPath = sprintf("uploads/org/%s/slide.svs", UPLOAD_ID)
    ), auto_unbox = TRUE)
  })
  app$put("/_gcs/resumable", function(req, res) {
    rng <- req$get_header("content-range") %||% ""
    parts <- strsplit(rng, "[/-]")[[1]]
    end_byte <- as.integer(parts[2])
    end_total <- as.integer(parts[3])
    is_final <- !is.na(end_byte) && !is.na(end_total) && end_total == (end_byte + 1L)
    res$set_status(if (is_final) 200L else 308L)$send("")
  })
  app$post("/api/v1/uploads/:id/complete", function(req, res) {
    res$set_status(200L)$send_json(list(
      uploadId = UPLOAD_ID, status = "ready",
      widthPx = 1024L, heightPx = 1024L
    ), auto_unbox = TRUE)
  })
  app$post("/api/v1/predict", function(req, res) {
    res$set_status(400L)$send_json(list(
      error = "unknown_markers",
      message = "Unknown marker: NOPE",
      unknownMarkers = list("NOPE"),
      knownMarkersSample = list("CD3", "CD8")
    ), auto_unbox = TRUE)
  })
  server <- start_strand_server(app)
  client <- testing_client(server)

  tmp <- withr::local_tempdir()
  slide <- make_slide_file(tmp)

  err <- tryCatch(
    strand_run(client, slide, "NOPE",
               poll_interval_sec = 0.05, timeout_sec = 10),
    strand_unknown_markers_error = function(e) e
  )
  expect_s3_class(err, "strand_unknown_markers_error")
  expect_equal(err$upload_id, UPLOAD_ID)
  expect_equal(err$unknown, "NOPE")
})


test_that("strand_run progress callback never receives non-numeric fractions", {
  skip_if_no_webfakes()
  server <- start_strand_server(build_pipeline_app("CD3"))
  client <- testing_client(server)

  tmp <- withr::local_tempdir()
  slide <- make_slide_file(tmp)
  out <- file.path(tmp, "out")

  fractions <- numeric()
  strand_run(client, slide, "CD3",
             output_dir = out,
             poll_interval_sec = 0.05,
             timeout_sec = 10,
             on_progress = function(stage, fraction) {
               fractions <<- c(fractions, fraction)
             })

  expect_true(all(is.numeric(fractions)))
  expect_true(all(!is.na(fractions)))
  expect_true(all(fractions >= 0 & fractions <= 1))
})


test_that("strand_run with wait=FALSE returns the job after upload+submit only", {
  skip_if_no_webfakes()
  # Hand-rolled minimal app: upload + submit succeed; /jobs/:id and
  # /jobs/:id/results return errors so any stray .wait()/.download() in the
  # wait=FALSE path surfaces as a test failure rather than silently passing.
  app <- webfakes::new_app()
  app$post("/api/v1/uploads", function(req, res) {
    host <- req$get_header("host")
    res$set_status(200L)$send_json(list(
      uploadId = UPLOAD_ID,
      uploadUrl = sprintf("http://%s/_gcs/resumable?upload_id=abc", host),
      gcsPath = sprintf("uploads/org/%s/slide.svs", UPLOAD_ID)
    ), auto_unbox = TRUE)
  })
  app$put("/_gcs/resumable", function(req, res) {
    rng <- req$get_header("content-range") %||% ""
    parts <- strsplit(rng, "[/-]")[[1]]
    end_byte <- as.integer(parts[2])
    end_total <- as.integer(parts[3])
    is_final <- !is.na(end_byte) && !is.na(end_total) && end_total == (end_byte + 1L)
    res$set_status(if (is_final) 200L else 308L)$send("")
  })
  app$post("/api/v1/uploads/:id/complete", function(req, res) {
    res$set_status(200L)$send_json(list(
      uploadId = UPLOAD_ID, status = "ready",
      widthPx = 1024L, heightPx = 1024L
    ), auto_unbox = TRUE)
  })
  app$post("/api/v1/predict", function(req, res) {
    res$set_status(202L)$send_json(
      list(jobId = JOB_ID, reservedCredits = 42L, status = "queued"),
      auto_unbox = TRUE
    )
  })
  app$get("/api/v1/jobs/:id", function(req, res) {
    res$set_status(500L)$send_json(
      list(error = "should_not_poll", message = "wait=FALSE must skip polling"),
      auto_unbox = TRUE
    )
  })
  app$get("/api/v1/jobs/:id/results", function(req, res) {
    res$set_status(500L)$send_json(
      list(error = "should_not_download", message = "wait=FALSE must skip download"),
      auto_unbox = TRUE
    )
  })

  server <- start_strand_server(app)
  client <- testing_client(server)

  tmp <- withr::local_tempdir()
  slide <- make_slide_file(tmp)

  stages <- character()
  job <- strand_run(client, slide, "CD3",
                    wait = FALSE,
                    output_dir = file.path(tmp, "out"),
                    poll_interval_sec = 0.05,
                    timeout_sec = 10,
                    on_progress = function(stage, fraction) {
                      stages <<- c(stages, stage)
                    })
  expect_s3_class(job, "strand_job")
  expect_equal(job$id, JOB_ID)
  expect_equal(job$reserved_credits, 42L)
  # Progress stages must include upload + submit but NOT wait / download.
  expect_true(all(c("upload", "submit") %in% stages))
  expect_false("wait" %in% stages)
  expect_false("download" %in% stages)
})


test_that("strand_run forwards model on the submit body", {
  skip_if_no_webfakes()
  # Hand-rolled app — the handler runs out-of-process, so we encode the
  # received `model` into `reservedCredits` as a sentinel.
  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$post("/api/v1/uploads", function(req, res) {
    host <- req$get_header("host")
    res$set_status(200L)$send_json(list(
      uploadId = UPLOAD_ID,
      uploadUrl = sprintf("http://%s/_gcs/resumable?upload_id=abc", host),
      gcsPath = sprintf("uploads/org/%s/slide.svs", UPLOAD_ID)
    ), auto_unbox = TRUE)
  })
  app$put("/_gcs/resumable", function(req, res) {
    rng <- req$get_header("content-range") %||% ""
    parts <- strsplit(rng, "[/-]")[[1]]
    end_byte <- as.integer(parts[2])
    end_total <- as.integer(parts[3])
    is_final <- !is.na(end_byte) && !is.na(end_total) && end_total == (end_byte + 1L)
    res$set_status(if (is_final) 200L else 308L)$send("")
  })
  app$post("/api/v1/uploads/:id/complete", function(req, res) {
    res$set_status(200L)$send_json(list(
      uploadId = UPLOAD_ID, status = "ready",
      widthPx = 1024L, heightPx = 1024L
    ), auto_unbox = TRUE)
  })
  app$post("/api/v1/predict", function(req, res) {
    sentinel <- if (identical(req$json$model, "v0.5")) 222L else 0L
    res$set_status(202L)$send_json(
      list(jobId = JOB_ID, reservedCredits = sentinel, status = "queued"),
      auto_unbox = TRUE
    )
  })

  server <- start_strand_server(app)
  client <- testing_client(server)

  tmp <- withr::local_tempdir()
  slide <- make_slide_file(tmp)

  job <- strand_run(client, slide, "CD3",
                    model = "v0.5",
                    wait = FALSE,
                    poll_interval_sec = 0.05,
                    timeout_sec = 10)
  expect_s3_class(job, "strand_job")
  expect_equal(job$reserved_credits, 222L)
})


test_that("strand_run validates markers and image_path before any I/O", {
  client <- strand_client(api_key = "sk-strand-test",
                          base_url = "http://127.0.0.1:1")
  tmp <- withr::local_tempdir()
  slide <- make_slide_file(tmp)

  expect_error(strand_run(client, slide, character(0)),
               "at least one non-empty entry")
  expect_error(strand_run(client, file.path(tmp, "missing.svs"), "CD3"),
               "No such file")
})
