owned_sample_body <- function(id = "sample-1", jobs = list(), job_count = length(jobs),
                              name = "Slide A", filename = "slide-a.svs",
                              tags = list("cohort-a"), status = "ready") {
  list(
    ownership = "mine",
    id = id,
    name = name,
    filename = filename,
    status = status,
    fileSize = "1048576",
    widthPx = 20000L,
    heightPx = 15000L,
    mpp = 0.5,
    tags = tags,
    createdAt = "2026-01-15T12:00:00Z",
    expiresAt = "2026-12-31T00:00:00Z",
    expiresAtSource = "custom",
    expiresInDays = 120L,
    willExpire = TRUE,
    trashedAt = NULL,
    jobs = jobs,
    jobCount = job_count
  )
}

sample_job_body <- function(id, status = "completed") {
  list(
    id = id,
    status = status,
    progress = if (status == "completed") 1 else 0.5,
    reservedCredits = 30L,
    markers = list("CD3", "CD8"),
    createdAt = "2026-01-15T12:01:00Z",
    startedAt = "2026-01-15T12:02:00Z",
    completedAt = if (status %in% c("completed", "partial_failed"))
      "2026-01-15T12:10:00Z" else NULL,
    errorMessage = if (status == "partial_failed") "CD8 failed" else NULL,
    resultsAvailable = status %in% c("completed", "partial_failed")
  )
}

test_that("strand_samples_list sends default scope and limit", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$get("/api/v1/samples", function(req, res) {
    res$send_json(list(
      items = list(list(
        ownership = "mine",
        id = "mine-1",
        name = NULL,
        filename = paste(req$query$scope, req$query$limit, sep = ":"),
        status = "ready",
        fileSize = "9007199254740991",
        tags = list("baseline"),
        createdAt = "2026-01-15T12:00:00Z"
      )),
      nextCursor = "cursor-2"
    ), auto_unbox = TRUE)
  })
  server <- start_strand_server(app)

  page <- strand_samples_list(testing_client(server))

  expect_identical(page$items[[1]]$ownership, "mine")
  expect_identical(page$items[[1]]$id, "mine-1")
  expect_identical(page$items[[1]]$filename, "mine:48")
  expect_type(page$items[[1]]$file_size, "double")
  expect_identical(page$items[[1]]$tags, "baseline")
  expect_identical(page$next_cursor, "cursor-2")
  expect_false("title" %in% names(page$items[[1]]))
})

test_that("strand_samples_list maps every scope plus limit, cursor, and tag", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$get("/api/v1/samples", function(req, res) {
    label <- paste(
      req$query$scope,
      req$query$limit,
      req$query$cursor %||% "missing",
      req$query$tag %||% "missing",
      sep = ":"
    )
    res$send_json(list(items = list(list(
      ownership = "public",
      id = paste0("share-", req$query$scope),
      title = label,
      thumbnailUrl = "/thumbnail",
      tags = list("tcga-coad", "stage-ii"),
      metadata = list(stage = "II", site = "colon")
    )), nextCursor = NULL), auto_unbox = TRUE)
  })
  server <- start_strand_server(app)
  client <- testing_client(server)

  for (scope in c("mine", "public", "all")) {
    page <- strand_samples_list(
      client, scope = scope, limit = 17L, cursor = "opaque", tag = "tcga-coad"
    )
    item <- page$items[[1]]
    expect_identical(item$ownership, "public")
    expect_identical(item$id, paste0("share-", scope))
    expect_identical(item$title, paste(scope, 17, "opaque", "tcga-coad", sep = ":"))
    expect_identical(item$tags, c("tcga-coad", "stage-ii"))
    expect_identical(item$metadata, list(stage = "II", site = "colon"))
    expect_false("public_id" %in% names(item))
    expect_false("filename" %in% names(item))
  }
})

test_that("strand_samples_list rejects every non-exact scope before HTTP", {
  client <- strand_client(api_key = "test", base_url = "http://127.0.0.1:1")
  invalid <- list(
    "m", "Mine", "", NA_character_, NULL, c("mine", "public"), 1, TRUE
  )

  for (scope in invalid) {
    expect_error(
      strand_samples_list(client, scope = scope),
      "scope must be exactly one of",
      fixed = TRUE
    )
  }
})

test_that("strand_samples_list rejects malformed limit before HTTP", {
  client <- strand_client(api_key = "test", base_url = "http://127.0.0.1:1")
  for (limit in list(0, 101, 1.5, Inf, NA_real_, "48", c(1, 2))) {
    expect_error(strand_samples_list(client, limit = limit), "whole number from 1 to 100")
  }
})

test_that("sample list items require a valid ownership discriminator", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$get("/api/v1/samples", function(req, res) {
    res$send_json(list(items = list(list(id = "hidden")), nextCursor = NULL),
                  auto_unbox = TRUE)
  })
  server <- start_strand_server(app)

  expect_error(
    strand_samples_list(testing_client(server)),
    "ownership must be 'mine' or 'public'"
  )
})

test_that("strand_samples_get parses owned history including partial failures", {
  skip_if_no_webfakes()
  jobs <- list(
    sample_job_body("job-new", "partial_failed"),
    sample_job_body("job-old", "completed")
  )
  app <- webfakes::new_app()
  app$get("/api/v1/samples/:id", function(req, res) {
    res$send_json(owned_sample_body(req$params$id, jobs, job_count = 2L),
                  auto_unbox = TRUE)
  })
  server <- start_strand_server(app)

  sample <- strand_samples_get(testing_client(server), "sample-1")

  expect_identical(sample$ownership, "mine")
  expect_identical(sample$id, "sample-1")
  expect_equal(sample$file_size, 1048576)
  expect_equal(sample$mpp, 0.5)
  expect_identical(sample$job_count, 2L)
  expect_length(sample$jobs, 2L)
  expect_identical(sample$jobs[[1]]$status, "partial_failed")
  expect_identical(sample$jobs[[1]]$markers, c("CD3", "CD8"))
  expect_true(sample$jobs[[1]]$results_available)
  expect_identical(sample$jobs[[1]]$error_message, "CD8 failed")
})

test_that("strand_samples_get preserves empty and capped owned job histories", {
  skip_if_no_webfakes()
  jobs <- lapply(seq_len(50), function(i) sample_job_body(paste0("job-", i)))
  app <- webfakes::new_app()
  app$get("/api/v1/samples/:id", function(req, res) {
    if (req$params$id == "empty") {
      body <- owned_sample_body(req$params$id, jobs = list(), job_count = 0L)
    } else {
      body <- owned_sample_body(req$params$id, jobs = jobs, job_count = 73L)
    }
    res$send_json(body, auto_unbox = TRUE)
  })
  server <- start_strand_server(app)
  client <- testing_client(server)

  empty <- strand_samples_get(client, "empty")
  expect_identical(empty$jobs, list())
  expect_identical(empty$job_count, 0L)

  capped <- strand_samples_get(client, "capped")
  expect_length(capped$jobs, 50L)
  expect_identical(capped$job_count, 73L)
  expect_identical(capped$jobs[[50]]$id, "job-50")
})

test_that("strand_samples_get parses the public branch without job fields", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$get("/api/v1/samples/:id", function(req, res) {
    res$send_json(list(
      ownership = "public",
      id = req$params$id,
      title = "TCGA slide",
      thumbnailUrl = "/api/v1/public/samples/share-1/thumbnail",
      tags = list("tcga-coad"),
      metadata = list(stage = "II"),
      geometry = list(widthPx = 20000L, heightPx = 15000L, mppX = 0.5, mppY = 0.5),
      viewer = list(
        pyramidUrl = "/api/v1/public/samples/share-1/zarr",
        markers = list(list(name = "CD3"), list(name = "CD8"))
      )
    ), auto_unbox = TRUE)
  })
  server <- start_strand_server(app)

  sample <- strand_samples_get(testing_client(server), "share-1")

  expect_identical(sample$ownership, "public")
  expect_identical(sample$id, "share-1")
  expect_identical(sample$markers, c("CD3", "CD8"))
  expect_identical(sample$metadata, list(stage = "II"))
  expect_equal(sample$geometry$mpp_x, 0.5)
  expect_identical(sample$pyramid_url, "/api/v1/public/samples/share-1/zarr")
  expect_false("jobs" %in% names(sample))
  expect_false("job_count" %in% names(sample))
  expect_false("public_id" %in% names(sample))
})

test_that("strand_samples_get raises a not-found error", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$get("/api/v1/samples/:id", function(req, res) {
    res$set_status(404)$send_json(
      list(error = "not_found", message = "Sample not found"),
      auto_unbox = TRUE
    )
  })
  server <- start_strand_server(app)

  expect_error(
    strand_samples_get(testing_client(server), "sample-1"),
    class = "strand_not_found_error"
  )
})

test_that("strand_patch_sample omits NULL fields and sends tags as a set", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$patch("/api/v1/samples/:id", function(req, res) {
    keys <- paste(names(req$json), collapse = ",")
    tag_shape <- if (is.list(req$json$tags)) "array" else "scalar"
    body <- owned_sample_body(
      req$params$id,
      filename = paste0("keys:", keys),
      name = tag_shape,
      tags = req$json$tags %||% list(),
      status = as.character(length(req$json$tags %||% list()))
    )
    res$send_json(body, auto_unbox = TRUE)
  })
  server <- start_strand_server(app)
  client <- testing_client(server)

  set_one <- strand_patch_sample(client, "sample-1", tags = "baseline")
  expect_identical(set_one$filename, "keys:tags")
  expect_identical(set_one$name, "array")
  expect_identical(set_one$status, "1")
  expect_identical(set_one$tags, "baseline")

  cleared <- strand_patch_sample(client, "sample-1", tags = character(0))
  expect_identical(cleared$filename, "keys:tags")
  expect_identical(cleared$name, "array")
  expect_identical(cleared$status, "0")
  expect_identical(cleared$tags, character(0))
})

test_that("strand_patch_sample sends explicit null only for clear_name", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$patch("/api/v1/samples/:id", function(req, res) {
    keys <- paste(names(req$json), collapse = ",")
    null_name <- "name" %in% names(req$json) && is.null(req$json$name)
    body <- owned_sample_body(
      req$params$id,
      name = if (null_name) NULL else req$json$name,
      filename = paste0("keys:", keys)
    )
    res$send_json(body, auto_unbox = TRUE)
  })
  server <- start_strand_server(app)
  client <- testing_client(server)

  renamed <- strand_patch_sample(client, "sample-1", name = "New name")
  expect_identical(renamed$name, "New name")
  expect_identical(renamed$filename, "keys:name")

  cleared <- strand_patch_sample(client, "sample-1", clear_name = TRUE)
  expect_null(cleared$name)
  expect_identical(cleared$filename, "keys:name")
})

test_that("strand_patch_sample combines fields and validates local semantics", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$patch("/api/v1/samples/:id", function(req, res) {
    body <- owned_sample_body(
      req$params$id,
      filename = paste(names(req$json), collapse = ","),
      name = req$json$name,
      tags = req$json$tags
    )
    body$mpp <- req$json$mpp
    res$send_json(body, auto_unbox = TRUE)
  })
  server <- start_strand_server(app)
  client <- testing_client(server)

  sample <- strand_patch_sample(
    client, "sample-1", name = "Renamed", tags = c("a", "b"), mpp = 0.26
  )
  expect_identical(sample$filename, "name,tags,mpp")
  expect_equal(sample$mpp, 0.26)
  expect_identical(sample$tags, c("a", "b"))

  expect_error(
    strand_patch_sample(client, "sample-1", name = "x", clear_name = TRUE),
    "cannot be combined"
  )
  expect_error(
    strand_patch_sample(client, "sample-1"),
    "at least one of"
  )
  for (value in list(0, -0.1, 100.1, Inf, NaN, NA_real_, "0.5", c(0.5, 0.5))) {
    expect_error(
      strand_patch_sample(client, "sample-1", mpp = value),
      "greater than 0 and at most 100"
    )
  }
})

test_that("sample functions validate the client argument", {
  expect_error(strand_samples_list(list()), "client must be a strand_client")
  expect_error(strand_samples_get(list(), "sample-1"), "client must be a strand_client")
  expect_error(
    strand_patch_sample(list(), "sample-1", name = "x"),
    "client must be a strand_client"
  )
})
