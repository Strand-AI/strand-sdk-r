test_that("format-driven result request and status preserve content selection", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  response <- function(status) list(
    schemaVersion = "1.0", status = status, format = "ome-zarr-zip",
    selection = list(
      includeHe = TRUE,
      includeSegmentation = TRUE,
      includeOriginalUpload = TRUE
    ),
    expiresAt = NULL, retryable = FALSE, error = NULL,
    artifacts = list(prediction = list(downloadUrl = NULL))
  )
  app$post("/api/v1/jobs/:id/exports", function(req, res) {
    res$set_status(202L)$send_json(response("pending"), auto_unbox = TRUE)
  })
  app$get("/api/v1/jobs/:id/exports", function(req, res) {
    res$send_json(response("ready"), auto_unbox = TRUE)
  })
  server <- start_strand_server(app)
  job <- structure(list(id = "job-1", client = testing_client(server)), class = "strand_job")

  expect_equal(
    strand_export_request(job, "ome-zarr-zip", include_he = TRUE,
                          include_segmentation = TRUE,
                          include_original_upload = TRUE)$status,
    "pending"
  )
  expect_equal(
    strand_export_get(job, "ome-zarr-zip", include_he = TRUE,
                      include_segmentation = TRUE,
                      include_original_upload = TRUE)$status,
    "ready"
  )
})

test_that("download stops when stale worker is surfaced as retryable failure", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  response <- function(status, retryable, error = NULL) list(
    schemaVersion = "1.0", status = status, format = "ome-tiff",
    selection = list(includeHe = FALSE, includeSegmentation = FALSE),
    expiresAt = NULL, retryable = retryable, error = error,
    artifacts = list(prediction = list(downloadUrl = NULL))
  )
  app$post("/api/v1/jobs/:id/exports", function(req, res) {
    res$set_status(202L)$send_json(response("running", FALSE), auto_unbox = TRUE)
  })
  app$get("/api/v1/jobs/:id/exports", function(req, res) {
    res$send_json(
      response("failed", TRUE, "Export worker stopped updating; repeat POST to retry"),
      auto_unbox = TRUE
    )
  })
  server <- start_strand_server(app)
  job <- structure(list(id = "job-1", client = testing_client(server)), class = "strand_job")

  expect_error(
    strand_export_download(job, "ome-tiff", tempfile(), poll_interval = 0),
    "stopped updating"
  )
})
