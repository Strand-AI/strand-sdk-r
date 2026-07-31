test_that("OME-TIFF request and get expose asynchronous export status", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$post("/api/v1/jobs/:id/exports/ome-tiff", function(req, res) {
    res$set_status(202L)$send_json(list(
      status = "pending",
      format = "ome-tiff",
      sizeBytes = NULL,
      updatedAt = "2026-07-30T10:00:00Z"
    ), auto_unbox = TRUE)
  })
  app$get("/api/v1/jobs/:id/exports/ome-tiff", function(req, res) {
    res$send_json(list(
      status = "ready",
      format = "ome-tiff",
      sizeBytes = 1024L,
      downloadUrl = "https://storage.example/export.ome.tiff",
      downloadUrlExpiresAt = "2026-07-30T11:00:00Z",
      updatedAt = "2026-07-30T10:05:00Z"
    ), auto_unbox = TRUE)
  })
  server <- start_strand_server(app)
  client <- testing_client(server)
  job <- structure(list(id = "job-1", client = client), class = "strand_job")

  requested <- strand_ome_tiff_request(job)
  current <- strand_ome_tiff_get(job)

  expect_s3_class(requested, "strand_ome_tiff_export")
  expect_equal(requested$status, "pending")
  expect_equal(current$status, "ready")
  expect_equal(current$size_bytes, 1024)
  expect_equal(current$download_url_expires_at, "2026-07-30T11:00:00Z")
})

test_that("strand_download_ome_tiff polls and downloads the signed URL", {
  skip_if_no_webfakes()
  download_app <- webfakes::new_app()
  download_app$get("/export.ome.tiff", function(req, res) {
    res$set_type("application/octet-stream")
    res$send(charToRaw("TIFF"))
  })
  download_server <- start_strand_server(download_app)
  download_url <- paste0(download_server$url(), "/export.ome.tiff")

  app <- webfakes::new_app()
  app$post("/api/v1/jobs/:id/exports/ome-tiff", function(req, res) {
    res$set_status(202L)$send_json(list(
      status = "running", format = "ome-tiff", sizeBytes = NULL,
      updatedAt = NULL
    ), auto_unbox = TRUE)
  })
  app$get("/api/v1/jobs/:id/exports/ome-tiff", function(req, res) {
    res$send_json(list(
      status = "ready", format = "ome-tiff", sizeBytes = 4L,
      downloadUrl = download_url,
      downloadUrlExpiresAt = "2026-07-30T11:00:00Z",
      updatedAt = "2026-07-30T10:05:00Z"
    ), auto_unbox = TRUE)
  })
  server <- start_strand_server(app)
  client <- testing_client(server)
  job <- structure(list(id = "job-1", client = client), class = "strand_job")
  target <- file.path(tempdir(), "strand-test", "result.ome.tiff")
  withr::defer(unlink(dirname(target), recursive = TRUE), envir = parent.frame())

  written <- strand_download_ome_tiff(job, target, timeout = 1,
                                      poll_interval = 0)

  expect_equal(readBin(written, "raw", n = 4L), charToRaw("TIFF"))
})
