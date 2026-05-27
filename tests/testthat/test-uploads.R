# Tests for strand_uploads_list() / strand_uploads_get().

test_that("strand_uploads_list parses uploads and next_cursor", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$get("/api/v1/uploads", function(req, res) {
    res$send_json(list(
      uploads = list(
        list(
          id = "11111111-1111-1111-1111-111111111111",
          filename = "slide-a.svs",
          fileSize = "12345678",
          status = "ready",
          gcsPath = "uploads/org/aaaa/slide-a.svs",
          createdAt = "2026-05-20T10:00:00Z",
          widthPx = 10000L,
          heightPx = 8000L
        ),
        list(
          id = "22222222-2222-2222-2222-222222222222",
          filename = "slide-b.svs",
          fileSize = "9999",
          status = "uploading",
          gcsPath = "uploads/org/aaaa/slide-b.svs",
          createdAt = "2026-05-19T10:00:00Z",
          widthPx = NULL,
          heightPx = NULL
        )
      ),
      nextCursor = "Y3Vyc29yLW9wYXF1ZQ"
    ), auto_unbox = TRUE)
  })
  server <- start_strand_server(app)
  client <- testing_client(server)

  page <- strand_uploads_list(client, limit = 50L)
  expect_s3_class(page, "strand_upload_list")
  expect_length(page$uploads, 2L)
  expect_equal(page$next_cursor, "Y3Vyc29yLW9wYXF1ZQ")

  first <- page$uploads[[1L]]
  expect_s3_class(first, "strand_upload")
  expect_equal(first$id, "11111111-1111-1111-1111-111111111111")
  expect_equal(first$filename, "slide-a.svs")
  expect_equal(first$file_size, 12345678)
  expect_equal(first$status, "ready")
  expect_equal(first$width_px, 10000L)
  expect_equal(first$height_px, 8000L)

  second <- page$uploads[[2L]]
  expect_equal(second$status, "uploading")
  expect_null(second$width_px)
})


test_that("strand_uploads_list forwards limit and cursor as query params", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  # The handler runs in a separate worker process, so we can't read locals
  # via `<<-`. Echo the parsed query back in the response body instead.
  app$get("/api/v1/uploads", function(req, res) {
    res$send_json(list(
      uploads = list(),
      nextCursor = NULL,
      echoLimit = req$query$limit,
      echoCursor = req$query$cursor
    ), auto_unbox = TRUE)
  })
  server <- start_strand_server(app)
  client <- testing_client(server)

  raw <- strand_perform_json(client, "uploads",
                             query = list(limit = 25L, cursor = "abc-cursor"))
  expect_equal(raw$echoLimit, "25")
  expect_equal(raw$echoCursor, "abc-cursor")
})


test_that("strand_uploads_list rejects non-positive limit", {
  client <- strand_client(api_key = "x", base_url = "http://127.0.0.1:1")
  expect_error(strand_uploads_list(client, limit = 0L), "positive integer")
  expect_error(strand_uploads_list(client, limit = -1L), "positive integer")
})


test_that("strand_uploads_get returns a strand_upload row", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$get("/api/v1/uploads/:id", function(req, res) {
    res$send_json(list(
      id = req$params$id,
      filename = "slide.svs",
      fileSize = "256",
      status = "ready",
      gcsPath = "uploads/org/aa/slide.svs",
      createdAt = "2026-05-20T10:00:00Z",
      widthPx = 100L,
      heightPx = 200L
    ), auto_unbox = TRUE)
  })
  server <- start_strand_server(app)
  client <- testing_client(server)

  u <- strand_uploads_get(client, "11111111-1111-1111-1111-111111111111")
  expect_s3_class(u, "strand_upload")
  expect_equal(u$id, "11111111-1111-1111-1111-111111111111")
  expect_equal(u$file_size, 256)
  expect_equal(u$status, "ready")
})


test_that("strand_uploads_get maps 404 to strand_not_found_error", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$get("/api/v1/uploads/:id", function(req, res) {
    res$set_status(404L)$send_json(list(
      error = "not_found",
      message = "Upload not found"
    ), auto_unbox = TRUE)
  })
  server <- start_strand_server(app)
  client <- testing_client(server)

  expect_error(
    strand_uploads_get(client, "11111111-1111-1111-1111-111111111111"),
    class = "strand_not_found_error"
  )
})


test_that("strand_uploads_get rejects empty upload_id client-side", {
  client <- strand_client(api_key = "x", base_url = "http://127.0.0.1:1")
  expect_error(strand_uploads_get(client, ""), "non-empty string")
  expect_error(strand_uploads_get(client, c("a", "b")), "non-empty string")
})
