test_that("strand_samples_get parses the full sample resource", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$get("/api/v1/samples/:id", function(req, res) {
    res$send_json(
      list(
        id = req$params$id,
        name = "Slide A",
        filename = "slide-a.svs",
        status = "ready",
        fileSize = "1048576",
        widthPx = 20000L,
        heightPx = 15000L,
        mpp = 0.5,
        tags = list("cohort-a", "histowiz"),
        createdAt = "2026-01-15T12:00:00Z",
        expiresAt = "2026-12-31T00:00:00Z",
        expiresAtSource = "custom",
        expiresInDays = 120L,
        willExpire = TRUE,
        trashedAt = NULL
      ),
      auto_unbox = TRUE
    )
  })
  server <- start_strand_server(app)

  s <- strand_samples_get(testing_client(server), "sample-1")

  expect_identical(s$id, "sample-1")
  expect_identical(s$name, "Slide A")
  expect_identical(s$status, "ready")
  expect_equal(s$file_size, 1048576)
  expect_identical(s$width_px, 20000L)
  expect_equal(s$mpp, 0.5)
  expect_identical(s$tags, c("cohort-a", "histowiz"))
  expect_identical(s$expires_at, "2026-12-31T00:00:00Z")
  expect_identical(s$expires_at_source, "custom")
  expect_identical(s$expires_in_days, 120L)
  expect_true(s$will_expire)
  expect_null(s$trashed_at)
})

test_that("strand_samples_get handles never-expire, no scale, no tags", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$get("/api/v1/samples/:id", function(req, res) {
    res$send_json(
      list(
        id = req$params$id,
        name = NULL,
        filename = "slide-b.svs",
        status = "uploading",
        fileSize = "2048",
        widthPx = NULL,
        heightPx = NULL,
        mpp = NULL,
        tags = list(),
        createdAt = "2026-01-16T12:00:00Z",
        expiresAt = NULL,
        expiresAtSource = NULL,
        expiresInDays = NULL,
        willExpire = FALSE,
        trashedAt = NULL
      ),
      auto_unbox = TRUE
    )
  })
  server <- start_strand_server(app)

  s <- strand_samples_get(testing_client(server), "sample-1")

  expect_null(s$name)
  expect_null(s$mpp)
  expect_null(s$width_px)
  expect_identical(s$tags, character(0))
  expect_false(s$will_expire)
  expect_null(s$expires_in_days)
})

test_that("strand_samples_get raises a not-found error outside the org", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
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

test_that("strand_samples_get validates the client argument", {
  expect_error(
    strand_samples_get(list(), "sample-1"),
    "client must be a strand_client"
  )
})
