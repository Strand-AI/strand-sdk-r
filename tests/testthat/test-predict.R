test_that("strand_predict returns a strand_job with reserved credits", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$post("/api/v1/predict", function(req, res) {
    res$set_status(202L)$send_json(list(
      jobId = "22222222-2222-2222-2222-222222222222",
      reservedCredits = 300L,
      status = "queued"
    ), auto_unbox = TRUE)
  })
  server <- start_strand_server(app)
  client <- testing_client(server)

  job <- strand_predict(client, "u-1", c("CD3"))
  expect_s3_class(job, "strand_job")
  expect_equal(job$id, "22222222-2222-2222-2222-222222222222")
  expect_equal(job$reserved_credits, 300L)
})

test_that("402 maps to strand_insufficient_credits_error with required", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$post("/api/v1/predict", function(req, res) {
    res$set_status(402L)$send_json(list(
      error = "insufficient_credits",
      message = "Need 1000 credits",
      required = 1000L
    ), auto_unbox = TRUE)
  })
  server <- start_strand_server(app)
  client <- testing_client(server)

  err <- tryCatch(
    strand_predict(client, "u-1", c("CD3")),
    strand_insufficient_credits_error = function(e) e
  )
  expect_s3_class(err, "strand_insufficient_credits_error")
  expect_equal(err$required, 1000L)
})

test_that("400 unknown_markers maps to strand_unknown_markers_error with fields", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$post("/api/v1/predict", function(req, res) {
    res$set_status(400L)$send_json(list(
      error = "unknown_markers",
      message = "Unknown markers: NOPE, ALSO_NOPE",
      unknownMarkers = list("NOPE", "ALSO_NOPE"),
      knownMarkersSample = list("CD3", "CD8", "HER2")
    ), auto_unbox = TRUE)
  })
  server <- start_strand_server(app)
  client <- testing_client(server)

  err <- tryCatch(
    strand_predict(client, "u-1", c("NOPE")),
    strand_unknown_markers_error = function(e) e
  )
  expect_s3_class(err, "strand_unknown_markers_error")
  expect_s3_class(err, "strand_bad_request_error")  # inherits from 400
  expect_equal(err$unknown, c("NOPE", "ALSO_NOPE"))
  expect_equal(err$known_subset, c("CD3", "CD8", "HER2"))
  expect_equal(err$error_code, "unknown_markers")
})

test_that("429 maps to strand_rate_limit_error with retry_after", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$post("/api/v1/predict", function(req, res) {
    res$set_header("Retry-After", "30")
    res$set_status(429L)$send_json(list(
      error = "rate_limited",
      message = "Concurrent cap exceeded"
    ), auto_unbox = TRUE)
  })
  server <- start_strand_server(app)
  client <- testing_client(server)

  err <- tryCatch(
    strand_predict(client, "u-1", c("CD3")),
    strand_rate_limit_error = function(e) e
  )
  expect_s3_class(err, "strand_rate_limit_error")
  expect_equal(err$retry_after, 30L)
})
