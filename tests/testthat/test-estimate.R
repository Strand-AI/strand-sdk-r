test_that("strand_estimate parses the response and surfaces snake_case fields", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$post("/api/v1/predict/estimate", function(req, res) {
    res$send_json(list(
      patchCount = 42L,
      markerCount = 3L,
      estimatedCredits = 126L,
      orgBalance = 1000L,
      orgPending = 0L
    ), auto_unbox = TRUE)
  })
  server <- start_strand_server(app)
  client <- testing_client(server)

  est <- strand_estimate(client, "u-1", c("CD3", "CD8", "Ki67"))
  expect_equal(est$patch_count, 42L)
  expect_equal(est$estimated_credits, 126L)
  expect_equal(est$org_balance, 1000L)
})

test_that("400 maps to strand_bad_request_error", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$post("/api/v1/predict/estimate", function(req, res) {
    res$set_status(400L)$send_json(list(
      error = "bad_request",
      message = "Invalid input"
    ), auto_unbox = TRUE)
  })
  server <- start_strand_server(app)
  client <- testing_client(server)

  expect_error(
    strand_estimate(client, "u-1", c("CD3")),
    class = "strand_bad_request_error"
  )
})

test_that("empty markers vector is rejected client-side", {
  client <- strand_client(api_key = "x", base_url = "http://localhost:9999")
  expect_error(strand_estimate(client, "u", character(0)), "at least one")
  expect_error(strand_estimate(client, "u", c("", "  ")), "at least one")
})
