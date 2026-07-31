test_that("strand_set_mpp sends a scalar for isotropic pixels", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$patch("/api/v1/samples/:id/mpp", function(req, res) {
    value <- if (identical(req$json$mpp, 0.26)) 0.26 else 99
    res$send_json(
      list(id = req$params$id, userMpp = list(x = value, y = value)),
      auto_unbox = TRUE
    )
  })
  server <- start_strand_server(app)

  result <- strand_set_mpp(testing_client(server), "sample-1", 0.26)

  expect_identical(result$id, "sample-1")
  expect_equal(result$user_mpp, c(x = 0.26, y = 0.26))
})

test_that("strand_set_mpp sends explicit x and y axes", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$patch("/api/v1/samples/:id/mpp", function(req, res) {
    valid <- identical(req$json$mpp$x, 0.26) && identical(req$json$mpp$y, 0.25)
    x <- if (valid) 0.26 else 99
    y <- if (valid) 0.25 else 99
    res$send_json(
      list(id = req$params$id, userMpp = list(x = x, y = y)),
      auto_unbox = TRUE
    )
  })
  server <- start_strand_server(app)

  result <- strand_set_mpp(testing_client(server), "sample-1", 0.26, 0.25)

  expect_equal(result$user_mpp, c(x = 0.26, y = 0.25))
})

test_that("strand_set_mpp validates physical pixel sizes", {
  client <- strand_client(api_key = "sk-strand-test", base_url = "http://127.0.0.1:1")
  for (value in list(0, -0.1, 100.1, Inf, NaN, NA_real_, "0.5", c(0.5, 0.5))) {
    expect_error(
      strand_set_mpp(client, "sample-1", value),
      "greater than 0 and at most 100"
    )
  }
  expect_error(
    strand_set_mpp(client, "sample-1", 0.5, 0),
    "mpp_y must be a number greater than 0 and at most 100"
  )
})
