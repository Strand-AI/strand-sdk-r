test_that("strand_markers_list parses the entitlement-scoped catalog", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$get("/api/v1/markers", function(req, res) {
    res$send_json(
      list(
        fullPanel = FALSE,
        count = 2L,
        markers = list(
          list(name = "DAPI", publicPanel = TRUE),
          list(name = "CD8", publicPanel = TRUE)
        )
      ),
      auto_unbox = TRUE
    )
  })
  server <- start_strand_server(app)

  catalog <- strand_markers_list(testing_client(server))

  expect_false(catalog$full_panel)
  expect_identical(catalog$markers, c("DAPI", "CD8"))
  expect_identical(catalog$public_markers, c("DAPI", "CD8"))
})

test_that("strand_markers_list surfaces a 403 as a typed marker_not_available error", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  # A predict call is the realistic 403 trigger; assert the http layer maps it.
  app$post("/api/v1/predict", function(req, res) {
    res$set_status(403L)$send_json(
      list(
        error = "marker_not_available",
        message = "Marker not available on this account: CD4.",
        unavailableMarkers = list("CD4"),
        availableMarkers = list("DAPI", "CD8")
      ),
      auto_unbox = TRUE
    )
  })
  server <- start_strand_server(app)
  client <- testing_client(server)

  err <- tryCatch(
    strand_predict(client, "00000000-0000-4000-8000-0000000000ee", markers = c("CD4")),
    strand_marker_not_available_error = function(e) e
  )
  expect_s3_class(err, "strand_marker_not_available_error")
  expect_identical(err$unavailable, "CD4")
  expect_identical(err$available, c("DAPI", "CD8"))
})
