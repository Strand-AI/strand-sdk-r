test_that("segmentation lifecycle has start/status/manifest parity", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  state <- list(status = "queued", retryable = FALSE, creditCost = 0L,
                job = NULL, layer = NULL)
  app$get("/api/v1/samples/:id/segmentation", function(req, res) {
    res$send_json(state, auto_unbox = TRUE)
  })
  app$post("/api/v1/samples/:id/segmentation", function(req, res) {
    res$set_status(202L)$send_json(state, auto_unbox = TRUE)
  })
  app$get("/api/v1/samples/:id/segmentation/:layer/manifest", function(req, res) {
    res$send_json(list(schemaVersion = "1.0", layerId = req$params$layer,
                       artifacts = list(cells = list(joinKey = "instance_id"))),
                  auto_unbox = TRUE)
  })
  server <- start_strand_server(app)
  client <- testing_client(server)

  expect_equal(strand_segmentation_get(client, "sample-1")$creditCost, 0L)
  expect_equal(strand_segmentation_start(client, "sample-1")$status, "queued")
  expect_equal(
    strand_segmentation_manifest(client, "sample-1", "layer-1")$artifacts$cells$joinKey,
    "instance_id"
  )
})
