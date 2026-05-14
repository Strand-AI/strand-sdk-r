test_that("strand_results_to_array reads a marker array via the proxy", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()

  root_meta <- list(
    zarr_format = 3L,
    node_type = "group",
    attributes = list(
      ome = list(version = "0.5"),
      multiscales = list(
        list(name = "H&E", datasets = list(list(path = "he/0"))),
        list(name = "CD3", datasets = list(list(path = "markers/CD3/0")))
      )
    )
  )
  array_meta <- list(
    zarr_format = 3L,
    node_type = "array",
    shape = list(1L, 2L, 2L),
    data_type = "float32",
    chunk_grid = list(name = "regular",
                      configuration = list(chunk_shape = list(1L, 2L, 2L))),
    chunk_key_encoding = list(name = "default",
                              configuration = list(separator = "/")),
    codecs = list(list(name = "bytes",
                        configuration = list(endian = "little"))),
    fill_value = 0L
  )
  chunk_bytes <- writeBin(c(1, 2, 3, 4), raw(), size = 4, endian = "little")

  app$get("/api/v1/jobs/job-1/results", function(req, res) {
    res$send_json(list(
      resultUrl = "https://example/zarr.json",
      resultBasePath = "predictions/org/job-1",
      expiresAt = "2026-05-14T11:05:00Z"
    ), auto_unbox = TRUE)
  })
  app$get("/api/v1/jobs/job-1/results/files/zarr.json", function(req, res) {
    res$set_type("application/json")
    res$send(jsonlite::toJSON(root_meta, auto_unbox = TRUE))
  })
  app$get("/api/v1/jobs/job-1/results/files/markers/CD3/0/zarr.json", function(req, res) {
    res$set_type("application/json")
    res$send(jsonlite::toJSON(array_meta, auto_unbox = TRUE))
  })
  app$get("/api/v1/jobs/job-1/results/files/markers/CD3/0/c/0/0/0", function(req, res) {
    res$set_type("application/octet-stream")
    res$send(chunk_bytes)
  })

  server <- start_strand_server(app)
  client <- testing_client(server)
  job <- structure(list(id = "job-1", reserved_credits = 100L, client = client),
                   class = "strand_job")

  res <- strand_results_to_array(job, name = "CD3")
  expect_equal(dim(res$array), c(1L, 2L, 2L))
  expect_equal(res$array[1, 1, 1], 1)
  expect_equal(res$array[1, 2, 2], 4)
})
