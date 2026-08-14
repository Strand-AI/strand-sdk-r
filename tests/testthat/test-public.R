test_that("strand_public_samples_list parses a page and sends pagination params", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$get("/api/v1/public/samples", function(req, res) {
    res$send_json(
      list(
        items = list(
          list(
            publicId = "pub-1",
            title = "TCGA slide",
            thumbnailUrl = "/api/v1/public/samples/pub-1/thumbnail",
            tags = list("tcga-coad"),
            metadata = list(stage = "II")
          )
        ),
        page = as.integer(req$query$page %||% 1L),
        pageSize = as.integer(req$query$pageSize %||% 48L),
        totalCount = 1L,
        totalPages = 1L
      ),
      auto_unbox = TRUE
    )
  })
  server <- start_strand_server(app)

  page <- strand_public_samples_list(
    testing_client(server),
    page = 2, page_size = 10, tag = "tcga-coad"
  )

  expect_identical(page$page, 2L)
  expect_identical(page$page_size, 10L)
  expect_identical(page$total_count, 1L)
  expect_length(page$items, 1L)
  expect_identical(page$items[[1]]$public_id, "pub-1")
  expect_identical(page$items[[1]]$tags, "tcga-coad")
})

test_that("strand_public_sample_get parses detail with markers and geometry", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$get("/api/v1/public/samples/:id", function(req, res) {
    res$send_json(
      list(
        publicId = req$params$id,
        title = "TCGA slide",
        thumbnailUrl = "/api/v1/public/samples/pub-1/thumbnail",
        tags = list("tcga-coad"),
        metadata = list(stage = "II"),
        geometry = list(widthPx = 20000L, heightPx = 15000L, mppX = 0.5, mppY = 0.5),
        viewer = list(
          pyramidUrl = "/api/v1/public/samples/pub-1/zarr",
          markers = list(list(name = "CD3"), list(name = "CD8"))
        )
      ),
      auto_unbox = TRUE
    )
  })
  server <- start_strand_server(app)

  s <- strand_public_sample_get(testing_client(server), "pub-1")

  expect_identical(s$public_id, "pub-1")
  expect_identical(s$markers, c("CD3", "CD8"))
  expect_identical(s$geometry$width_px, 20000L)
  expect_equal(s$geometry$mpp_x, 0.5)
  expect_identical(s$pyramid_url, "/api/v1/public/samples/pub-1/zarr")
  expect_identical(s$thumbnail_url, "/api/v1/public/samples/pub-1/thumbnail")
})

test_that("strand_public_sample_get raises not-found for a non-public sample", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$get("/api/v1/public/samples/:id", function(req, res) {
    res$set_status(404)$send_json(
      list(error = "not_found", message = "Public sample not found"),
      auto_unbox = TRUE
    )
  })
  server <- start_strand_server(app)

  expect_error(
    strand_public_sample_get(testing_client(server), "pub-x"),
    class = "strand_not_found_error"
  )
})

test_that("public cohort readers validate the client argument", {
  expect_error(
    strand_public_samples_list(list()),
    "client must be a strand_client"
  )
  expect_error(
    strand_public_sample_get(list(), "pub-1"),
    "client must be a strand_client"
  )
})
