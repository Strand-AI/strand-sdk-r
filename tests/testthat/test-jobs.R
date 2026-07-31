test_that("strand_job_cancel cancels and returns the refreshed status", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$post("/api/v1/jobs/:id/cancel", function(req, res) {
    res$send_json(
      list(id = req$params$id, status = "cancelled"),
      auto_unbox = TRUE
    )
  })
  app$get("/api/v1/jobs/:id", function(req, res) {
    res$send_json(
      list(
        id = req$params$id,
        status = "cancelled",
        markers = list("CD3"),
        resultsAvailable = FALSE
      ),
      auto_unbox = TRUE
    )
  })
  server <- start_strand_server(app)
  client <- testing_client(server)
  job <- structure(
    list(id = "job-1", reserved_credits = 1L, client = client),
    class = "strand_job"
  )

  status <- strand_job_cancel(job)

  expect_identical(status$id, "job-1")
  expect_identical(status$status, "cancelled")
})

test_that("strand_job_cancel requires a job handle", {
  expect_error(strand_job_cancel("job-1"), "job must be a strand_job")
})

test_that("strand_job_wait treats cancelled as terminal", {
  skip_if_no_webfakes()
  app <- webfakes::new_app()
  app$get("/api/v1/jobs/:id", function(req, res) {
    res$send_json(
      list(
        id = req$params$id,
        status = "cancelled",
        markers = list(),
        resultsAvailable = FALSE
      ),
      auto_unbox = TRUE
    )
  })
  server <- start_strand_server(app)
  job <- structure(
    list(id = "job-1", reserved_credits = 1L, client = testing_client(server)),
    class = "strand_job"
  )

  status <- strand_job_wait(job, timeout = 1, poll_interval = 0)

  expect_identical(status$status, "cancelled")
})
