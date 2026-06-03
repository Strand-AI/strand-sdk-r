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

test_that("strand_predict forwards canonical v0.X model ids unchanged", {
  skip_if_no_webfakes()
  # webfakes runs the handler in a separate process, so we can't share state
  # with the test process directly. We use `reservedCredits` as a sentinel:
  # the handler maps the received body shape to a distinct integer that the
  # test can assert on.
  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$post("/api/v1/predict", function(req, res) {
    sent_model <- req$json$model
    sentinel <- if (is.null(sent_model)) 1L
                else if (identical(sent_model, "v0.4")) 4L
                else if (identical(sent_model, "v0.5")) 5L
                else 99L
    res$set_status(202L)$send_json(list(
      jobId = "22222222-2222-2222-2222-222222222222",
      reservedCredits = sentinel,
      status = "queued"
    ), auto_unbox = TRUE)
  })
  server <- start_strand_server(app)
  client <- testing_client(server)

  # No `model` arg → body must NOT carry a `model` key → sentinel 1.
  job <- strand_predict(client, "u-1", c("CD3"))
  expect_equal(job$reserved_credits, 1L)

  # Canonical v0.4 / v0.5 → forwarded verbatim → sentinel 4/5.
  job <- strand_predict(client, "u-1", c("CD3"), model = "v0.4")
  expect_equal(job$reserved_credits, 4L)
  job <- strand_predict(client, "u-1", c("CD3"), model = "v0.5")
  expect_equal(job$reserved_credits, 5L)
})

test_that("strand_predict accepts legacy v10-* aliases with a deprecation warning", {
  skip_if_no_webfakes()
  # The SDK rewrites `"v10-fullpanel"` → `"v0.4"` and
  # `"v10-fullpanel-v2"` → `"v0.5"` before sending, so the server only
  # ever sees the canonical id. This keeps older callers on the air
  # without requiring the platform to accept the legacy enum.
  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$post("/api/v1/predict", function(req, res) {
    sent_model <- req$json$model
    sentinel <- if (identical(sent_model, "v0.4")) 4L
                else if (identical(sent_model, "v0.5")) 5L
                else 99L
    res$set_status(202L)$send_json(list(
      jobId = "22222222-2222-2222-2222-222222222222",
      reservedCredits = sentinel,
      status = "queued"
    ), auto_unbox = TRUE)
  })
  server <- start_strand_server(app)
  client <- testing_client(server)

  # "v10-fullpanel" → warns, rewritten to "v0.4".
  expect_warning(
    job <- strand_predict(client, "u-1", c("CD3"), model = "v10-fullpanel"),
    "deprecated alias.*v0\\.4"
  )
  expect_equal(job$reserved_credits, 4L)

  # "v10-fullpanel-v2" → warns, rewritten to "v0.5".
  expect_warning(
    job <- strand_predict(client, "u-1", c("CD3"), model = "v10-fullpanel-v2"),
    "deprecated alias.*v0\\.5"
  )
  expect_equal(job$reserved_credits, 5L)
})

test_that("strand_predict warns on sunset v10 alias and forwards to server", {
  skip_if_no_webfakes()
  # The "v10" alias used to resolve to v0.3, which is sunset. The SDK
  # emits a deprecation warning so the caller sees the rename, but
  # forwards the string unchanged so the server can answer with its
  # canonical `unknown_model` 400 (rather than the SDK rewriting to
  # `"v0.3"` and getting a different error).
  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  received <- list()
  app$post("/api/v1/predict", function(req, res) {
    res$set_status(400L)$send_json(list(
      error = "unknown_model",
      message = "Unknown model: v10"
    ), auto_unbox = TRUE)
  })
  server <- start_strand_server(app)
  client <- testing_client(server)

  err <- tryCatch(
    {
      expect_warning(
        strand_predict(client, "u-1", c("CD3"), model = "v10"),
        "sunset POSTMAN version"
      )
    },
    strand_bad_request_error = function(e) e
  )
  expect_s3_class(err, "strand_bad_request_error")
})

test_that("strand_predict passes unknown model strings through to the server", {
  skip_if_no_webfakes()
  # An unknown string is forwarded verbatim — no SDK-side warning, no
  # client-side validation. Keeps the SDK forward-compatible with new
  # POSTMAN versions added on the server without an R SDK release.
  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$post("/api/v1/predict", function(req, res) {
    res$set_status(400L)$send_json(list(
      error = "unknown_model",
      message = "Unknown model: v0.99"
    ), auto_unbox = TRUE)
  })
  server <- start_strand_server(app)
  client <- testing_client(server)

  err <- tryCatch(
    strand_predict(client, "u-1", c("CD3"), model = "v0.99"),
    strand_bad_request_error = function(e) e
  )
  expect_s3_class(err, "strand_bad_request_error")
})

test_that("strand_predict rejects only structurally-invalid model args", {
  client <- strand_client(api_key = "sk-strand-test",
                          base_url = "http://127.0.0.1:1")
  # Empty / multi-element / NA are structural errors caught client-side
  # before any HTTP call. Unknown *strings* are no longer rejected
  # client-side — they're sent to the server for the canonical
  # `unknown_model` response.
  expect_error(strand_predict(client, "u-1", c("CD3"), model = ""),
               "non-empty")
  expect_error(strand_predict(client, "u-1", c("CD3"), model = c("v0.4", "v0.5")),
               "single non-empty")
  expect_error(strand_predict(client, "u-1", c("CD3"), model = NA_character_),
               "non-empty")
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
