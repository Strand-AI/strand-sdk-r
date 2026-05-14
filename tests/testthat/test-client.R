test_that("strand_client reads STRAND_API_KEY from env", {
  withr::with_envvar(c(STRAND_API_KEY = "sk-strand-test-123"), {
    c <- strand_client()
    expect_s3_class(c, "strand_client")
    expect_equal(c$api_key, "sk-strand-test-123")
    expect_match(c$api_root, "/api/v1$")
  })
})

test_that("strand_client errors without an API key", {
  withr::with_envvar(c(STRAND_API_KEY = NA_character_), {
    expect_error(strand_client(), "No API key")
  })
})

test_that("base_url trailing slashes are normalized", {
  c <- strand_client(api_key = "x", base_url = "https://example.com/")
  expect_equal(c$base_url, "https://example.com")
  expect_equal(c$api_root, "https://example.com/api/v1")
})

test_that("printing redacts the API key", {
  c <- strand_client(api_key = "sk-strand-abcdefghijklmnopqrstuvwx",
                     base_url = "https://example.com")
  expect_output(print(c), "sk-strand-ab")
  expect_output(print(c), "uvwx")
})
