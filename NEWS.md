# strandai 0.4.0

## BREAKING

* `strand_set_retention()` removed — use `strand_set_expiration()`.
* `strand_set_retention_bulk()` removed — use `strand_set_expiration_bulk()`.
* The `pin = TRUE` argument is replaced by `never_expire = TRUE`.
* REST endpoint paths renamed: `/samples/{id}/retention` → `/samples/{id}/expiration`;
  `/samples/retention` → `/samples/expiration`.
* Request body field `pin` → `neverExpire`.

No deprecation shim. The old function names are not exported.

### Migration

```r
# Before
strand_set_retention(client, sample_id, pin = TRUE)
strand_set_retention_bulk(client, c(id1, id2), expires_at = date)
# After
strand_set_expiration(client, sample_id, never_expire = TRUE)
strand_set_expiration_bulk(client, c(id1, id2), expires_at = date)
```

# strandai (development version)

* `strand_predict()` and `strand_run()` now accept a `model` argument
  (`"v10"` or `"v10-fullpanel"`). When omitted (`NULL`, the default), the
  platform picks. Unsupported model ids are rejected client-side before any
  HTTP call.
* `strand_run()` now accepts `wait = FALSE` to return the `strand_job` handle
  as soon as upload + submit complete, skipping the `wait` / `download`
  stages. Default `wait = TRUE` preserves the prior blocking behavior.

# strandai 0.1.0

Initial public release of the R client for the Strand Platform API.

* `strand_client()` — construct a client; reads `STRAND_API_KEY` and
  `STRAND_BASE_URL` from the environment.
* `strand_upload_file()` — resumable chunked upload to GCS for whole slide
  images, with optional progress reporting.
* `strand_estimate()` and `strand_predict()` — typed wrappers around the
  `/api/v1` REST endpoints for cost estimation and job submission.
* `strand_run()` (and `predict.strand_client()` S3 method) — full pipeline in
  one blocking call (upload → submit → wait → download), returning a
  `strand_predict_result`.
* `strand_job_get()` / `strand_job_wait()` — poll or block on a job with SSE
  event streaming for live progress.
* `strand_download_results()` — download OME-Zarr predictions and return as a
  `SpatialExperiment` (when the optional Bioconductor dependency is installed).
* Typed conditions mirroring the platform API: `strand_bad_request_error`,
  `strand_auth_error`, `strand_insufficient_credits_error` (carries
  `required`), `strand_not_found_error`, `strand_rate_limit_error` (carries
  `retry_after`).
