# strandai 0.8.0

## Added

* Added an `auto_segment` argument to `strand_upload_file()` and `strand_run()`
  (and thus `predict()`). Cell segmentation still runs on ingest by default;
  pass `auto_segment = FALSE` to skip it for that upload (the slide is still
  ingested and rendered), or `TRUE` to force it even when the org default is
  off. `NULL` (the default) defers to the org's default. The resolved value is
  surfaced as `auto_segment` on the `strand_upload` returned by
  `strand_uploads_get()` / `strand_uploads_list()`.

# strandai 0.7.0

## Added

* Added `strand_samples_get()`, the first sample-read function. It returns a
  sample's curated resource (identity, status, physical scale, tags, and the
  expiration fields), so you can check when a sample expires — via
  `will_expire`, `expires_in_days`, and `expires_at` — without the mutation
  `strand_set_expiration()` requires. Previously there was no way to read a
  sample's expiration over the API without changing it.

# strandai 0.6.0

## Added

* Added `strand_set_mpp()` for user-reported physical pixel-size overrides.
* Added `strand_job_cancel()` for cancelling in-flight jobs and retrieving the
  refreshed status.
* Added `strand_uploads_list()` and `strand_uploads_get()` for upload reads.
* Added `strand_ome_tiff_request()`, `strand_ome_tiff_get()`, and
  `strand_download_ome_tiff()` for asynchronous OME-TIFF export and download.

# strandai 0.5.2

## Changed

* Added `"v0.7"`, the current dispatchable Lattice version and default, to the
  documented model ids. `"v0.4"` and `"v0.5"` remain readable on historical
  jobs but return 400 `model_sunset` for new submissions.

# strandai 0.5.1

## Changed

* Dropped the legacy `"v10"` / `"v10-fullpanel"` / `"v10-fullpanel-v2"`
  alias-rewriting path. `strand_validate_model()` no longer warns and no
  longer rewrites legacy strings client-side; they're forwarded verbatim to
  the server, which now returns 400 `unknown_model`. Pass `model = "v0.4"`
  or `model = "v0.5"` directly. The original 0.5.0 release notes (below)
  called out a 2026-12-01 sunset window; we collapsed that to a hard
  cutover on 2026-06-03 after the in-the-wild traffic sample showed no
  callers still emitting the legacy strings. See
  `infra/notes/postman-versioning-2026-06.md` §4 (rewritten 2026-06-03)
  in the platform repo.
* The `model` field on `strand_predict_result` and `strand_job_get()` may
  now surface `"v0.1"` (the renumbered legacy 35-marker base; design note
  §8.2, locked 2026-06-03) instead of `"v0.3"` on historical jobs that
  ran on `wx0hp7fb`.

## Migration

No action required if you already migrated to canonical v0.X ids per
0.5.0 below. If you're still passing `"v10*"` strings, expect a 400
`unknown_model` response and update the call to the canonical id.

# strandai 0.5.0

## Changed

* `strand_predict()` and `strand_run()` now route to the canonical Lattice
  version track (`"v0.4"` and `"v0.5"`). The earlier `"v10"`,
  `"v10-fullpanel"`, and `"v10-fullpanel-v2"` ids are still accepted on input
  as deprecated aliases; the SDK rewrites `"v10-fullpanel"` → `"v0.4"` and
  `"v10-fullpanel-v2"` → `"v0.5"` before sending and emits a deprecation
  `warning()` on each call. `"v10"` warns and is forwarded unchanged; the
  server returns 400 `unknown_model` because v0.3 was sunset. Aliases will be
  removed on 2026-12-01. See `infra/notes/postman-versioning-2026-06.md` §4
  in the platform repo.
* `strand_job_get()` and the `strand_predict_result` list returned by
  `strand_run()` now include a `model` field carrying the canonical v0.X
  label the platform persisted. `NULL` against older servers that didn't
  populate it.
* `strand_predict(model = "<unknown>")` no longer rejects unknown strings
  client-side; the server is the authority on which versions are live.
  Empty / NA / multi-element `model` values are still rejected up-front.

## Migration

```r
# Before
job <- strand_predict(client, upload$id, c("CD8"), model = "v10-fullpanel-v2")
# After (no warning, future-proof through 2026-12-01)
job <- strand_predict(client, upload$id, c("CD8"), model = "v0.5")
```

Legacy strings keep working until 2026-12-01; the deprecation warning is the
only change visible to a caller that doesn't migrate.

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
