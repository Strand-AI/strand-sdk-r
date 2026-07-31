# strandai

R client for the [Strand Platform](https://strandai.com) — H&E → multiplex protein inference. Functions all use the `strand_*` prefix; the package is named `strandai` to avoid a name clash with the unrelated [`strand`](https://cran.r-project.org/package=strand) package on CRAN.

**Agent-friendly docs:** The full API reference is published as Markdown at [https://app.strandai.com/docs/api.md](https://app.strandai.com/docs/api.md), and the LLM index lives at [https://app.strandai.com/llms.txt](https://app.strandai.com/llms.txt).

## Install

```r
# From r-universe (recommended):
install.packages("strandai", repos = c("https://strand-ai.r-universe.dev", "https://cloud.r-project.org"))

# Fallback — install directly from GitHub:
remotes::install_github("Strand-AI/strand-sdk-r")
```

To return results as `SpatialExperiment` (recommended), also install:

```r
BiocManager::install("SpatialExperiment")
```

## Quickstart

One blocking call runs the full pipeline — upload, submit, wait, download:

```r
library(strandai)
client <- strand_client()  # reads STRAND_API_KEY

result <- strand_run(
  client, "biopsy.ome.tiff",
  markers = c("HER2", "CD8", "PD1"),
  output_dir = "./outputs/"
)
cat("Used", result$credits_used, "credits;",
    length(result$marker_outputs), "markers written\n")
```

`strand_run()` returns a `strand_predict_result` list with `job_id`, `status`,
`credits_used`, `marker_outputs` (one path per marker under `output_dir`),
`output_dir`, and `job` (the underlying `strand_job` handle). A
`predict.strand_client` S3 method also dispatches:

```r
result <- predict(client, "biopsy.ome.tiff", markers = c("HER2", "CD8"))
```

Pass `on_progress = function(stage, fraction) ...` to follow the four stages
(`"upload"`, `"submit"`, `"wait"`, `"download"`).

### Fire-and-forget: `wait = FALSE`

A full pipeline run blocks for 15+ minutes. To kick a job off and continue
with other work, pass `wait = FALSE` — `strand_run()` returns the
`strand_job` handle as soon as upload + submit complete:

```r
job <- strand_run(client, "slide.svs", c("CD3", "CD8"), wait = FALSE)
message("submitted ", job$id)

# ...do other work, or shut down the process — the job runs server-side.

# Later (same process or a fresh one via `strand_job_get(client, job$id)`):
status <- strand_job_wait(job)
spe <- strand_download_results(job)
# or render a single pyramidal OME-TIFF for QuPath or Fiji:
strand_download_ome_tiff(job, "result.ome.tiff", progress = TRUE)
```

Cancel an in-flight job to refund its reserved credits and release the
organization's concurrent-job slot:

```r
status <- strand_job_cancel(job)
```

When `wait = FALSE`, the `"wait"` and `"download"` progress stages don't fire,
and `timeout_sec` / `poll_interval_sec` / `output_dir` are ignored.

### Choosing a model

`strand_predict()` and `strand_run()` accept an optional `model =`:

- `"v0.7"` — current dispatchable version and default.

Omit `model` (or pass `NULL`) to let the platform pick `"v0.7"`. The older
`"v0.4"` and `"v0.5"` labels can appear on historical jobs but are sunset for
new submissions.

```r
result <- strand_run(
  client, "slide.svs",
  markers = c("CD8", "Ki67", "PanCK"),
  model = "v0.7"
)
result$model  # → "v0.7"
```

`strand_predict_result$model` (and the `model` field on `strand_job_get()`
output) always carry the canonical v0.X label. Historical jobs may surface
as `"v0.1"` (the renumbered legacy 35-marker base — sunset; readable but not
dispatchable).

#### Migration from `v10-*` names

The earlier `"v10"`, `"v10-fullpanel"`, and `"v10-fullpanel-v2"` names were
dropped on 2026-06-03 — the server returns 400 `unknown_model` for all of
them. Pass the canonical v0.X id instead:

| Legacy id               | Replacement   |
| ----------------------- | ------------- |
| `"v10-fullpanel-v2"`    | `"v0.5"`      |
| `"v10-fullpanel"`       | `"v0.4"`      |
| `"v10"`                 | (sunset — no replacement; the underlying 35-marker base was retired) |

### Setting slide scale

If a slide's physical pixel size is missing or incorrect, set the base-level
microns per pixel before submitting a job. Omit `mpp_y` for isotropic pixels:

```r
strand_set_mpp(client, upload$id, 0.26)
strand_set_mpp(client, upload$id, 0.26, 0.25)
```

The user-reported value takes precedence over embedded slide metadata for
subsequent inference jobs.

### Lower-level primitives

The submit / wait / download steps stay exported for fine-grained control:

```r
upload <- strand_upload_file(client, "slide.svs", progress = TRUE)

est <- strand_estimate(client, upload$id, c("CD3", "CD8", "Ki67"))
message("Will cost ~", est$estimated_credits, " credits")

job <- strand_predict(client, upload$id, c("CD3", "CD8", "Ki67"))
status <- strand_job_wait(job, progress = TRUE)

spe <- strand_download_results(job)            # SpatialExperiment
```

## Configuration

| Source | Argument / variable | Default |
|---|---|---|
| Arg | `strand_client(api_key=...)` | reads env |
| Env | `STRAND_API_KEY` | required |
| Env | `STRAND_BASE_URL` | `https://app.strandai.com` |

## Error handling

Documented HTTP error codes are mapped to typed conditions:

| HTTP | Condition class |
|---|---|
| 400 | `strand_bad_request_error` |
| 401 | `strand_auth_error` |
| 402 | `strand_insufficient_credits_error` (carries `required`) |
| 404 | `strand_not_found_error` |
| 429 | `strand_rate_limit_error` (carries `retry_after`) |

```r
tryCatch(
  strand_predict(client, upload$id, markers),
  strand_insufficient_credits_error = function(e) {
    message("Need ", e$required, " credits — top up the org first.")
  },
  strand_rate_limit_error = function(e) {
    message("Wait ", e$retry_after, "s before retrying.")
  }
)
```

## Layout

```
R/
  client.R                  strand_client + S3 print method
  http.R                    internal httr2 wrapper + typed error mapping
  uploads.R                 strand_upload_file (resumable chunked PUT to GCS)
  predict.R                 strand_estimate, strand_predict (submit),
                            strand_run (full pipeline), predict.strand_client
  mpp.R                     strand_set_mpp (physical pixel-size override)
  jobs.R                    strand_job_get + strand_job_wait + strand_job_cancel
  results.R                 strand_download_results + SpatialExperiment
tests/testthat/             unit tests using webfakes (in-process http server)
vignettes/quickstart.Rmd
```

## Issues & contributing

File bug reports and feature requests at
[Strand-AI/strand-sdk-r/issues](https://github.com/Strand-AI/strand-sdk-r/issues).

We don't accept external pull requests on the SDK at this time. If you'd like
to contribute or have ideas you'd like to discuss, email
[support@strandai.com](mailto:support@strandai.com).

## License

Apache 2.0
