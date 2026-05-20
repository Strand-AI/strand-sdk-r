# strandai

R client for the [Strand Platform](https://strandai.com) — H&E → multiplex protein inference. Functions all use the `strand_*` prefix; the package is named `strandai` to avoid a name clash with the unrelated [`strand`](https://cran.r-project.org/package=strand) package on CRAN.

**Agent-friendly docs:** The full API reference is published as Markdown at [https://app.strandai.com/docs/api.md](https://app.strandai.com/docs/api.md), and the LLM index lives at [https://app.strandai.com/llms.txt](https://app.strandai.com/llms.txt).

## Install

```r
# From r-universe (when published)
install.packages("strandai", repos = c("https://strand-ai.r-universe.dev", "https://cloud.r-project.org"))

# Or directly from this repository (development):
remotes::install_local("sdks/r/strand")
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
  jobs.R                    strand_job_get + strand_job_wait
  results.R                 strand_download_results + SpatialExperiment
tests/testthat/             unit tests using webfakes (in-process http server)
vignettes/quickstart.Rmd
```

## License

Apache 2.0
