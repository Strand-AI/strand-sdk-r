# strandai

R client for [Lattice spatial proteomics from H&E](https://strandai.com/lattice).
Upload H&E whole-slide images, predict spatially resolved protein-marker maps,
and download the virtual mIF channels as a `SpatialExperiment` or
OME-Zarr/OME-TIFF. Functions use the `strand_*`
prefix; the package is named `strandai` to avoid a clash with the unrelated
[`strand`](https://cran.r-project.org/package=strand) package on CRAN.

📚 **Full documentation: <https://docs.strandai.com/sdks/r>**

Agent-readable API reference: <https://app.strandai.com/docs/api.md> · LLM
index: <https://app.strandai.com/llms.txt>

## Install

```r
# From r-universe (recommended):
install.packages("strandai", repos = c("https://strand-ai.r-universe.dev", "https://cloud.r-project.org"))

# For SpatialExperiment results (recommended):
BiocManager::install("SpatialExperiment")
```

## Quickstart

One blocking call runs the full pipeline — upload, submit, wait, download:

```r
library(strandai)
client <- strand_client()  # reads STRAND_API_KEY

result <- strand_run(
  client, "biopsy.ome.tiff",
  markers = c("PanCK", "CD4", "CD45"),
  output_dir = "./outputs/"
)
cat("Used", result$credits_used, "credits;",
    length(result$marker_outputs), "markers written\n")
```

Use the unified sample surface to browse owned and public slides, inspect an
owned slide's job history, and update its attributes:

```r
page <- strand_samples_list(client, scope = "all")
sample <- strand_samples_get(client, page$items[[1]]$id)

if (sample$ownership == "mine") {
  sample <- strand_patch_sample(
    client, sample$id,
    name = "Baseline biopsy",
    fields = list(tags = c("baseline", "responding")),
    mpp = 0.26
  )
}
```

Price a prediction through the same operation that submits it:

```r
estimate <- strand_predict(
  client, upload$id, c("CD4", "PanCK"), dry_run = TRUE
)
```

Uploads, model selection, async jobs, OME-TIFF export, and error handling are
covered in the [hosted docs](https://docs.strandai.com/sdks/r).

## Issues & support

File bug reports and feature requests at
[Strand-AI/strand-sdk-r/issues](https://github.com/Strand-AI/strand-sdk-r/issues),
or email [support@strandai.com](mailto:support@strandai.com). This repository
is a generated, read-only mirror of Strand AI's monorepo — pull requests opened
here are overwritten by the next sync.

## License

Apache 2.0
