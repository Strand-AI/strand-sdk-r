# strandai

R client for the [Strand AI Platform](https://strandai.com): upload H&E
whole-slide images, run virtual multiplex-immunofluorescence inference
(H&E → spatial proteomics), and download per-marker predictions as a
`SpatialExperiment` or OME-Zarr/OME-TIFF. Functions use the `strand_*`
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
  markers = c("HER2", "CD8", "PD1"),
  output_dir = "./outputs/"
)
cat("Used", result$credits_used, "credits;",
    length(result$marker_outputs), "markers written\n")
```

Everything else — uploads, model selection, async jobs, OME-TIFF export,
error handling — is covered in the
[hosted docs](https://docs.strandai.com/sdks/r).

## Issues & support

File bug reports and feature requests at
[Strand-AI/strand-sdk-r/issues](https://github.com/Strand-AI/strand-sdk-r/issues),
or email [support@strandai.com](mailto:support@strandai.com). This repository
is a generated, read-only mirror of Strand AI's monorepo — pull requests opened
here are overwritten by the next sync.

## License

Apache 2.0
