# Public cell-segmentation lifecycle.

#' Get cell-segmentation state
#'
#' @param client A `strand_client`.
#' @param sample_id An owned sample UUID. Public sample handles are read-only.
#' @return Lifecycle state with latest job/layer metadata.
#' @export
strand_segmentation_get <- function(client, sample_id) {
  strand_perform_json(client, sprintf("samples/%s/segmentation", sample_id))
}

#' Start or retry cell segmentation
#'
#' Free and idempotent: completed or active work is reused, failed work can be
#' retried after the documented cooldown, and no credits are reserved.
#'
#' @inheritParams strand_segmentation_get
#' @return Lifecycle state and polling URL.
#' @export
strand_segmentation_start <- function(client, sample_id) {
  strand_perform_json(
    client,
    sprintf("samples/%s/segmentation", sample_id),
    method = "POST",
    expected = c(200L, 202L)
  )
}

#' Get segmentation artifact manifest
#'
#' @inheritParams strand_segmentation_get
#' @param layer_id Segmentation layer UUID from the lifecycle response.
#' @return Explicit mask, geometry/morphology, marker-expression, and provenance schema.
#' @export
strand_segmentation_manifest <- function(client, sample_id, layer_id) {
  strand_perform_json(
    client,
    sprintf("samples/%s/segmentation/%s/manifest", sample_id, layer_id)
  )
}
