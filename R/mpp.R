# Shared physical pixel-size validation for sample mutations.

strand_validate_mpp <- function(value, name) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || value <= 0 || value > 100) {
    stop(name, " must be a number greater than 0 and at most 100", call. = FALSE)
  }
  as.numeric(value)
}
