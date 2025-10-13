#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
NULL

utils::globalVariables(c(
  "Estimate", "CI_Upper", "CI_Lower", "p.value", "RSE_percent", "Term",
  "is_significant", "CI_Width", "is_rse_high"
))
