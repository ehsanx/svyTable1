#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @return The result of calling the right-hand side with the left-hand side as
#'   its first argument: \code{lhs \%>\% rhs} is equivalent to \code{rhs(lhs)}.
#'   Used to chain a sequence of operations; the returned value and its class are
#'   determined by the right-hand side expression.
#' @usage lhs \%>\% rhs
#' @param lhs A value to be piped into the right-hand side.
#' @param rhs A function call using the magrittr semantics.
NULL

utils::globalVariables(c(
  "Estimate", "CI_Upper", "CI_Lower", "p.value", "RSE_percent", "Term",
  "is_significant", "CI_Width", "is_rse_high"
))
