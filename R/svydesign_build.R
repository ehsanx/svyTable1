#' Build a Complex-Survey Design with Validation and Safe Subpopulations
#'
#' @description
#' A student-friendly wrapper around \code{survey::svydesign()} that takes plain
#' column names (rather than formulas), validates the inputs with clear error
#' messages, and -- crucially -- supports subpopulation ("domain") analysis the
#' \emph{correct} way, by subsetting the design instead of pre-filtering the rows.
#'
#' @details
#' The most common and most consequential mistake in complex-survey analysis is to
#' \code{filter()} the data to a subgroup \emph{before} building the design. That
#' discards the design information needed to compute correct standard errors for
#' the subgroup. The right approach is to build the design on the \strong{full}
#' sample and then \code{subset()} it. Passing a `subpop` condition here does
#' exactly that.
#'
#' The function also flags strata that contain a single primary sampling unit
#' (PSU). Such "lonely PSUs" make variance estimation fail unless you set
#' \code{options(survey.lonely.psu = "adjust")} (or "average"); the function only
#' \emph{advises} this -- it does not change global options on your behalf.
#'
#' @param data A \code{data.frame} containing the survey variables.
#' @param ids A string naming the cluster / primary sampling unit (PSU) variable
#'   (e.g. \code{"psu"}). Use \code{"0"} or \code{NA} for no clustering.
#' @param weights A string naming the sampling-weight variable (e.g.
#'   \code{"survey_weight"}).
#' @param strata A string naming the stratification variable, or \code{NULL}
#'   (the default) for no strata.
#' @param nest Logical; passed to \code{survey::svydesign()}. If \code{TRUE}
#'   (default), PSU ids are treated as nested within strata.
#' @param subpop Optional character string giving a logical condition that
#'   defines a subpopulation (e.g. \code{"age >= 20 & sex == 'Female'"}). When
#'   supplied, the design is built on the full sample and then restricted with
#'   \code{survey::subset()}, preserving correct standard errors.
#' @param verbose Logical; if \code{TRUE} (default), prints the analytic sample
#'   size and any lonely-PSU advisory as messages.
#'
#' @return A survey design object of class \code{survey.design2} (the same class
#'   returned by \code{survey::svydesign()}), restricted to the subpopulation
#'   when `subpop` is supplied. Pass it to \code{svytable1()}, \code{svyglm()},
#'   and the other functions in this package.
#'
#' @importFrom survey svydesign
#' @importFrom stats as.formula
#'
#' @export
#'
#' @examples
#' data(nhanes_mortality, package = "svyTable1")
#'
#' # Build the design from plain column names.
#' design <- svydesign_build(
#'   data = nhanes_mortality,
#'   ids = "psu",
#'   strata = "strata",
#'   weights = "survey_weight"
#' )
#'
#' # Correct subpopulation analysis: build on the full sample, then subset.
#' design_women <- svydesign_build(
#'   data = nhanes_mortality,
#'   ids = "psu",
#'   strata = "strata",
#'   weights = "survey_weight",
#'   subpop = "sex == 'Female'"
#' )
svydesign_build <- function(data, ids, weights, strata = NULL,
                            nest = TRUE, subpop = NULL, verbose = TRUE) {

  # --- 1. Input validation ---
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (missing(ids) || !is.character(ids) || length(ids) != 1) {
    stop("`ids` must be a single column name (a string), e.g. \"psu\".",
         call. = FALSE)
  }
  if (missing(weights) || !is.character(weights) || length(weights) != 1) {
    stop("`weights` must be a single column name (a string), e.g. ",
         "\"survey_weight\".", call. = FALSE)
  }

  needed <- c(ids, weights)
  if (!is.null(strata)) {
    if (!is.character(strata) || length(strata) != 1) {
      stop("`strata` must be a single column name (a string) or NULL.",
           call. = FALSE)
    }
    needed <- c(needed, strata)
  }
  # ids = "0" is the survey convention for "no clusters"; do not require a column.
  needed <- setdiff(needed, "0")
  missing_cols <- setdiff(needed, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in `data`: ",
         paste(missing_cols, collapse = ", "), ".", call. = FALSE)
  }

  w <- data[[weights]]
  if (!is.numeric(w)) {
    stop("The weights column '", weights, "' must be numeric.", call. = FALSE)
  }
  if (anyNA(w)) {
    stop("The weights column '", weights, "' contains missing values. ",
         "Survey weights must be complete.", call. = FALSE)
  }
  if (any(w < 0)) {
    stop("The weights column '", weights, "' contains negative values. ",
         "Survey weights must be non-negative.", call. = FALSE)
  }

  # --- 2. Build the design on the FULL sample ---
  ids_f    <- stats::as.formula(paste0("~", ids))
  w_f      <- stats::as.formula(paste0("~", weights))
  strata_f <- if (!is.null(strata)) stats::as.formula(paste0("~", strata)) else NULL

  design <- survey::svydesign(ids = ids_f, strata = strata_f, weights = w_f,
                              data = data, nest = nest)
  n_full <- nrow(data)

  # --- 3. Optional subpopulation via subset (correct for SEs) ---
  if (!is.null(subpop)) {
    if (!is.character(subpop) || length(subpop) != 1) {
      stop("`subpop` must be a single character string giving a logical ",
           "condition, e.g. \"age >= 20\".", call. = FALSE)
    }
    keep <- tryCatch(
      eval(parse(text = subpop), envir = data, enclos = parent.frame()),
      error = function(e) {
        stop("Could not evaluate `subpop` (\"", subpop, "\"): ",
             conditionMessage(e), call. = FALSE)
      }
    )
    if (!is.logical(keep) || length(keep) != n_full) {
      stop("`subpop` must evaluate to a logical condition over the rows of ",
           "`data` (got length ", length(keep), ").", call. = FALSE)
    }
    keep[is.na(keep)] <- FALSE
    design <- design[which(keep), ]
    if (verbose) {
      message(sprintf(
        "Subpopulation: %d of %d rows retained via subset() (design structure preserved for correct SEs).",
        sum(keep), n_full))
    }
  } else if (verbose) {
    message(sprintf("Survey design built on %d rows.", n_full))
  }

  # --- 4. Lonely-PSU advisory (does not change global options) ---
  if (verbose && !is.null(strata) && !identical(ids, "0")) {
    psu_per_stratum <- tapply(data[[ids]], data[[strata]],
                              function(x) length(unique(x)))
    n_lonely <- sum(psu_per_stratum < 2, na.rm = TRUE)
    if (n_lonely > 0) {
      message(sprintf(
        "Note: %d stratum/strata contain a single PSU. Variance estimation may fail; consider options(survey.lonely.psu = \"adjust\").",
        n_lonely))
    }
  }

  design
}
