#' Calculate All Additive Interaction Measures from an Interaction Model
#'
#' @description
#' Automatically identifies interaction terms in a fitted model involving two
#' categorical factors (e.g., `~ A * B`) and calculates measures of additive
#' interaction (RERI, AP, S) for each combination of non-reference levels.
#' This function requires the `addint()` function to be available.
#'
#' @details
#' This function serves as a wrapper around `addint()`. It determines the
#' levels of the two specified factors from the model object, constructs the
#' expected coefficient names for main effects and interaction terms (checking
#' both `A:B` and `B:A` formats), and then calls `addint()` with
#' `type = "interaction"` for each valid combination found in the model.
#' The results are compiled into a single summary table.
#'
#' @param model A fitted model object (e.g., `svycoxph`, `svyglm`) of type
#'   `"interaction"` (containing a `factor1 * factor2` term).
#' @param factor1_name Character string: The name of the first factor variable
#'   in the interaction (e.g., `"Race1"`).
#' @param factor2_name Character string: The name of the second factor variable
#'   (e.g., `"ObeseStatus"`).
#' @param measures A character vector specifying which measures to calculate via `addint()`.
#'   Options: `"RERI"`, `"AP"`, `"S"`, or `"all"`. Default is `"all"`.
#' @param conf.level Confidence level for the interval (default 0.95).
#' @param digits Integer. Number of decimal places for rounding estimates and
#'   CIs in the final table. Default is 3. (Set to NULL for no rounding).
#' @param verbose Logical. If `TRUE`, prints a progress message for each
#'   factor-level combination as it is calculated. Defaults to `FALSE`.
#' @param ci_method Confidence-interval method for RERI passed to
#'   \code{\link{addint}}: `"delta"` (default) or `"mover"` (Zou 2008).
#'
#' @return A `tibble` (data frame) summarizing the additive interaction results.
#'   Columns include:
#'   \itemize{
#'     \item `Factor1`: Name of the first factor.
#'     \item `Level1`: Non-reference level of the first factor for the comparison.
#'     \item `Factor2`: Name of the second factor.
#'     \item `Level2`: Non-reference level of the second factor for the comparison.
#'     \item `Measure`: The additive interaction measure calculated ("RERI", "AP", or "S").
#'     \item `Estimate`: Point estimate of the measure.
#'     \item `SE`: Standard Error (Note: For S, this is SE of log(S)).
#'     \item `CI_low`: Lower confidence interval bound.
#'     \item `CI_upp`: Upper confidence interval bound.
#'     \item `Scale`: The ratio scale the measures were computed on, inferred
#'       from the model: "OR (logistic)", "HR (Cox)", "RR (log-binomial)",
#'       "RR (Poisson)", or "ratio".
#'   }
#'   Returns an empty tibble if errors occur.
#'
#' @details
#' RERI, AP, and the synergy index S are additive-interaction measures defined on
#' the \emph{risk-ratio} scale. When the model reports odds ratios (logistic) or
#' hazard ratios (Cox), these measures only approximate their risk-ratio
#' counterparts when the outcome is rare; for a common outcome the odds ratio
#' overstates the risk ratio and RERI/AP/S can be biased. The `Scale` column makes
#' the operating scale explicit. To estimate risk-ratio-based interaction
#' directly for a common outcome, fit a log-binomial or Poisson working model.
#'
#' @seealso \code{\link{addint}}
#'
#' @importFrom stats coef
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows mutate relocate select across
#' @importFrom tidyr pivot_longer
#' @importFrom rlang := .data
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(nhanes_mortality, package = "svyTable1")
#' nhanes_mortality$htn01 <- as.numeric(nhanes_mortality$htn == "Yes")
#'
#' design <- survey::svydesign(
#'   id = ~psu, strata = ~strata, weights = ~survey_weight,
#'   nest = TRUE, data = nhanes_mortality
#' )
#'
#' # Saturated interaction model of sex and insulin use.
#' interaction_model <- survey::svyglm(
#'   htn01 ~ sex * insulin + age,
#'   design = design, family = quasibinomial()
#' )
#'
#' # RERI, AP and S for every factor-level combination. The Scale column flags
#' # that these are OR-based (see Details for the rare-outcome caveat).
#' addintlist(
#'   model = interaction_model,
#'   factor1_name = "sex",
#'   factor2_name = "insulin",
#'   measures = "all"
#' )
#' }
addintlist <- function(model,
                       factor1_name,
                       factor2_name,
                       measures = "all",
                       conf.level = 0.95,
                       digits = 3,
                       verbose = FALSE,
                       ci_method = c("delta", "mover")) {

  ci_method <- match.arg(ci_method)

  # Check if the core calculation function 'addint' exists
  if (!exists("addint") || !is.function(addint)) {
    stop("The 'addint' function is required but was not found. Ensure it is loaded.", call. = FALSE)
  }

  valid_measures_req <- c("RERI", "AP", "S", "all")
  if (any(!measures %in% valid_measures_req)) {
    stop("Invalid measure specified in 'measures'. Choose from 'RERI', 'AP', 'S', or 'all'.", call. = FALSE)
  }
  if ("all" %in% measures) {
    measures_to_calc <- c("RERI", "AP", "S")
  } else {
    measures_to_calc <- unique(measures)
  }


  if (!inherits(model, c("svycoxph", "svyglm", "coxph", "glm"))) {
    warning("Model object may not be of a supported type.")
    # Attempt to proceed anyway
  }

  beta <- tryCatch(stats::coef(model), error = function(e) NULL)
  if (is.null(beta)) {
    warning("Could not extract coefficients from the model.")
    return(tibble::tibble()) # Return empty tibble
  }
  coef_names_all <- names(beta)

  # --- Get Factor Levels (excluding reference) ---
  levels1 <- NULL
  levels2 <- NULL

  # Try accessing xlevels if they exist (common in lm, glm)
  if (!is.null(model$xlevels)) {
    levels1 <- model$xlevels[[factor1_name]]
    levels2 <- model$xlevels[[factor2_name]]
  } else if (inherits(model, "survey.design")) {
    # For survey design objects passed directly (less common)
    if (factor1_name %in% names(model$variables) && is.factor(model$variables[[factor1_name]])) {
      levels1 <- levels(model$variables[[factor1_name]])
    }
    if (factor2_name %in% names(model$variables) && is.factor(model$variables[[factor2_name]])) {
      levels2 <- levels(model$variables[[factor2_name]])
    }
  } else if (!is.null(model$survey.design)) {
    # For survey model objects (svyglm, svycoxph)
    model_data_vars <- model$survey.design$variables
    if (factor1_name %in% names(model_data_vars) && is.factor(model_data_vars[[factor1_name]])) {
      levels1 <- levels(model_data_vars[[factor1_name]])
    }
    if (factor2_name %in% names(model_data_vars) && is.factor(model_data_vars[[factor2_name]])) {
      levels2 <- levels(model_data_vars[[factor2_name]])
    }
  } else if (!is.null(model$data)) {
    # For some other model types that might store data
    if (factor1_name %in% names(model$data) && is.factor(model$data[[factor1_name]])) {
      levels1 <- levels(model$data[[factor1_name]])
    }
    if (factor2_name %in% names(model$data) && is.factor(model$data[[factor2_name]])) {
      levels2 <- levels(model$data[[factor2_name]])
    }
  }


  if (is.null(levels1) || length(levels1) < 2) {
    warning("Could not automatically determine levels for factor1: ", factor1_name, ". Check if it's a factor with >1 level in the model data.")
    return(tibble::tibble()) # Return empty tibble
  }
  if (is.null(levels2) || length(levels2) < 2) {
    warning("Could not automatically determine levels for factor2: ", factor2_name, ". Check if it's a factor with >1 level in the model data.")
    return(tibble::tibble()) # Return empty tibble
  }

  ref_level1 <- levels1[1]
  ref_level2 <- levels2[1]
  non_ref_levels1 <- levels1[-1]
  non_ref_levels2 <- levels2[-1]

  all_results_list <- list() # Store results before binding

  # --- Loop through combinations of non-reference levels ---
  for (level1 in non_ref_levels1) {
    for (level2 in non_ref_levels2) {

      comparison_name <- paste0(factor1_name, "_", level1, "_vs_", factor2_name, "_", level2)
      if (verbose) message("Calculating for: ", comparison_name)

      # --- Construct coefficient names ---
      exp1_coef_name <- paste0(factor1_name, level1)
      exp2_coef_name <- paste0(factor2_name, level2)
      inter_coef_name1 <- paste0(factor1_name, level1, ":", factor2_name, level2)
      inter_coef_name2 <- paste0(factor2_name, level2, ":", factor1_name, level1)

      inter_coef_name <- NULL
      if (inter_coef_name1 %in% coef_names_all) {
        inter_coef_name <- inter_coef_name1
      } else if (inter_coef_name2 %in% coef_names_all) {
        inter_coef_name <- inter_coef_name2
      }

      # Check if all needed coefficients were found
      current_coef_names_list <- list(
        exp1_coef = exp1_coef_name,
        exp2_coef = exp2_coef_name,
        inter_coef = inter_coef_name
      )

      required_in_model <- c(exp1_coef_name, exp2_coef_name, inter_coef_name)

      if (is.null(inter_coef_name) || any(!required_in_model %in% coef_names_all) ) {
        warning("Could not find all necessary coefficients for interaction between ",
                level1, " and ", level2, ". Skipping this combination.")
        # Store minimal info about the skip
        all_results_list[[length(all_results_list) + 1]] <- tibble::tibble(
          Factor1 = factor1_name, Level1 = level1,
          Factor2 = factor2_name, Level2 = level2,
          Measure = "Error", Estimate = NA_real_, SE = NA_real_,
          CI_low = NA_real_, CI_upp = NA_real_
        )
        next # Skip to the next combination
      }


      # --- Call the calculation function 'addint' ---
      interaction_results <- addint( # Call the dependency function
        model = model,
        type = "interaction",
        coef_names = current_coef_names_list,
        measures = measures_to_calc, # Use the validated list
        conf.level = conf.level,
        ci_method = ci_method
      )

      # --- Process results into a flat tibble for this combination ---
      if (!is.null(interaction_results) && length(interaction_results) > 0) {
        for(measure_name in names(interaction_results)) {
          res_vector <- interaction_results[[measure_name]]
          se_val <- ifelse(measure_name == "S", res_vector["S_SE_log"], res_vector[2]) # Get correct SE column

          all_results_list[[length(all_results_list) + 1]] <- tibble::tibble(
            Factor1 = factor1_name,
            Level1 = level1,
            Factor2 = factor2_name,
            Level2 = level2,
            Measure = measure_name,
            Estimate = res_vector[1], # First element is always estimate
            SE = se_val,
            CI_low = res_vector[3], # Third element is always LowerCI
            CI_upp = res_vector[4]  # Fourth element is always UpperCI
          )
        }
      } else {
        # Store minimal info if addint returned NULL
        all_results_list[[length(all_results_list) + 1]] <- tibble::tibble(
          Factor1 = factor1_name, Level1 = level1,
          Factor2 = factor2_name, Level2 = level2,
          Measure = "Error", Estimate = NA_real_, SE = NA_real_,
          CI_low = NA_real_, CI_upp = NA_real_
        )
        warning("Calculation failed within addint() for interaction between ",
                level1, " and ", level2, ". Check previous warnings.")
      }
    } # end loop level2
  } # end loop level1

  # --- Combine all results into a single tibble ---
  if (length(all_results_list) == 0) {
    warning("No interaction results could be calculated.")
    # Return an empty tibble with correct columns
    return(tibble::tibble(Factor1 = character(), Level1 = character(),
                          Factor2 = character(), Level2 = character(),
                          Measure = character(), Estimate = double(), SE = double(),
                          CI_low = double(), CI_upp = double()))
  }

  final_table <- dplyr::bind_rows(all_results_list)

  # --- Optional rounding ---
  if (!is.null(digits)) {
    final_table <- final_table %>%
      # Use .data pronoun and string vector for across
      dplyr::mutate(dplyr::across(c("Estimate", "SE", "CI_low", "CI_upp"), ~round(.x, digits)))
  }

  # --- Final structure ---
  # The Scale column makes explicit which ratio measure RERI/AP/S were built
  # from (OR for logistic, HR for Cox, RR for log-binomial/Poisson), since these
  # additive-interaction measures are only RR-scale-correct when the outcome is
  # rare (see Details).
  final_table <- final_table %>%
    dplyr::mutate(Scale = .effect_measure_scale(model)) %>%
    dplyr::relocate("Factor1", "Level1", "Factor2", "Level2",
                    "Measure", "Estimate", "SE", "CI_low", "CI_upp", "Scale")

  return(final_table)
}

# Internal: infer the ratio scale that RERI/AP/S were computed on, from the
# fitted model's class/family. Not exported.
.effect_measure_scale <- function(model) {
  cl <- class(model)[1]
  if (cl %in% c("svycoxph", "coxph")) return("HR (Cox)")
  fam  <- tryCatch(stats::family(model)$family, error = function(e) NA_character_)
  link <- tryCatch(stats::family(model)$link,   error = function(e) NA_character_)
  if (!is.na(fam)) {
    if (grepl("binomial", fam) && identical(link, "logit")) return("OR (logistic)")
    if (grepl("binomial", fam) && identical(link, "log"))   return("RR (log-binomial)")
    if (grepl("poisson", fam))                              return("RR (Poisson)")
  }
  "ratio"
}
