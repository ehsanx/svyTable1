#' Calculate Joint Effects from an Interaction Model
#'
#' @description
#' Calculates the joint effect for every combination of two interacting
#' categorical variables, relative to the model's reference group. This
#' function reproduces the output of a joint-variable model (e.g., `~ A_B`)
#' using the results from an interaction-term model (e.g., `~ A * B`).
#' Output can be specified on the log scale (e.g., log-OR/log-HR) or the
#' ratio scale (e.g., OR/HR).
#'
#' It works for various model types, including `svyglm`, `svycoxph`, `glm`,
#' and `coxph`, by dynamically identifying the main effect and interaction
#' coefficients.
#'
#' @param interaction_model A fitted model object (e.g., `svycoxph`, `svyglm`)
#'   containing a two-way interaction term (e.g., `A*B`).
#' @param factor1_name Character string. The name of the first factor variable
#'   in the interaction (e.g., `"Race1"`).
#' @param factor2_name Character string. The name of the second factor variable
#'   (e.g., `"ObeseStatus"`).
#' @param scale Character string. The scale for the output estimates, CIs, and SE.
#'   Options are `"ratio"` (default, e.g., OR/HR) or `"log"` (log-OR/log-HR).
#' @param conf.level Confidence level for the interval (default 0.95).
#' @param digits Integer. Number of decimal places for rounding estimates, SE,
#'   and CIs when `scale = "ratio"`. Default is 2. (Set to NULL for no rounding).
#'
#' @return
#' A `tibble` (data frame) with one row for each combination of factor levels.
#' Columns include:
#' \itemize{
#'   \item `Level1`: The level of the first factor.
#'   \item `Level2`: The level of the second factor.
#'   \item `Estimate`: The point estimate on the specified `scale`.
#'   \item `SE`: The standard error on the specified `scale`. Calculated using the
#'     delta method for `scale = "ratio"`.
#'   \item `CI.low`: The lower bound of the confidence interval (on the specified `scale`).
#'   \item `CI.upp`: The upper bound of the confidence interval (on the specified `scale`).
#' }
#'
#' @importFrom stats coef vcov qnorm
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr mutate if_else bind_rows select case_when relocate across
#' @importFrom msm deltamethod
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \donttest{
#' # --- Load required libraries for the example ---
#' if (requireNamespace("survey", quietly = TRUE) &&
#'     requireNamespace("NHANES", quietly = TRUE) &&
#'     requireNamespace("dplyr", quietly = TRUE) &&
#'     requireNamespace("tidyr", quietly = TRUE) &&
#'     requireNamespace("msm", quietly = TRUE)) { # msm needed for ratio SE
#'
#'   library(survey)
#'   library(NHANES)
#'   library(dplyr)
#'   library(tidyr)
#'   library(msm) # Load msm
#'
#'   # --- 1. Data Preparation (NHANES Example) ---
#'   data(NHANESraw)
#'
#'   vars_needed <- c("Age", "Race1", "BPSysAve", "BMI", "ObeseStatus", "Hypertension_130",
#'                    "SDMVPSU", "SDMVSTRA", "WTMEC2YR")
#'
#'   nhanes_adults_processed <- NHANESraw %>%
#'     filter(Age >= 20) %>%
#'     mutate(
#'       ObeseStatus = factor(ifelse(BMI >= 30, "Obese", "Not Obese"),
#'                            levels = c("Not Obese", "Obese")),
#'       Hypertension_130 = factor(ifelse(BPSysAve >= 130, "Yes", "No"),
#'                                 levels = c("No", "Yes")),
#'       Race1 = relevel(as.factor(Race1), ref = "White")
#'     ) %>%
#'     select(all_of(vars_needed)) %>%
#'     drop_na()
#'
#'   adult_design_binary <- svydesign(
#'     id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR,
#'     nest = TRUE, data = nhanes_adults_processed
#'   )
#'
#'   # --- 2. Fit the Interaction Model ---
#'   interaction_model_logit <- svyglm(
#'     Hypertension_130 ~ Race1 * ObeseStatus + Age,
#'     design = adult_design_binary, family = quasibinomial()
#'   )
#'
#'   # --- 3. Run the function ---
#'
#'   # Example 1: Output on Ratio scale (default)
#'   joint_effects_ratio <- jointeffects(
#'     interaction_model = interaction_model_logit,
#'     factor1_name = "Race1",
#'     factor2_name = "ObeseStatus",
#'     scale = "ratio"
#'   )
#'   print("--- Joint Effects (Ratio Scale) ---")
#'   print(joint_effects_ratio, n = 50)
#'
#'   # Example 2: Output on Log scale
#'   joint_effects_log <- jointeffects(
#'     interaction_model = interaction_model_logit,
#'     factor1_name = "Race1",
#'     factor2_name = "ObeseStatus",
#'     scale = "log"
#'   )
#'   print("--- Joint Effects (Log Scale) ---")
#'   print(joint_effects_log, n = 50)
#' }
#' }
jointeffects <- function(interaction_model,
                         factor1_name,
                         factor2_name,
                         scale = c("ratio", "log"), # Added scale argument
                         conf.level = 0.95,
                         digits = 2) {

  scale <- match.arg(scale) # Validate scale argument

  if (!requireNamespace("msm", quietly = TRUE)) {
    stop("Package 'msm' needed for ratio SE calculation. Please install it.", call. = FALSE)
  }

  # --- 1. Extract Model Components ---
  beta <- tryCatch(stats::coef(interaction_model), error = function(e) NULL)
  V <- tryCatch(stats::vcov(interaction_model), error = function(e) NULL)

  if (is.null(beta) || is.null(V)) {
    warning("Could not extract coefficients or vcov from the model.")
    return(NULL)
  }
  all_coef_names <- names(beta)

  # --- 2. Get Factor Levels (including reference) ---
  levels1 <- NULL
  levels2 <- NULL

  # Try accessing xlevels (common in lm, glm, coxph)
  if (!is.null(interaction_model$xlevels)) {
    levels1 <- interaction_model$xlevels[[factor1_name]]
    levels2 <- interaction_model$xlevels[[factor2_name]]
  }
  # Fallback for survey objects
  if (is.null(levels1) && !is.null(interaction_model$survey.design$variables)) {
    model_data_vars <- interaction_model$survey.design$variables
    if (factor1_name %in% names(model_data_vars) && is.factor(model_data_vars[[factor1_name]])) {
      levels1 <- levels(model_data_vars[[factor1_name]])
    }
    if (factor2_name %in% names(model_data_vars) && is.factor(model_data_vars[[factor2_name]])) {
      levels2 <- levels(model_data_vars[[factor2_name]])
    }
  }

  if (is.null(levels1) || length(levels1) < 2) {
    warning("Could not automatically determine levels for factor1: ", factor1_name)
    return(NULL)
  }
  if (is.null(levels2) || length(levels2) < 2) {
    warning("Could not automatically determine levels for factor2: ", factor2_name)
    return(NULL)
  }

  ref_level1 <- levels1[1]
  ref_level2 <- levels2[1]

  # --- 3. Create a grid of all level combinations ---
  all_combinations <- expand.grid(
    Level1 = levels1,
    Level2 = levels2,
    stringsAsFactors = FALSE
  )

  results <- list()

  # --- 4. Loop through each combination ---
  for (i in 1:nrow(all_combinations)) {
    lvl1 <- all_combinations$Level1[i]
    lvl2 <- all_combinations$Level2[i]

    terms_needed <- c() # Coefficients needed for this combination's logEstimate
    logEstimate <- NA # Initialize
    se_log <- NA      # Initialize SE for log estimate
    se_ratio <- NA   # Initialize SE for ratio estimate

    is_ref1 <- (lvl1 == ref_level1)
    is_ref2 <- (lvl2 == ref_level2)

    # --- 5. Determine required coefficients and calculate estimates ---

    if (is_ref1 && is_ref2) {
      # This is the reference group (A=0, B=0)
      logEstimate <- 0
      se_log <- 0
      se_ratio <- 0 # SE of 1 is 0

    } else {
      # --- Build coefficient names ---
      coef1_name <- NULL
      coef2_name <- NULL
      inter_coef_name <- NULL

      # Main effect 1 (if not reference)
      if (!is_ref1) {
        coef1_name <- paste0(factor1_name, lvl1)
        if (coef1_name %in% all_coef_names) terms_needed <- c(terms_needed, coef1_name)
      }

      # Main effect 2 (if not reference)
      if (!is_ref2) {
        coef2_name <- paste0(factor2_name, lvl2)
        if (coef2_name %in% all_coef_names) terms_needed <- c(terms_needed, coef2_name)
      }

      # Interaction effect (if neither is reference)
      if (!is_ref1 && !is_ref2) {
        # Check both A:B and B:A name orderings
        name_v1 <- paste0(coef1_name, ":", coef2_name)
        name_v2 <- paste0(coef2_name, ":", coef1_name)

        if (name_v1 %in% all_coef_names) {
          inter_coef_name <- name_v1
        } else if (name_v2 %in% all_coef_names) {
          inter_coef_name <- name_v2
        }

        if (!is.null(inter_coef_name)) terms_needed <- c(terms_needed, inter_coef_name)
      }

      # --- 6. Check if all required coefficients were found ---
      required_for_case <- c(coef1_name, coef2_name, inter_coef_name)
      if (!all(stats::na.omit(required_for_case) %in% terms_needed)) {
        warning(paste("Could not find all required coefficients for combination:",
                      lvl1, "+", lvl2, ". Skipping."))
        # Keep logEstimate, se_log, se_ratio as NA
      } else {
        # --- 7. Calculate combined estimate and SE ---
        logEstimate <- sum(beta[terms_needed])

        # Variance of sum = sum of all elements in the sub-vcov matrix
        if (length(terms_needed) == 1) {
          if (terms_needed %in% rownames(V) && terms_needed %in% colnames(V)) {
            var_comb <- V[terms_needed, terms_needed]
          } else {
            var_comb <- NA
          }
        } else {
          if (all(terms_needed %in% rownames(V)) && all(terms_needed %in% colnames(V))) {
            V_sub <- V[terms_needed, terms_needed, drop = FALSE]
            var_comb <- sum(V_sub)
          } else {
            var_comb <- NA
          }
        }

        # Handle potential negative variance
        if (!is.na(var_comb)) {
          if (var_comb < 0 && abs(var_comb) < 1e-10) var_comb <- 0
          if (var_comb < 0) {
            warning(paste("Negative variance calculated for combination:", lvl1, "+", lvl2, ". SE set to NA."))
            se_log <- NA
          } else {
            se_log <- sqrt(var_comb)
            # Calculate SE on ratio scale using delta method
            se_ratio <- tryCatch({
              # Ensure var_comb is non-NA and non-negative before passing
              if (is.na(var_comb) || var_comb < 0) stop("Invalid variance for delta method.")
              msm::deltamethod(~exp(x1), logEstimate, var_comb)
            }, error = function(e) {
              warning(paste("Delta method failed for ratio SE (", lvl1, "+", lvl2, "): ", e$message))
              NA
            })
          }
        } else {
          warning(paste("Variance could not be calculated for combination:", lvl1, "+", lvl2, ". SE set to NA."))
          se_log <- NA
          se_ratio <- NA
        }
      }
    }

    results[[i]] <- list(Level1 = lvl1, Level2 = lvl2,
                         logEstimate = logEstimate, SE_log = se_log, SE_ratio = se_ratio)
  }

  # --- 8. Format Output Table ---
  z <- stats::qnorm(1 - (1 - conf.level) / 2)

  output_df_list <- lapply(results, function(res) {
    est_ratio <- NA
    ci.low_ratio <- NA
    ci.upp_ratio <- NA
    ci.low_log <- NA
    ci.upp_log <- NA
    # ci_fmt <- NA_character_ # Removed

    if (!is.na(res$logEstimate) && !is.na(res$SE_log)) {
      if (res$logEstimate == 0 && res$SE_log == 0) { # Reference group
        est_ratio <- 1.0
        ci.low_ratio <- 1.0
        ci.upp_ratio <- 1.0
        ci.low_log <- 0.0
        ci.upp_log <- 0.0
        # ci_fmt <- "1.00 (Reference)" # Removed
      } else {
        est_ratio <- exp(res$logEstimate)
        ci.low_log <- res$logEstimate - z * res$SE_log
        ci.upp_log <- res$logEstimate + z * res$SE_log
        ci.low_ratio <- exp(ci.low_log)
        ci.upp_ratio <- exp(ci.upp_log)

        # # Formatting with specified digits - Removed, now done later if needed
        # if (!is.null(digits)) {
        #     fmt_str_est <- paste0("%.", digits, "f")
        #     fmt_str_ci <- paste0("(", fmt_str_est, ", ", fmt_str_est, ")")
        #     ci_fmt <- sprintf(paste0(fmt_str_est, " ", fmt_str_ci), est_ratio, ci.low_ratio, ci.upp_ratio)
        # } else {
        #     ci_fmt <- sprintf("%g (%g, %g)", est_ratio, ci.low_ratio, ci.upp_ratio)
        # }
      }
    } # else keep NAs

    # Create tibble for this row
    tibble::tibble(
      Level1 = res$Level1,
      Level2 = res$Level2,
      logEstimate = res$logEstimate,
      SE_log = res$SE_log,
      Estimate_ratio = est_ratio,
      SE_ratio = res$SE_ratio, # Add ratio SE
      CI.low_ratio = ci.low_ratio,
      CI.upp_ratio = ci.upp_ratio,
      CI.low_log = ci.low_log,
      CI.upp_log = ci.upp_log
      # CI_formatted = ci_fmt # Removed
    )
  })

  output_df <- dplyr::bind_rows(output_df_list)

  # --- Select final columns based on scale ---
  if (scale == "log") {
    final_df <- output_df %>%
      # Use .data pronoun
      dplyr::select(
        .data$Level1,
        .data$Level2,
        Estimate = .data$logEstimate,
        SE = .data$SE_log,
        CI.low = .data$CI.low_log,
        CI.upp = .data$CI.upp_log
      )
  } else { # scale == "ratio"
    # Round ratio scale columns if digits specified
    if (!is.null(digits)) {
      output_df <- output_df %>%
        # Use across() with string vector
        dplyr::mutate(
          dplyr::across(c("Estimate_ratio", "SE_ratio", "CI.low_ratio", "CI.upp_ratio"),
                        ~round(.x, digits))
        )
    }
    final_df <- output_df %>%
      # Use .data pronoun
      dplyr::select(
        .data$Level1,
        .data$Level2,
        Estimate = .data$Estimate_ratio,
        SE = .data$SE_ratio,
        CI.low = .data$CI.low_ratio,
        CI.upp = .data$CI.upp_ratio
        # CI_formatted column is now completely removed
      )
  }

  return(final_df)
}
