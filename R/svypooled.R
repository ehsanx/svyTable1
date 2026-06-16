#' Create a Publication-Ready Table for Pooled Model Results
#'
#' @description
#' This function takes a pooled model object from the `mice` package and creates a
#' publication-ready HTML table of the model's effect estimates (e.g., odds
#' ratios, hazard ratios).
#'
#' It can produce two types of tables:
#' 1.  A **"fallacy-safe"** table (`fallacy_safe = TRUE`), which displays only the
#'     results for the main exposure variable and lists all adjustment
#'     variables in a footnote. This helps prevent the misinterpretation of
#'     statistics for covariates.
#' 2.  A **full table** (`fallacy_safe = FALSE`), which displays results for all
#'     variables in the model, grouped by variable name.
#'
#' @details
#' The function processes a `mipo` object (the result of `mice::pool()`). It
#' exponentiates the estimates, calculates 95% confidence intervals, and
#' formats the results into a clean HTML table using `knitr::kable()` and
#' `kableExtra`. P-values are formatted to three decimal places, with values
#' less than 0.001 shown as "<0.001".
#'
#' @param pooled_model A `mipo` object resulting from `mice::pool()`. This contains
#'   the pooled results from analyses on multiply imputed datasets.
#' @param main_exposure A character string specifying the name of the main exposure
#'   variable in the model. The function uses this to identify which variable's
#'   results to show in the "fallacy-safe" mode.
#' @param adj_var_names A character vector of the names of all adjustment
#'   variables (covariates) included in the model.
#' @param measure A character string for the column header of the effect
#'   measure (e.g., "OR", "HR", "RR"). Defaults to "OR".
#' @param title A character string for the table's caption.
#' @param fallacy_safe A logical value. If `TRUE` (the default), the function
#'   returns a table showing only the main exposure and lists covariates in a
#'   footnote. If `FALSE`, it returns a full table with all model terms.
#'
#' @return A `kable` object (class `c("kableExtra", "knitr_kable")`): a character
#'   vector of HTML with attributes, ready to print in an R Markdown report. In
#'   "fallacy-safe" mode it contains one row per level of `main_exposure` (with the
#'   adjustment variables named in a footnote); otherwise it contains all model
#'   terms grouped by variable. Each cell shows the effect measure and its 95\%
#'   confidence interval pooled across imputations using Rubin's rules.
#'
#' @importFrom dplyr select mutate case_when
#' @importFrom stringr str_extract str_remove
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling pack_rows footnote
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(nhanes_mortality, package = "svyTable1")
#' dat <- nhanes_mortality
#' dat$htn01 <- as.numeric(dat$htn == "Yes")
#' dat <- dat[, c("htn01", "sex", "age", "smoking", "psu", "strata", "survey_weight")]
#'
#' # Induce missingness, then multiply impute (small m for a fast example only;
#' # for a real analysis use m >= 20 and check convergence).
#' set.seed(1)
#' dat$age[sample(nrow(dat), 200)] <- NA
#' imp <- mice::mice(dat, m = 2, maxit = 2, printFlag = FALSE, seed = 1)
#'
#' # Fit a survey-weighted model in each imputed dataset, then pool (Rubin's rules).
#' fits <- with(imp, survey::svyglm(
#'   htn01 ~ sex + age + smoking,
#'   design = survey::svydesign(id = ~psu, strata = ~strata,
#'                              weights = ~survey_weight, nest = TRUE,
#'                              data = data.frame(mget(ls()))),
#'   family = quasibinomial()
#' ))
#' pooled <- mice::pool(fits)
#'
#' # Fallacy-safe table: shows only the exposure; covariates go in a footnote.
#' svypooled(pooled, main_exposure = "sex",
#'           adj_var_names = c("age", "smoking"), measure = "OR",
#'           title = "Adjusted odds of hypertension")
#' }
svypooled <- function(pooled_model,
                      main_exposure,
                      adj_var_names,
                      measure = "OR",
                      title = "Adjusted Model Results",
                      fallacy_safe = TRUE) {

  # Ensure required packages are available
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("knitr", quietly = TRUE)) stop("Package 'knitr' is required.")
  if (!requireNamespace("kableExtra", quietly = TRUE)) stop("Package 'kableExtra' is required.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Package 'stringr' is required.")

  # Get a summary data frame, removing the intercept
  summary_df <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
  summary_df <- summary_df[summary_df$term != "(Intercept)", ]

  # Combine all variable names for parsing
  all_vars <- c(main_exposure, adj_var_names)
  pattern <- paste(all_vars, collapse = "|")

  processed_results <- summary_df %>%
    dplyr::select("term", "estimate", "conf.low", "conf.high", "p.value") %>%
    dplyr::mutate(
      group = stringr::str_extract(.data$term, pattern),
      Characteristic = stringr::str_remove(.data$term, pattern),
      Estimate_CI = sprintf("%.2f (%.2f, %.2f)", .data$estimate, .data$conf.low, .data$conf.high),
      p_value_formatted = dplyr::case_when(
        .data$p.value < 0.001 ~ "<0.001",
        TRUE ~ sprintf("%.3f", .data$p.value)
      )
    ) %>%
    dplyr::select("group", "Characteristic", "Estimate_CI", "p_value_formatted")

  # Conditionally filter for fallacy-safe output
  if (fallacy_safe) {
    if (!main_exposure %in% processed_results$group) {
      stop(paste("Main exposure '", main_exposure, "' not found in model terms.", sep = ""))
    }
    results_to_display <- processed_results %>% dplyr::filter(.data$group == main_exposure)
    footnote_text <- paste("Adjusted for:", paste(adj_var_names, collapse = ", "))
  } else {
    results_to_display <- processed_results
  }

  # Create the final HTML table using a single pipe chain for robustness
  final_table <- knitr::kable(
    results_to_display[, -1], # Remove helper 'group' column
    col.names = c("Characteristic", paste(measure, "(95% CI)"), "p-value"),
    align = "lcc",
    caption = title,
    row.names = FALSE,
    format = "html" # Explicitly set format for kableExtra
  ) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>%
    {
      # Use curly braces for conditional piping logic
      if (!fallacy_safe) {
        # For the full table, group rows by variable name
        kableExtra::pack_rows(., index = table(factor(results_to_display$group, levels = all_vars)))
      } else {
        # For the fallacy-safe table, add a single header for the main exposure
        kableExtra::pack_rows(., main_exposure, 1, nrow(results_to_display))
      }
    } %>%
    {
      if (fallacy_safe) {
        # Add footnote only in fallacy-safe mode
        kableExtra::footnote(., general = footnote_text, footnote_as_chunk = TRUE, general_title = " ")
      } else {
        . # Pass the table through unchanged if not fallacy_safe
      }
    }

  return(final_table)
}
