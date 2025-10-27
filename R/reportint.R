#' Create a Multi-Panel Interaction Report
#'
#' @description
#' This wrapper function automates the creation of a three-panel interaction
#' report (Joint Effects, Simple Effects, Additive Interaction). It can
#' return the data as a list, render to the RStudio Viewer, or save the
#' source .Rmd file.
#'
#' It takes a **single pre-fitted model** as input and works for `glm`,
#' `svyglm`, `coxph`, and `svycoxph` objects.
#'
#' @param interaction_model A fitted model object containing an interaction
#'   term (e.g., `outcome ~ factor1 * factor2 + ...`).
#' @param factor1_name Character string. The name of the first factor variable.
#'   If `NULL` (default), the function will attempt to auto-detect the
#'   interaction terms.
#' @param factor2_name Character string. The name of the second factor variable.
#'   If `NULL` (default), the function will attempt to auto-detect the
#'   interaction terms.
#' @param output Character string. The desired output type:
#'   \itemize{
#'     \item `"list"` (default): Returns a named list containing the data frames for
#'       Panel A, B, C, and the notes for Panel D.
#'     \item `"viewer"`: Renders an HTML report to the RStudio
#'       Viewer (or default browser).
#'     \item `"rmd"`: Saves the auto-generated .Rmd file. Requires
#'       `output_file` to be set.
#'   }
#' @param output_file Character string. The file path to save the .Rmd file
#'   (e.g., "my_report.Rmd"). Only used if `output = "rmd"`.
#' @param digits_ratio Integer. Digits for rounding ORs/CIs in Panels A & B.
#' @param digits_additive Integer. Digits for rounding RERI/AP/S in Panel C.
#' @param conf.level Confidence level (default 0.95).
#' @param verbose Logical. If `TRUE`, prints status messages to the console.
#'
#' @return
#' If `output = "list"`, returns a named list of data frames.
#' For all other outputs, invisibly returns the same named list.
#' The list contains: `joint_effects`, `stratum_specific_effects`,
#' `additive_interaction`, and `model_details` (as a tibble).
#'
#' @details
#' This function requires the 'svyTable1' and 'Publish' packages.
#'
#' @importFrom dplyr mutate if_else select left_join rename
#' @importFrom tidyr pivot_wider
#' @importFrom rlang sym .data
#' @importFrom stats pnorm formula terms
#' @importFrom utils browseURL
#' @importFrom rmarkdown render
#' @importFrom tibble tibble
#' @importFrom Publish publish
#' @importFrom rstudioapi isAvailable viewer
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # --- Load required libraries for the example ---
#' if (requireNamespace("svyTable1", quietly = TRUE) &&
#'     requireNamespace("Publish", quietly = TRUE) &&
#'     requireNamespace("survey", quietly = TRUE) &&
#'     requireNamespace("NHANES", quietly = TRUE) &&
#'     requireNamespace("dplyr", quietly = TRUE) &&
#'     requireNamespace("tidyr", quietly = TRUE)) {
#'
#'   library(svyTable1)
#'   library(Publish)
#'   library(survey)
#'   library(NHANES)
#'   library(dplyr)
#'   library(tidyr)
#'
#'   # --- 1. Data Preparation (NHANES Example) ---
#'   data(NHANESraw)
#'
#'   vars_needed <- c("Age", "Race1", "BPSysAve", "BMI", "ObeseStatus", "Hypertension_130",
#'                    "SDMVPSU", "SDMVSTRA", "WTMEC2YR")
#'
#'   nhanes_adults_processed <- NHANESraw %>%
#'     dplyr::filter(Age >= 20) %>%
#'     dplyr::mutate(
#'       ObeseStatus = factor(ifelse(BMI >= 30, "Obese", "Not Obese"),
#'                            levels = c("Not Obese", "Obese")),
#'       Hypertension_130 = factor(ifelse(BPSysAve >= 130, "Yes", "No"),
#'                                   levels = c("No", "Yes")),
#'       Race1 = relevel(as.factor(Race1), ref = "White")
#'     ) %>%
#'     dplyr::select(dplyr::all_of(vars_needed)) %>%
#'     tidyr::drop_na()
#'
#'   adult_design_binary <- svydesign(
#'     id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR,
#'     nest = TRUE, data = nhanes_adults_processed
#'   )
#'
#'   # --- 2. FIT ONLY THE INTERACTION MODEL ---
#'
#'   interaction_model_fit <- svyglm(
#'     Hypertension_130 ~ Race1 * ObeseStatus + Age,
#'     design = adult_design_binary, family = quasibinomial()
#'   )
#'
#'   # --- 3. Run the wrapper function ---
#'
#'   # The function will auto-detect "Race1" and "ObeseStatus"
#'   # from the single interaction term.
#'   # output = "list" is now the default
#'   report_list <- reportint(
#'     interaction_model = interaction_model_fit
#'   )
#'
#'   # You can then print the tables:
#'   # print(report_list$joint_effects)
#'   # print(report_list$stratum_specific_effects)
#'
#' }
#' }
reportint <- function(interaction_model,
                      factor1_name = NULL,
                      factor2_name = NULL,
                      output = c("list", "viewer", "rmd"),
                      output_file = NULL,
                      digits_ratio = 2,
                      digits_additive = 3,
                      conf.level = 0.95,
                      verbose = FALSE) {

  # Validate output argument
  output <- match.arg(output)

  # Check output_file requirement
  if (output == "rmd" && is.null(output_file)) {
    stop("`output_file` must be specified when output = 'rmd'.", call. = FALSE)
  }

  # --- 1. Generate all data ---
  # Call the internal helper function
  data_list <- .reportint_generate_data(
    interaction_model = interaction_model,
    factor1_name = factor1_name,
    factor2_name = factor2_name,
    digits_ratio = digits_ratio,
    digits_additive = digits_additive,
    conf.level = conf.level,
    verbose = verbose
  )

  # --- 2. Handle Output ---

  # Option 1: Return the list directly (DEFAULT)
  if (output == "list") {
    if (verbose) message("Returning a list of data frames.")
    return(data_list)
  }

  # --- For "viewer" or "rmd", we must render the Rmd template ---

  # Find the Rmd template within your package
  # *** YOU MUST REPLACE "YourPackageName" WITH YOUR ACTUAL PACKAGE NAME ***
  template_path <- system.file("rmd/interaction_report.Rmd", package = "svyTable1")

  if (template_path == "") {
    stop("Could not find 'interaction_report.Rmd' in 'inst/rmd/'.\n",
         "Make sure the file exists and 'YourPackageName' is correct.", call. = FALSE)
  }

  # --- Create render parameters ---
  # We need to re-create the model details strings for the Rmd params
  model_details_list <- data_list$model_details_internal

  ref_group_str <- sprintf("Reference group: %s & %s. Model: %s adjusted for %s.",
                           model_details_list$ref_level1,
                           model_details_list$ref_level2,
                           model_details_list$model_type_str,
                           model_details_list$adj_vars_str_md)

  # Create a list of all parameters to pass to the Rmd
  rmd_params <- list(
    data_list = data_list,
    ref_level1 = model_details_list$ref_level1,
    ref_level2 = model_details_list$ref_level2,
    factor1_name = model_details_list$factor1_name,
    factor2_name = model_details_list$factor2_name,
    ref_group_str = ref_group_str
  )

  # Option 2: Save the .Rmd file
  if (output == "rmd") {
    if (verbose) message(paste("Saving Rmd file to:", output_file))
    # We copy the *template* file, not a rendered one.
    file.copy(template_path, output_file, overwrite = TRUE)
  }

  # Option 3: Render to viewer
  if (output == "viewer") {
    if (verbose) message("Rendering HTML report to viewer...")
    temp_html_path <- tempfile(fileext = ".html")

    # Guarantee cleanup of the temp HTML file
    on.exit(unlink(temp_html_path), add = TRUE)

    tryCatch(
      rmarkdown::render(
        template_path,
        output_file = temp_html_path,
        params = rmd_params,
        quiet = !verbose # Be quiet unless verbose = TRUE
      ),
      error = function(e) {
        warning(paste("Failed to render HTML report:", e$message), call. = FALSE)
      }
    )

    if (verbose) message("Displaying report.")
    if (rstudioapi::isAvailable()) {
      rstudioapi::viewer(temp_html_path)
    } else {
      if (verbose) message("RStudio not detected. Opening in default browser.")
      utils::browseURL(temp_html_path)
    }
  }

  # For "viewer" and "rmd", return the list invisibly
  invisible(data_list)
}


#' Internal Helper to Generate Interaction Report Data
#' (Not Exported)
#'
#' @inheritParams reportint
#' @return A named list with data frames and internal model details.
#' @noRd
.reportint_generate_data <- function(interaction_model,
                                     factor1_name = NULL,
                                     factor2_name = NULL,
                                     digits_ratio = 2,
                                     digits_additive = 3,
                                     conf.level = 0.95,
                                     verbose = FALSE) {

  # --- Helper Formatting Functions ---
  format_p_value <- function(p) {
    ifelse(is.na(p), "-",
           ifelse(p < 0.0001, "< 1e-04",
                  ifelse(p < 0.001, sprintf("%.6f", p),
                         sprintf("%.3f", p))))
  }

  format_ci <- function(low, upp, digits) {
    sprintf(paste0("[%.", digits, "f, %.", digits, "f]"), low, upp)
  }

  # --- 2. Data & Formula Setup ---
  if (verbose) message("Extracting data from models...")

  # --- Auto-detect factor names if not provided ---
  if (is.null(factor1_name) || is.null(factor2_name)) {
    if (verbose) message("Attempting to auto-detect interaction terms...")
    tl <- attr(stats::terms(interaction_model), "term.labels")
    iterms <- grep(":", tl, value = TRUE)

    if (length(iterms) == 0) {
      stop("No interaction terms found in the model formula.", call. = FALSE)
    } else if (length(iterms) > 1) {
      stop(paste("Multiple interaction terms found:", paste(iterms, collapse = ", "),
                 "\nPlease specify `factor1_name` and `factor2_name` arguments."), call. = FALSE)
    } else {
      factors <- strsplit(iterms[1], ":")[[1]]
      factor1_name <- factors[1]
      factor2_name <- factors[2]
      if (verbose) message(sprintf("Auto-detected interaction: %s * %s", factor1_name, factor2_name))
    }
  } else {
    if (verbose) message(sprintf("Using specified interaction: %s * %s", factor1_name, factor2_name))
  }

  # Get levels from the model's xlevels component
  if (is.null(interaction_model$xlevels)) {
    stop("Could not find factor levels in interaction_model$xlevels. Model must be fit with factor variables.", call. = FALSE)
  }
  f1_levels <- interaction_model$xlevels[[factor1_name]]
  f2_levels <- interaction_model$xlevels[[factor2_name]]
  if (is.null(f1_levels) || is.null(f2_levels)) {
    stop(paste("Could not find", factor1_name, "or", factor2_name, "in interaction_model$xlevels. Check names."), call. = FALSE)
  }
  ref_level1 <- f1_levels[1]
  ref_level2 <- f2_levels[1]

  # --- 3. Fit Models ---
  if (verbose) message("Model provided. Skipping internal fitting.")

  # --- 4. Generate & Format Panel A ---
  if (verbose) message("Generating Panel A data...")
  panelA_data_ratio <- jointeffects(
    interaction_model, factor1_name, factor2_name,
    scale = "ratio", digits = digits_ratio, conf.level = conf.level
  )

  panelA_data_log <- jointeffects(
    interaction_model, factor1_name, factor2_name,
    scale = "log", conf.level = conf.level
  )

  panelA_data_log <- panelA_data_log %>%
    dplyr::mutate(
      p_value = dplyr::if_else(.data$SE == 0 & .data$Estimate == 0, NA_real_, 2 * stats::pnorm(-abs(.data$Estimate / .data$SE)))
    )

  panelA_table <- panelA_data_ratio %>%
    dplyr::left_join(dplyr::select(panelA_data_log, .data$Level1, .data$Level2, .data$p_value), by = c("Level1", "Level2")) %>%
    dplyr::mutate(
      OR_str = sprintf(paste0("%.", digits_ratio, "f"), .data$Estimate),
      CI_str = format_ci(.data$CI.low, .data$CI.upp, digits = digits_ratio),
      p_str = format_p_value(.data$p_value),
      OR_str = dplyr::if_else(.data$Level1 == ref_level1 & .data$Level2 == ref_level2, "1.00", .data$OR_str),
      CI_str = dplyr::if_else(.data$Level1 == ref_level1 & .data$Level2 == ref_level2, "(Reference)", .data$CI_str)
    ) %>%
    dplyr::select(
      !!rlang::sym(factor1_name) := .data$Level1,
      !!rlang::sym(factor2_name) := .data$Level2,
      OR = .data$OR_str,
      `95% CI` = .data$CI_str,
      p = .data$p_str
    )

  # --- 5. Generate & Format Panel B ---
  if (verbose) message("Generating Panel B data...")

  ci_format_str <- "(%s, %s)"

  pub_obj <- Publish::publish(interaction_model, ci.format = ci_format_str, digits = digits_ratio)
  panelB_table_raw <- pub_obj$regressionTable

  table_names <- names(panelB_table_raw)
  estimate_col_name <- NULL

  if ("HazardRatio" %in% table_names) {
    estimate_col_name <- "HazardRatio"
  } else if ("OddsRatio" %in% table_names) {
    estimate_col_name <- "OddsRatio"
  } else if ("Coefficient" %in% table_names) {
    estimate_col_name <- "Coefficient"
  } else {
    stop("Could not find 'HazardRatio', 'OddsRatio', or 'Coefficient' column in Publish::publish output.", call. = FALSE)
  }

  if (verbose) message(sprintf("Detected estimate column in Panel B: '%s'", estimate_col_name))

  panelB_table <- panelB_table_raw %>%
    dplyr::filter(grepl(factor1_name, .data$Variable, ignore.case = TRUE) | grepl(factor2_name, .data$Variable, ignore.case = TRUE)) %>%
    dplyr::rename(
      Comparison = .data$Variable,
      Estimate = !!rlang::sym(estimate_col_name),
      `95% CI` = .data$CI.95,
      p = .data$`p-value`
    ) %>%
    dplyr::select(.data$Comparison, .data$Estimate, .data$`95% CI`, .data$p)


  # --- 6. Generate & Format Panel C ---
  if (verbose) message("Generating Panel C data...")
  panelC_data_raw <- addintlist(
    model = interaction_model,
    factor1_name = factor1_name,
    factor2_name = factor2_name,
    measures = "all",
    conf.level = conf.level,
    digits = digits_additive
  )

  panelC_table <- panelC_data_raw %>%
    dplyr::filter(.data$Measure != "Error") %>%
    dplyr::mutate(
      Est_str = sprintf(paste0("%.", digits_additive, "f"), .data$Estimate),
      CI_str = format_ci(.data$CI_low, .data$CI_upp, digits = digits_additive)
    ) %>%
    dplyr::select(.data$Level1, .data$Level2, .data$Measure, .data$Est_str, .data$CI_str) %>%
    tidyr::pivot_wider(
      names_from = .data$Measure,
      values_from = c(.data$Est_str, .data$CI_str),
      names_glue = "{Measure}_{.value}"
    ) %>%
    dplyr::select(
      !!rlang::sym(factor1_name) := .data$Level1,
      !!rlang::sym(factor2_name) := .data$Level2,
      RERI = .data$RERI_Est_str,
      `RERI 95% CI` = .data$RERI_CI_str,
      AP = .data$AP_Est_str,
      `AP 95% CI` = .data$AP_CI_str,
      S = .data$S_Est_str,
      `S 95% CI` = .data$S_CI_str
    )

  # --- 7. Generate Panel D Notes ---
  if (verbose) message("Generating Panel D notes...")

  all_vars <- all.vars(stats::formula(interaction_model))
  outcome_formula <- stats::terms(interaction_model)[[2]]

  if (length(outcome_formula) > 1 && (outcome_formula[[1]] == "Surv" || outcome_formula[[1]] == "cbind")) {
    outcome <- paste(deparse(outcome_formula), collapse = "")
  } else {
    outcome <- all_vars[1]
  }

  adj_vars <- all_vars[!all_vars %in% c(as.character(outcome_formula), outcome, factor1_name, factor2_name)]
  adj_vars <- adj_vars[!adj_vars %in% all.vars(outcome_formula)]

  model_class <- class(interaction_model)[1]
  model_type_str <- switch(model_class,
                           "svyglm" = "survey-weighted logistic regression",
                           "glm" = "logistic regression",
                           "svycoxph" = "survey-weighted Cox regression",
                           "coxph" = "Cox regression",
                           paste("model of type", model_class))

  adj_vars_str_tibble <- if (length(adj_vars) == 0) "`None`" else paste(paste0("`", adj_vars, "`"), collapse = ", ")

  model_details_table <- tibble::tibble(
    Item = c(
      "Outcome",
      "Exposures",
      "Joint reference profile",
      "Model",
      "Adjustment variable list",
      "Parameterization"
    ),
    Description = c(
      sprintf("`%s`", outcome),
      sprintf("`%s` (reference %s), `%s` (reference %s).", factor1_name, ref_level1, factor2_name, ref_level2),
      sprintf("%s & %s.", ref_level1, ref_level2),
      model_type_str,
      adj_vars_str_tibble,
      "saturated for interaction."
    )
  )

  # --- 8. Create Final Output List ---

  # Data for Rmd template params
  model_details_internal <- list(
    ref_level1 = ref_level1,
    ref_level2 = ref_level2,
    model_type_str = model_type_str,
    adj_vars_str_md = if (length(adj_vars) == 0) "None" else paste(adj_vars, collapse = ", "),
    factor1_name = factor1_name,
    factor2_name = factor2_name
  )

  output_list <- list(
    joint_effects = panelA_table,
    stratum_specific_effects = panelB_table,
    additive_interaction = panelC_table,
    model_details = model_details_table,
    model_details_internal = model_details_internal # Helper list for rendering
  )

  return(output_list)
}
