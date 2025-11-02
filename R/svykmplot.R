#' Create a Survey-Weighted Kaplan-Meier Plot
#'
#' This function creates a publication-ready, survey-weighted Kaplan-Meier plot
#' with an attached "Number at Risk" table and censor markings, using `ggplot2`
#' and `patchwork`.
#'
#' @param formula A survival formula, e.g., `Surv(stime, status) ~ strata_var`.
#' @param design A survey design object created with the `survey` package.
#' @param time_unit The unit for time on the x-axis. One of `"days"`,
#'   `"months"`, or `"years"`. This determines the divisor for the time variable.
#' @param time_breaks A numeric vector of breaks for the x-axis,
#'   e.g., `seq(0, 10, by = 2)`.
#' @param legend_title A character string for the legend title.
#' @param risk_table_title A character string for the "Number at Risk" table's
#'   y-axis title.
#' @param palette A character vector of color codes (e.g., hex codes) to
#'   use for the strata.
#' @param show_pval Logical. If `TRUE`, calculates and displays the
#'   survey-weighted log-rank test p-value.
#' @param show_censor_marks Logical. If `TRUE`, displays censor markings (`+`)
#'   on the survival curves.
#' @param base_font_size Base font size for the plot theme.
#' @param table_font_size Font size for the text in the risk table.
#'
#' @return A list containing the following components:
#' \item{plot}{The final combined `ggplot` object (plot + table) from `patchwork`.}
#' \item{table}{A `data.frame` (tibble) of the number-at-risk data.}
#' \item{p_value}{The numeric p-value from the survey-weighted log-rank test
#' (if `show_pval = TRUE`).}
#' \item{km_fit}{The `svykm` fit object from the `survey` package.}
#' \item{logrank_test}{The `svylogrank` test object (if `show_pval = TRUE`).}
#'
#' @export
#'
#' @importFrom dplyr select mutate group_by ungroup count sym rename
#' @importFrom ggplot2 ggplot aes geom_step geom_ribbon theme_classic labs
#'   scale_x_continuous coord_cartesian theme element_blank scale_color_manual
#'   scale_fill_manual geom_point annotate geom_text scale_y_discrete
#'   element_text
#' @importFrom survival Surv
#' @importFrom patchwork plot_layout
#' @importFrom purrr map_dfr
#' @importFrom rlang sym
#' @importFrom scales comma
#' @importFrom stats na.omit
#' @importFrom survey svydesign svykm svylogrank
#' @importFrom tidyr pivot_wider
#' @importFrom tools toTitleCase
#'
#' @examples
#' \dontrun{
#' # This example uses the 'nhanes_mortality' dataset,
#'
#' if (requireNamespace("survey", quietly = TRUE)) {
#'
#'   # 1. Load the data
#'   data(nhanes_mortality)
#'
#'   # 2. Create the main survey design object
#'   analytic_design <- survey::svydesign(
#'     strata = ~strata,
#'     id = ~psu,
#'     weights = ~survey_weight,
#'     data = nhanes_mortality,
#'     nest = TRUE
#'   )
#'
#'   # 3. Create a subsetted design for females
#'   design_female <- subset(analytic_design, sex == "Female")
#'
#'   # 4. Define the formula
#'   km_formula <- Surv(stime, status) ~ caff
#'
#'   # 5. Define a 4-color palette
#'   distinct_palette <- c("#377EB8", "#FF7F00", "#4DAF4A", "#E41A1C")
#'
#'   # 6. Run the function
#'   km_results_female <- svykmplot(
#'     formula = km_formula,
#'     design = design_female,
#'     legend_title = "Caffeine Consumption",
#'     time_unit = "days", # Use "days" so divisor is 1
#'     time_breaks = seq(0, 240, by = 60),
#'     palette = distinct_palette,
#'     show_pval = TRUE,
#'     show_censor_marks = TRUE
#'   )
#'
#'   # 7. Display the plot (and correct the x-axis label)
#'   if (requireNamespace("ggplot2", quietly = TRUE)) {
#'     km_results_female$plot +
#'       ggplot2::labs(x = "Follow-up Time (Months)")
#'   }
#'
#'   # 8. Print the at-risk table
#'   print(km_results_female$table)
#'
#' }
#' }
svykmplot <- function(
    formula, # e.g., Surv(stime, status) ~ age_meno
    design,  # The original, design object
    time_unit = "years", # "days", "months", or "years"
    time_breaks = seq(0, 16, by = 4),
    legend_title = "Strata",
    risk_table_title = "Number at Risk",
    palette = NULL,
    show_pval = TRUE,
    show_censor_marks = TRUE,
    base_font_size = 11,
    table_font_size = 3.5
) {

  # --- 1. Create a Clean Design Object for Plotting ---

  formula_vars <- all.vars(formula)
  strata_var <- all.vars(formula[[3]])[1]

  # Check for weight variable name
  # Your svyTable1 uses 'survey_weight'
  if ("survey_weight" %in% names(design$variables)) {
    design_vars <- c("psu", "strata", "survey_weight")
  } else if ("svy.weight" %in% names(design$variables)) {
    design_vars <- c("psu", "strata", "svy.weight")
    # Rename for internal consistency
    design$variables$survey_weight <- design$variables$svy.weight
  } else {
    stop("Weight variable not found. Looked for 'survey_weight' or 'svy.weight'.")
  }

  # Get the data before omitting NAs
  data_to_clean <- design$variables %>%
    dplyr::select(dplyr::all_of(c(formula_vars, design_vars)))

  n_before <- nrow(data_to_clean)

  # Create clean_data
  clean_data <- stats::na.omit(data_to_clean)

  n_after <- nrow(clean_data)

  if (n_before > n_after) {
    rows_dropped <- n_before - n_after
    warning(paste(
      rows_dropped, "rows were dropped due to missingness in survival, status, or strata variables.",
      "\nAnalysis is being run on N =", n_after
    ), call. = FALSE)
  }

  design_clean <- survey::svydesign(
    strata = ~strata,
    id = ~psu,
    weights = ~survey_weight,
    data = clean_data,
    nest = TRUE
  )

  # --- 2. Run Survey Survival Calculations ---

  svy_fit_object <- survey::svykm(formula, design = design_clean, se = TRUE)

  n_strata <- length(svy_fit_object)
  strata_names <- names(svy_fit_object)

  # --- 2b. Set Time Scale ---
  time_divisor <- switch(time_unit,
                         "years" = 365.25,
                         "months" = 30.4375,
                         "days" = 1,
                         365.25)
  xlabel <- paste("Follow-up Time (", tools::toTitleCase(time_unit), ")", sep = "")

  # --- 2c. Extract Plot and Censor Data ---

  # Part A: Get the curve and CI data from the svykm fit object
  curve_data_raw <- purrr::map_dfr(seq_along(svy_fit_object), ~{
    data.frame(
      time = svy_fit_object[[.x]]$time,
      surv = svy_fit_object[[.x]]$surv,
      varlog = svy_fit_object[[.x]]$varlog,
      strata = strata_names[.x]
    )
  })

  # Part B: Get the censor data from the *original* clean data
  # We need to get the time and status variables from the formula
  time_var_name <- formula_vars[1]
  status_var_name <- formula_vars[2]

  censor_data_raw <- clean_data %>%
    dplyr::filter(!!rlang::sym(status_var_name) == 0) %>% # status == 0 means censored
    dplyr::select(time = !!rlang::sym(time_var_name), strata = !!rlang::sym(strata_var)) %>%
    dplyr::mutate(strata = as.character(.data$strata))

  # Part C: Manually join censor data to curve data to get Y-values
  censor_data_final <- censor_data_raw %>%
    dplyr::group_by(.data$strata) %>%
    dplyr::do({
      .data <- .
      stratum_name <- .data$strata[1]
      stratum_curve_data <- curve_data_raw %>% dplyr::filter(.data$strata == stratum_name)
      indices <- findInterval(.data$time, stratum_curve_data$time)
      indices[indices == 0] <- 1
      data.frame(
        time = .data$time,
        surv = stratum_curve_data$surv[indices],
        strata = stratum_name
      )
    }) %>% dplyr::ungroup()

  # --- 2c. Convert all times to the final time unit ---

  # We split the mutate to safely handle the upper_ci capping
  curve_data <- curve_data_raw %>%
    dplyr::mutate(
      time = .data$time / time_divisor,
      lower_ci = exp(log(.data$surv) - 1.96 * sqrt(.data$varlog)),
      upper_ci = exp(log(.data$surv) + 1.96 * sqrt(.data$varlog)),
      strata = factor(.data$strata, levels = strata_names)
    ) %>%
    # A second mutate is the clearest way to reference the new upper_ci
    dplyr::mutate(
      upper_ci = ifelse(.data$upper_ci > 1, 1, .data$upper_ci)
    )

  censor_data <- censor_data_final %>%
    dplyr::mutate(
      time = .data$time / time_divisor,
      strata = factor(.data$strata, levels = strata_names)
    )

  # --- 3. Build the Main Survival Plot ---

  time_limits <- c(min(time_breaks), max(time_breaks))

  p_surv <- ggplot2::ggplot(
    curve_data,
    ggplot2::aes(x = .data$time, color = .data$strata, fill = .data$strata)
  ) +
    ggplot2::geom_step(ggplot2::aes(y = .data$surv), lwd = 1) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$lower_ci, ymax = .data$upper_ci),
      alpha = 0.2, linetype = 0
    ) +
    ggplot2::theme_classic(base_size = base_font_size) +
    ggplot2::labs(
      y = "Survey-Weighted Survival Probability",
      color = legend_title,
      fill = legend_title
    ) +
    ggplot2::scale_x_continuous(breaks = time_breaks, limits = time_limits) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::theme(
      legend.position = "top",
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )

  if (!is.null(palette)) {
    p_surv <- p_surv +
      ggplot2::scale_color_manual(values = palette, labels = strata_names) +
      ggplot2::scale_fill_manual(values = palette, labels = strata_names)
  }

  if (show_censor_marks & nrow(censor_data) > 0) {
    p_surv <- p_surv +
      ggplot2::geom_point(
        data = censor_data,
        ggplot2::aes(y = .data$surv),
        shape = "+",
        size = 3
      )
  }

  # --- 3c. Add P-Value ---

  logrank_test_obj <- NULL
  p_value_num <- NULL

  if (show_pval && n_strata > 1) {
    logrank_test_obj <- survey::svylogrank(formula, design = design_clean)
    p_value_num <- logrank_test_obj[[2]]["p"]
    p_value_text_raw <- ifelse(p_value_num < 0.001,
                               "< 0.001",
                               round(p_value_num, 3))
    p_value_label <- paste("Survey-weighted log-rank test: p =", p_value_text_raw)

    p_surv <- p_surv +
      ggplot2::annotate("text",
                        x = min(time_breaks) + (max(time_breaks) * 0.05),
                        y = 0.15,
                        label = p_value_label,
                        hjust = 0,
                        fontface = "italic",
                        size = table_font_size + 0.5
      )
  }

  # --- 4. Build the "Number at Risk" Table ---

  at_risk_data <- design_clean$variables %>%
    dplyr::mutate(time_converted = !!rlang::sym(time_var_name) / time_divisor)

  table_data <- time_breaks %>%
    purrr::map_dfr(~{
      at_risk_data %>%
        dplyr::filter(time_converted >= .x) %>%
        dplyr::count(!!rlang::sym(strata_var), name = "n.risk") %>%
        dplyr::mutate(time = .x)
    }) %>%
    dplyr::mutate(
      time = as.numeric(time),
      !!rlang::sym(strata_var) := factor(!!rlang::sym(strata_var), levels = strata_names)
    )

  p_table <- ggplot2::ggplot(table_data,
                             ggplot2::aes(x = time, y = !!rlang::sym(strata_var))) +
    ggplot2::geom_text(ggplot2::aes(label = scales::comma(.data$n.risk)), size = table_font_size) +
    ggplot2::theme_classic(base_size = base_font_size) +
    ggplot2::labs(
      x = xlabel,
      y = risk_table_title
    ) +
    ggplot2::scale_x_continuous(breaks = time_breaks, limits = time_limits) +
    ggplot2::scale_y_discrete(labels = strata_names) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(face = "italic"),
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      legend.position = "none"
    )

  # --- 5. Combine and return ---

  printable_table <- table_data %>%
    tidyr::pivot_wider(names_from = .data$time, values_from = .data$n.risk) %>%
    dplyr::rename(!!legend_title := !!rlang::sym(strata_var))

  return(
    list(
      plot = p_surv / p_table + patchwork::plot_layout(heights = c(0.75, 0.25)),
      table = printable_table,
      p_value = p_value_num,
      km_fit = svy_fit_object,
      logrank_test = logrank_test_obj
    )
  )
}
