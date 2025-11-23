#' @title Test Constant Effect (PH) Assumption for Survey Cox Models
#' @description
#' This function provides a valid alternative to \code{cox.zph()} for complex survey
#' designs. It checks the Constant Effect (Proportional Hazards) assumption by
#' splitting the follow-up time into intervals (based on event quantiles), fitting
#' separate models for each interval, and plotting the estimated coefficients over time.
#'
#' @param formula_rhs A string specifying the right-hand side of the Cox model formula
#'                    (e.g., "treatment + age + sex").
#' @param design The original, un-split \code{svydesign} object.
#' @param var_to_test A string specifying the exact coefficient name to plot
#'                    (e.g., "age" or "treatmentYes").
#' @param time_var A string for the time-to-event variable name (default "stime").
#' @param status_var A string for the event status variable name (default "status", usually 0/1).
#' @param n_intervals Integer. The number of time intervals to create based on event
#'                    time quantiles (default is 5).
#' @param design_ids (Optional) A formula specifying the cluster IDs (e.g., \code{~psu})
#'                   to be used if automatic extraction fails.
#' @param design_weights (Optional) A formula specifying the survey weights (e.g., \code{~weight})
#'                       to be used if automatic extraction fails.
#' @param design_strata (Optional) A formula specifying the strata (e.g., \code{~strata})
#'                      to be used if automatic extraction fails.
#' @param verbose Logical. If TRUE, prints debugging information about cut-points and convergence.
#' @param print_main_model Logical. If TRUE, prints a tidy summary of the main
#'                         (un-split) model.
#' @param print_split_summary Logical. If TRUE, prints a tidy summary table of
#'                            coefficients for every time interval.
#' @param add_smoother Logical. If TRUE, adds a loess smooth line to the plot.
#' @param title String. Custom title for the plot.
#' @param xlab String. Custom x-axis label.
#' @param ylab String. Custom y-axis label.
#' @param use_classic_theme Logical. If TRUE, uses \code{theme_classic()}.
#' @param show_null_effect Logical. If TRUE, draws a dashed red line at y=0.
#'
#' @details
#' \strong{Methodology:}
#' The function uses "Event-Based Splitting." Instead of splitting time into equal
#' chunks (e.g., every 5 years), it splits time based on quantiles of the event
#' times (e.g., 20th, 40th percentiles). This ensures that each interval contains
#' roughly the same number of events, maintaining statistical power across all
#' segments.
#'
#' \strong{Fix for Pipe/Subset Errors:}
#' Survey design objects created via \code{magrittr} pipes or \code{subset()} often
#' cannot be easily reconstructed. This function attempts to automatically extract
#' the necessary design components (ids, weights, strata) and attach them to the data
#' before splitting. If this fails, the user can manually provide the
#' \code{design_ids}, \code{design_weights}, and \code{design_strata} arguments.
#'
#' @return A \code{ggplot} object showing the coefficient trend over time.
#'
#' @importFrom survey svycoxph svydesign
#' @importFrom survival survSplit
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_errorbar geom_hline geom_smooth theme_classic theme_minimal labs
#' @importFrom knitr kable
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # --- 1. Load data and create a design ---
#' library(survey)
#' library(dplyr)
#' data(nhanes_mortality, package = "svyTable1")
#'
#' # Create a base design
#' analytic_design <- svydesign(
#'   strata = ~strata,
#'   id = ~psu,
#'   weights = ~survey_weight,
#'   data = nhanes_mortality,
#'   nest = TRUE
#' )
#'
#' # Prepare data (filter to >0 time)
#' data_clean <- analytic_design$variables %>%
#'   dplyr::filter(stime > 0) %>%
#'   mutate(caff_bin = ifelse(caff == "No consumption", "No", "Yes"))
#'
#' # Create final design object
#' final_design <- svydesign(
#'   strata = ~strata,
#'   id = ~psu,
#'   weights = ~survey_weight,
#'   data = data_clean,
#'   nest = TRUE
#' )
#'
#' # --- Example 1: Standard Usage ---
#' svycoxph_CE(
#'   formula_rhs = "caff_bin + age",
#'   design = final_design,
#'   var_to_test = "age",
#'   time_var = "stime",
#'   status_var = "status"
#' )
#'
#' # --- Example 2: Manual Safety Valve (if automatic extraction fails) ---
#' svycoxph_CE(
#'   formula_rhs = "caff_bin + age",
#'   design = final_design,
#'   var_to_test = "caff_binYes", # Note: Check exact coef name
#'   design_ids = ~psu,
#'   design_weights = ~survey_weight,
#'   design_strata = ~strata
#' )
#' }

svycoxph_CE <- function(formula_rhs,
                        design,
                        var_to_test,
                        time_var = "stime",
                        status_var = "status",
                        n_intervals = 5,
                        design_ids = NULL,
                        design_weights = NULL,
                        design_strata = NULL,
                        verbose = FALSE,
                        print_main_model = TRUE,
                        print_split_summary = TRUE,
                        add_smoother = FALSE,
                        title = NULL,
                        xlab = NULL,
                        ylab = NULL,
                        use_classic_theme = TRUE,
                        show_null_effect = TRUE) {

  # --- 1. Extract data and clean ---
  data_full <- design$variables
  # Robust filtering: standard subsetting is safer for package code
  data_clean <- data_full[data_full[[time_var]] > 0, ]

  # --- 2. Fit the Main Model ---
  main_model_formula <- as.formula(paste0("Surv(", time_var, ", ", status_var, ") ~ ", formula_rhs))

  if (print_main_model) {
    print("--- Fitting Main (Constant Effect) Model ---")
    main_fit <- survey::svycoxph(main_model_formula, design = design)
    sum_fit <- summary(main_fit)
    hrs <- sum_fit$conf.int
    coefs <- sum_fit$coefficients

    tidy_main_model <- data.frame(
      Variable = rownames(hrs),
      HR = hrs[, "exp(coef)"],
      `CI.Lower` = hrs[, "lower .95"],
      `CI.Upper` = hrs[, "upper .95"],
      `p.value` = coefs[, "Pr(>|z|)"],
      row.names = NULL, check.names = FALSE
    )
    print(knitr::kable(tidy_main_model, digits = 3, format = "pipe"))
    cat("\n")
  }

  # --- 3. Define Cut-points ---
  event_times <- data_clean[[time_var]][data_clean[[status_var]] == 1]
  probs <- (1:(n_intervals - 1)) / n_intervals
  cuts <- quantile(event_times, probs = probs, na.rm = TRUE)

  if(verbose) {
    print("--- Cut points used ---")
    print(cuts)
  }

  # --- 4. Prepare Data (Conceptual Fix: Auto-Extract Design Features) ---
  # We extract weights/ids now and attach them to the dataframe so they
  # survive the survSplit duplication process.

  wts <- weights(design)
  # Ensure weights align with filtered data if design wasn't already subset
  if(length(wts) == nrow(data_full)) {
    wts <- wts[data_full[[time_var]] > 0]
  }
  data_clean$internal_wts_auto <- wts

  if(!is.null(design$cluster) && length(design$cluster) > 0) {
    ids <- design$cluster[[1]]
    if(length(ids) == nrow(data_full)) ids <- ids[data_full[[time_var]] > 0]
    data_clean$internal_ids_auto <- ids
  } else {
    data_clean$internal_ids_auto <- 1:nrow(data_clean)
  }

  if(!is.null(design$strata) && length(design$strata) > 0) {
    strats <- design$strata[[1]]
    if(length(strats) == nrow(data_full)) strats <- strats[data_full[[time_var]] > 0]
    data_clean$internal_strata_auto <- strats
  } else {
    data_clean$internal_strata_auto <- rep(1, nrow(data_clean))
  }

  # --- 5. Perform Split ---
  split_formula <- as.formula(paste0("Surv(", time_var, ", ", status_var, ") ~ ."))
  split_data <- survival::survSplit(split_formula,
                                    data = data_clean,
                                    cut = cuts,
                                    episode = "tgroup",
                                    id = "internal_split_id")

  # --- 6. Create New Design ---
  use_manual_design <- !is.null(design_ids) | !is.null(design_weights)

  if (use_manual_design) {
    if (verbose) print("--- Using Manual Design Specs (Safety Valve) ---")
    if (is.null(design_ids) || is.null(design_weights)) {
      stop("Error: If using manual design specs, you must provide both 'design_ids' and 'design_weights'.")
    }

    # Suppress messages to avoid default survey printout
    split_design <- suppressMessages(
      survey::svydesign(ids = design_ids,
                        weights = design_weights,
                        strata = design_strata,
                        data = split_data,
                        nest = TRUE)
    )
  } else {
    if (verbose) print("--- Using Automatic Design Extraction ---")
    split_design <- suppressMessages(
      survey::svydesign(ids = ~internal_ids_auto,
                        weights = ~internal_wts_auto,
                        strata = ~internal_strata_auto,
                        data = split_data,
                        nest = TRUE)
    )
  }

  # --- 7. Fit Interval Models ---
  plot_results_list <- list()
  summary_results_list <- list()
  time_intervals <- c(0, cuts, max(split_data[[time_var]]))
  loop_formula <- as.formula(paste0("Surv(tstart, ", time_var, ", ", status_var, ") ~ ", formula_rhs))

  for (i in 1:n_intervals) {
    fit <- tryCatch({
      # Subset the design for the specific time group
      d_sub <- suppressMessages(
        subset(split_design, split_design$variables$tgroup == i)
      )

      suppressWarnings(survey::svycoxph(loop_formula, design = d_sub))
    }, error = function(e) NULL)

    if (!is.null(fit)) {
      sum_fit <- summary(fit)
      hrs <- sum_fit$conf.int
      coefs <- sum_fit$coefficients

      if (print_split_summary) {
        summary_results_list[[i]] <- data.frame(
          Time.Interval = paste0(round(time_intervals[i], 0), "-", round(time_intervals[i+1], 0)),
          Variable = rownames(hrs),
          HR = hrs[, "exp(coef)"],
          `CI.Lower` = hrs[, "lower .95"],
          `CI.Upper` = hrs[, "upper .95"],
          `p.value` = coefs[, "Pr(>|z|)"],
          check.names = FALSE
        )
      }

      if (var_to_test %in% rownames(coefs)) {
        coef <- coefs[var_to_test, "coef"]
        se <- coefs[var_to_test, "robust se"]
        plot_results_list[[i]] <- data.frame(
          tgroup = i,
          time = (time_intervals[i] + time_intervals[i+1]) / 2,
          coef = coef,
          se = se
        )
      }
    }
  }

  # --- 8. Results & Plot ---
  if (print_split_summary && length(summary_results_list) > 0) {
    print("--- Summary of Time-Interval Models ---")
    full_summary <- do.call(rbind, summary_results_list)
    rownames(full_summary) <- NULL
    print(knitr::kable(full_summary, digits = 3, format = "pipe"))
  }

  results_df <- do.call(rbind, plot_results_list)
  if (is.null(results_df) || nrow(results_df) == 0) {
    stop("No models converged or the variable to test was not found.")
  }

  results_df <- results_df %>%
    mutate(ci_low = .data$coef - 1.96 * .data$se,
           ci_high = .data$coef + 1.96 * .data$se)

  # Internal helper for defaults
  `%||%` <- function(a, b) if (is.null(a)) b else a

  plot_theme <- if (use_classic_theme) ggplot2::theme_classic() else ggplot2::theme_minimal()

  p <- ggplot2::ggplot(results_df, ggplot2::aes(x = .data$time, y = .data$coef)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
    ggplot2::geom_line(color = "gray30") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$ci_low, ymax = .data$ci_high),
                           width = 0.5, color = "black") +
    ggplot2::geom_point(size = 3, color = "black") +
    plot_theme +
    ggplot2::labs(title = title %||% paste("Time-Varying Effect:", var_to_test),
                  x = xlab %||% "Follow-up Time",
                  y = ylab %||% "Log-Hazard Ratio")

  if (add_smoother) {
    p <- p + ggplot2::geom_smooth(se = FALSE, linetype = "dotted", color = "blue")
  }

  return(p)
}
