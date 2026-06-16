# --- 1. THE FUNCTION DEFINITION ---

#' @title Test Constant Effect (PH) Assumption for Pooled svycoxph Models
#' @description
#' This function provides a valid alternative to \code{cox.zph()} for complex survey
#' designs with multiple imputation. It takes a multiply-imputed survey design
#' object that has been split into time intervals (using \code{survival::survSplit}) and
#' fits a separate pooled model for each interval. It generates a plot of the
#' coefficients over time and provides tidy summary tables to guide model
#' selection.
#'
#' @param formula_rhs A string for the RHS of the Cox model (e.g., "var + age").
#' @param design_split A \code{svyimputationList} object, created from data that
#'                     has *already been split* using \code{survSplit()}.
#' @param var_to_test A string for the exact coefficient name to plot
#'                    (e.g., "active_asthma" or "age").
#' @param tgroup_var A string for the time-interval variable created by
#'                   \code{survSplit} (default is "tgroup").
#' @param time_var A string for the original 'stop' time variable in the
#'                 split data (default is "stime").
#' @param status_var A string for the event status variable, coded 0/1
#'                   (default is "status").
#' @param main_model_fit A pooled \code{svycoxph} fit object (class \code{mipo}). This is the
#'                       main, un-split model. It is optional, but highly
#'                       recommended, as the function will print its tidy
#'                       summary for you.
#' @param print_split_summary A logical. If TRUE, prints a tidy summary table
#'                            of all pooled coefficients from all interval models.
#' @param ... Other arguments passed to plotting, such as \code{title},
#'            \code{xlab}, \code{ylab}, \code{add_smoother} (TRUE/FALSE),
#'            \code{use_classic_theme} (TRUE/FALSE), or
#'            \code{show_null_effect} (TRUE/FALSE).
#'
#' @details
#' \strong{How to Interpret the Results:}
#'
#' This function helps you decide which model to report in your paper.
#'
#' 1.  \strong{Examine the "Time-Varying Effect Plot":}
#'     * \strong{If the plot is flat:} The confidence intervals for all time
#'         intervals should substantially overlap. This indicates that the effect
#'         of your variable is \strong{constant} over time.
#'     * \strong{If the plot is sloped:} A clear, non-random trend (upward or
#'         downward) suggests the effect is \strong{not constant}. The statistical
#'         test for this is to check if the confidence intervals from
#'         different time points (e.g., the first and last) do *not* overlap.
#'
#' 2.  \strong{Choose Your Final Model:}
#'     * \strong{If the assumption is MET (plot is flat):} You should report the
#'         \strong{"Tidy Table for Final Paper (if Constant Effect Assumption is Met)"}.
#'         This single, pooled model is valid, more precise, and the
#'         correct model to report.
#'     * \strong{If the assumption is VIOLATED (plot is sloped):} You
#'         \strong{must not} report the main model. It is statistically invalid.
#'         The correct model to report is the time-split model, which is
#'         summarized in the \strong{"Tidy Summary of All Time-Interval Models"}
#'         table.
#'
#' @return A \code{ggplot} object. The y-axis is the log hazard ratio for
#'   \code{var_to_test} within each follow-up interval, pooled across imputations
#'   (Rubin's rules) and plotted against time; a roughly flat trend supports the
#'   proportional-hazards (constant-effect) assumption. This is a visual
#'   diagnostic, not a formal hypothesis test.
#'
#' @importFrom mice pool
#' @importFrom dplyr n
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \donttest{
#' # MI version: test the constant-effect (PH) assumption across follow-up
#' # intervals, pooling over imputations with Rubin's rules.
#' if (requireNamespace("mice", quietly = TRUE) &&
#'     requireNamespace("mitools", quietly = TRUE)) {
#'
#'   library(survival)   # for Surv() / survSplit() in the formula below
#'   data(nhanes_mortality, package = "svyTable1")
#'   dat <- nhanes_mortality[nhanes_mortality$stime > 0, ]
#'   dat$caff_bin <- factor(ifelse(dat$caff == "No consumption", "No", "Yes"))
#'
#'   # Induce missingness, then impute (small m for a fast example only; for a
#'   # real analysis use m >= 20 and check convergence).
#'   set.seed(123)
#'   dat$age[runif(nrow(dat)) < 0.10] <- NA
#'   imp <- mice::mice(dat[, c("caff_bin", "sex", "age", "stime", "status",
#'                             "psu", "strata", "survey_weight")],
#'                     m = 2, maxit = 2, printFlag = FALSE, seed = 123)
#'   long <- mice::complete(imp, "long", include = FALSE)
#'
#'   # Split follow-up at the median event time into two intervals.
#'   cuts <- median(dat$stime[dat$status == 1], na.rm = TRUE)
#'   long_split <- survSplit(Surv(stime, status) ~ .,
#'                           data = long, cut = cuts,
#'                           episode = "tgroup", id = "split_id")
#'   design_split <- survey::svydesign(
#'     id = ~psu, strata = ~strata, weights = ~survey_weight, nest = TRUE,
#'     data = mitools::imputationList(split(long_split, long_split$.imp))
#'   )
#'
#'   svycoxph_CE_mi(
#'     formula_rhs = "caff_bin + sex + age",
#'     design_split = design_split,
#'     var_to_test = "caff_binYes",
#'     tgroup_var = "tgroup",
#'     time_var = "stime",
#'     status_var = "status",
#'     print_split_summary = FALSE
#'   )
#' }
#' }

svycoxph_CE_mi <- function(formula_rhs,
                           design_split,
                           var_to_test,
                           tgroup_var = "tgroup",
                           time_var = "stime",
                           status_var = "status",
                           main_model_fit = NULL,
                           print_split_summary = TRUE,
                           ...) {

  # --- 1. Extract data and formula components ---
  data_clean <- design_split$designs[[1]]$variables # Use first imputation for setup

  # Check for required split columns
  if (!tgroup_var %in% names(data_clean)) {
    stop(paste("Error: tgroup_var '", tgroup_var, "' not found in design object.", sep=""))
  }
  if (!"tstart" %in% names(data_clean)) {
    stop("Error: 'tstart' column not found. Was this design created with survSplit?")
  }

  tgroups <- sort(unique(data_clean[[tgroup_var]]))
  n_intervals <- length(tgroups)

  # --- 2. Print Main (PH) Model (if provided) ---
  if (!is.null(main_model_fit)) {
    print("--- Tidy Table for Final Paper (if Constant Effect Assumption is Met) ---")

    # Check if it's a pooled object
    if ("mipo" %in% class(main_model_fit)) {
      sum_fit <- summary(main_model_fit, all.comp = TRUE, conf.int = TRUE)

      tidy_main_model <- data.frame(
        Variable = sum_fit$term,
        HR = exp(sum_fit$estimate), # FIX: Convert coef to HR
        `CI.Lower` = sum_fit$conf.low,
        `CI.Upper` = sum_fit$conf.high,
        `p.value` = sum_fit$p.value,
        row.names = NULL,
        check.names = FALSE
      )
      print(knitr::kable(tidy_main_model, digits = 3, format = "pipe"))
    } else {
      print("Warning: 'main_model_fit' was not a pooled (mipo) object. Skipping summary.")
    }
    cat("\n\n")
  }

  # --- 3. Loop, Fit Pooled Models, and Extract Coefficients ---
  print(paste("--- Running Constant Effect Check: Using", n_intervals, "intervals ---"))

  plot_results_list <- list()
  summary_results_list <- list()

  loop_formula <- as.formula(
    paste0("Surv(tstart, ", time_var, ", ", status_var, ") ~ ", formula_rhs)
  )

  for (i in tgroups) {

    # Subset the pooled design
    design_subset <- subset(design_split, eval(as.name(tgroup_var)) == i)

    # Fit the model m times and pool the results
    fit_list <- with(design_subset, svycoxph(loop_formula))
    fit_pooled <- pool(fit_list)

    sum_fit <- summary(fit_pooled, all.comp = TRUE, conf.int = TRUE)
    coefs <- summary(fit_pooled, all.comp = TRUE, conf.int = FALSE)

    # --- Extract data for the Tidy Summary Table ---
    if (print_split_summary) {

      # Get interval times for label
      tstart_val <- min(design_subset$designs[[1]]$variables$tstart)
      tstop_val <- max(design_subset$designs[[1]]$variables[[time_var]])

      tidy_summary <- data.frame(
        Time.Interval = paste0(round(tstart_val, 0), "-", round(tstop_val, 0), " units"),
        Variable = sum_fit$term,
        HR = exp(sum_fit$estimate), # FIX: Convert coef to HR
        `CI.Lower` = sum_fit$conf.low,
        `CI.Upper` = sum_fit$conf.high, # FIX: Typo (.9G -> .95)
        `p.value` = sum_fit$p.value,
        row.names = NULL,
        check.names = FALSE
      )
      summary_results_list[[i]] <- tidy_summary
    }

    # --- Extract data for the Plot ---
    var_index <- which(coefs$term == var_to_test)
    if (length(var_index) > 0) {
      coef <- coefs$estimate[var_index]
      se <- coefs$std.error[var_index]
    } else {
      coef <- 0
      se <- 0
    }

    plot_results_list[[i]] <- data.frame(
      tgroup = i,
      coef = coef,
      se = se
    )
  }

  # --- 4. Print Tidy Summary Table (if requested) ---
  if (print_split_summary) {
    full_summary_df <- do.call(rbind, summary_results_list)
    print("--- Tidy Summary of All Time-Interval Models (if Constant Effect is Violated) ---")
    print(knitr::kable(full_summary_df, digits = 3, format = "pipe"))
    cat("\n\n")
  }

  # --- 5. Combine and Plot the Results ---
  results_df <- do.call(rbind, plot_results_list)

  if (is.null(results_df) || nrow(results_df) == 0) {
    stop("All models failed to converge or the variable was not found.")
  }

  results_df <- results_df %>%
    mutate(
      time = 1:n(), # Use simple 1, 2, 3... for x-axis
      ci_low = .data$coef - 1.96 * .data$se,
      ci_high = .data$coef + 1.96 * .data$se
    )

  print("--- Estimated Coefficients over Time (for Plot) ---")
  print(knitr::kable(results_df, digits = 3, format = "pipe"))

  # --- 6. Build the Plot ---
  # Capture optional plotting arguments
  plot_args <- list(...)
  plot_title <- plot_args$title %||% paste("Time-Varying Effect Plot:", var_to_test)
  plot_xlab <- plot_args$xlab %||% "Time Interval"
  plot_ylab <- plot_args$ylab %||% paste("Log-Hazard Ratio (Coefficient) for", var_to_test)
  use_classic_theme <- plot_args$use_classic_theme %||% TRUE
  show_null_effect <- plot_args$show_null_effect %||% FALSE
  add_smoother <- plot_args$add_smoother %||% FALSE

  plot_theme <- if (use_classic_theme) theme_classic() else theme_minimal()

  p <- ggplot(results_df, aes(x = time, y = coef)) +
    geom_point(size = 3) +
    geom_line() +
    geom_errorbar(aes(ymin = .data$ci_low, ymax = .data$ci_high), width = 0.2) +
    plot_theme +
    labs(title = plot_title, x = plot_xlab, y = plot_ylab) +
    scale_x_continuous(breaks = 1:n_intervals, labels = 1:n_intervals)

  if (show_null_effect) {
    p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "red")
  }

  if (add_smoother) {
    p <- p + geom_smooth(method = "loess", se = FALSE, linetype = "dotted", color = "blue")
  }

  return(p)
}

# --- Internal helper for ... arguments ---
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
