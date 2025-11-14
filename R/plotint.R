#' Plot Interaction Effects from a Regression Model
#'
#' This function creates a publication-ready plot of a two-way interaction
#' from various regression models using the emmeans package.
#'
#' @param model A fitted model object (e.g., svycoxph, glm).
#' @param effect Character string. The name of the focal predictor (x-axis).
#' @param moderator Character string. The name of the moderator variable (lines/groups).
#' @param data The original data frame used to fit the model (REQUIRED for survey models).
#' @param mod_scale Method for probing a continuous moderator: "sd" (default) or "quantile".
#' @param type The scale for the y-axis: "link" (default) or "response".
#' @param show_ci Logical. If TRUE (default), plots 95 percent confidence ribbons.
#' @param bw Logical. If TRUE, creates a black and white plot.
#' @param xlab Character string. Optional custom label for the x-axis.
#' @param ylab Character string. Optional custom label for the y-axis.
#' @param legend_title Character string. Optional custom title for the legend.
#' @param pval_position Position for p-value: "bottom.right", "top.left", etc., or "none".
#'
#' @return Invisibly returns a list containing 'plot_data' and 'p_value_text'.
#'
#' @import ggplot2
#' @import emmeans
#' @import stats
#'
#' @export
#'
#' @examples
#' \donttest{
#' # =================================================================
#' # Example 1: Continuous x Continuous (nhanes_mortality)
#' # =================================================================
#' library(survival)
#' library(survey)
#' library(svyTable1)
#' library(emmeans)
#' library(ggplot2)
#' library(dplyr)
#'
#' # --- 2. Load & Prepare Data ---
#' data("nhanes_mortality", package = "svyTable1")
#'
#' # Filter NAs for the variables used in the model
#' mort_data <- nhanes_mortality %>%
#'   mutate(
#'     log_cal = log(cal.total),
#'     log_carb = log(carbohyd)
#'   ) %>%
#'   na.omit(subset = c("psu", "strata", "survey_weight", "stime", "status",
#'                      "log_cal", "log_carb", "age", "sex", "race"))
#'
#' # --- 3. Fit Model ---
#' des <- svydesign(id = ~psu, strata = ~strata, weights = ~survey_weight,
#'                  data = mort_data, nest = TRUE)
#'
#' # Interaction: age * log_cal
#' # Adjustments: log_carb, sex, race
#' model_age_cal <- svycoxph(Surv(stime, status) ~ age * log_cal + log_carb + sex + race,
#'                           design = des)
#'
#' # --- 4. CALL THE PLOT FUNCTION ---
#' # We will plot the effect of Age (x-axis) at different levels of Calories
#' plot_data <- plotint(
#'   model = model_age_cal,
#'   effect = "age",
#'   moderator = "log_cal",
#'   data = mort_data,
#'   mod_scale = "sd", # Use Mean +/- 1 SD for log_cal
#'   type = "link",
#'   show_ci = TRUE,
#'   bw = FALSE,
#'   pval_position = "bottom.right",
#'   xlab = "Age (Years)",
#'   ylab = "Predicted log(Hazard) of Mortality",
#'   legend_title = "log(Total Calories)"
#' )
#'
#' # You can now access the plot data if you want it
#' # head(plot_data$plot_data)
#'
#'
#' # =================================================================
#' # Example 2: Continuous x Continuous (NHANESraw)
#' # =================================================================
#' library(NHANES)
#' library(dplyr)
#' library(survey)
#' library(emmeans)
#' library(ggplot2)
#'
#' # --- 3. DATA PREPARATION (NHANESraw) ---
#' data("NHANESraw", package = "NHANES")
#'
#' # Select relevant variables and filter for adults with complete cases
#' nhanes_clean <- NHANESraw %>%
#'   filter(Age >= 20) %>%
#'   mutate(
#'     # Ensure factors are set correctly
#'     Gender = factor(Gender),
#'     Race1 = factor(Race1)
#'   ) %>%
#'   # Keep only the variables we need for this model
#'   select(
#'     BPSysAve, Age, BMI, Gender, Race1,
#'     SDMVPSU, SDMVSTRA, WTMEC2YR
#'   ) %>%
#'   na.omit() # Listwise deletion for simplicity
#'
#' # --- 4. DESIGN & MODEL FITTING ---
#' # Create the survey design object
#' design_nhanes <- svydesign(
#'   id = ~SDMVPSU,
#'   strata = ~SDMVSTRA,
#'   weights = ~WTMEC2YR,
#'   nest = TRUE,
#'   data = nhanes_clean
#' )
#'
#' # Fit the model: Interaction of Age * BMI on Geometric Mean of BP
#' # We use gaussian(link="log") to model Geometric Means
#' model_bp_interaction <- svyglm(
#'   BPSysAve ~ Age * BMI + Gender + Race1,
#'   design = design_nhanes,
#'   family = gaussian(link = "log")
#' )
#'
#' # --- 5. PLOT THE INTERACTION ---
#'
#' # --- Plot 1: Standard Plot (using quantiles and CIs) ---
#' # This shows the effect of Age (x-axis) at 10th/50th/90th percentiles of BMI
#' print(
#'   plotint(
#'     model = model_bp_interaction,
#'     effect = "Age",
#'     moderator = "BMI",
#'     data = nhanes_clean,
#'     mod_scale = "quantile",       # Use 10th/50th/90th percentiles for BMI
#'     type = "response",          # Plot the Geometric Mean (back-transform from log)
#'     show_ci = TRUE,
#'     bw = FALSE,
#'     pval_position = "bottom.right",
#'     xlab = "Age (Years)",
#'     ylab = "Predicted Geometric Mean SBP (mmHg)",
#'     legend_title = "BMI (Percentiles)"
#'   )
#' )
#'
#' # --- Plot 2: Cleaner Plot (using SD and CIs) ---
#' # This shows the effect of Age at Mean +/- 1 SD of BMI
#' # We turn off CIs for a cleaner look if they are too wide
#' print(
#'   plotint(
#'     model = model_bp_interaction,
#'     effect = "Age",
#'     moderator = "BMI",
#'     data = nhanes_clean,
#'     mod_scale = "sd",             # Use Mean +/- 1 SD for BMI
#'     type = "response",
#'     show_ci = FALSE,            # Turn off CI ribbons
#'     bw = TRUE,                  # Use B&W linetypes
#'     pval_position = "bottom.right",
#'     xlab = "Age (Years)",
#'     ylab = "Predicted Geometric Mean SBP (mmHg)",
#'     legend_title = "BMI (Standard Deviations)"
#'   )
#' )
#' }
plotint <- function(model,
                    effect,
                    moderator,
                    data,
                    mod_scale = "sd",
                    type = "link",
                    show_ci = TRUE,
                    bw = FALSE,
                    xlab = NULL,
                    ylab = NULL,
                    legend_title = NULL,
                    pval_position = "none") {

  # --- 1. Input Validation & Setup ---
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop("Package 'emmeans' is required for this function.")
  }
  if (!mod_scale %in% c("sd", "quantile")) {
    stop("`mod_scale` must be one of 'sd' or 'quantile'.")
  }
  if (!pval_position %in% c("bottom.right", "bottom.left", "top.right", "top.left", "none")) {
    stop("`pval_position` must be one of 'bottom.right', 'bottom.left', 'top.right', 'top.left', or 'none'.")
  }

  is_cont_effect <- is.numeric(data[[effect]])
  is_cont_mod <- is.numeric(data[[moderator]])

  # --- 2. Build 'at' list for emmeans ---
  at_list <- list()
  mod_labels <- NULL

  if (is_cont_mod) {
    mod_vals <- data[[moderator]]
    if (mod_scale == "sd") {
      m <- mean(mod_vals, na.rm = TRUE)
      s <- sd(mod_vals, na.rm = TRUE)
      vals <- c(m - s, m, m + s)
      mod_labels <- c(paste0("-1 SD (", round(vals[1], 2), ")"),
                      paste0("Mean (", round(vals[2], 2), ")"),
                      paste0("+1 SD (", round(vals[3], 2), ")"))
    } else { # "quantile"
      vals <- stats::quantile(mod_vals, probs = c(0.10, 0.50, 0.90), na.rm = TRUE)
      mod_labels <- c(paste0("10th %ile (", round(vals[1], 2), ")"),
                      paste0("50th %ile (", round(vals[2], 2), ")"),
                      paste0("90th %ile (", round(vals[3], 2), ")"))
    }
    at_list[[moderator]] <- vals
  }

  if (is_cont_effect) {
    at_list[[effect]] <- seq(min(data[[effect]], na.rm = TRUE),
                             max(data[[effect]], na.rm = TRUE),
                             length.out = 100)
  }

  # --- 3. Get Plot Data from emmeans ---
  if (is_cont_effect) {
    # Continuous X-axis: Use emmip to get plot data
    emm_formula <- as.formula(paste(moderator, "~", effect))
    plot_data <- emmeans::emmip(model, emm_formula,
                                at = at_list, CIs = TRUE,
                                plotit = FALSE, data = data, type = type)

    # Convert complex emmGrid to a simple data.frame
    plot_data <- as.data.frame(plot_data)

    # Rename for ggplot
    names(plot_data)[names(plot_data) == "yvar"] <- "y_value"
    names(plot_data)[names(plot_data) == "LCL"] <- "ymin"
    names(plot_data)[names(plot_data) == "UCL"] <- "ymax"

  } else {
    # Categorical X-axis: Use emmeans to get points
    emm_formula <- as.formula(paste("~", effect, "|", moderator))
    emm_grid <- emmeans::emmeans(model, specs = emm_formula,
                                 data = data, at = at_list, type = type)
    plot_data <- as.data.frame(stats::confint(emm_grid))

    # Rename for ggplot (names vary by model/CI type)
    est_col <- names(plot_data)[grepl("estimate|ratio|emmean", names(plot_data))][1]
    low_col <- names(plot_data)[grepl("lower|LCL", names(plot_data))][1]
    high_col <- names(plot_data)[grepl("upper|UCL", names(plot_data))][1]

    names(plot_data)[names(plot_data) == est_col] <- "y_value"
    names(plot_data)[names(plot_data) == low_col] <- "ymin"
    names(plot_data)[names(plot_data) == high_col] <- "ymax"
  }

  if (!is.null(mod_labels)) {
    plot_data[[moderator]] <- factor(plot_data[[moderator]],
                                     levels = vals,
                                     labels = mod_labels)
  }

  # --- 4. Get Interaction P-value ---
  pval_text <- ""
  if (pval_position != "none") {
    jt <- emmeans::joint_tests(model, data = data)
    inter_pattern <- paste0(effect, ":", moderator, "|", moderator, ":", effect)
    inter_row <- jt[grepl(inter_pattern, jt$`model term`), ]

    if (nrow(inter_row) == 1) {
      pval <- inter_row$p.value
      pval_text <- if(pval < 0.001) "Interaction P < 0.001" else paste("Interaction P =", round(pval, 3))
    } else {
      pval_text <- "Interaction P N/A"
    }
  }

  # --- 5. Build Plot ---
  `%||%` <- function(a, b) if (is.null(a)) b else a
  xlab <- xlab %||% effect
  ylab <- ylab %||% if(type=="link") "Predicted (Link Scale)" else "Predicted (Response Scale)"
  legend_title <- legend_title %||% moderator

  y_value <- ymin <- ymax <- NULL

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[effect]], y = .data$y_value,
                                               group = .data[[moderator]]))

  if (is_cont_effect) {
    if (bw) {
      if(show_ci) p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$ymin, ymax = .data$ymax), fill = "grey70", alpha = 0.3)
      p <- p + ggplot2::geom_line(ggplot2::aes(linetype = .data[[moderator]]), linewidth = 1.2)
    } else {
      if(show_ci) p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$ymin, ymax = .data$ymax, fill = .data[[moderator]]), alpha = 0.2, color = NA)
      p <- p + ggplot2::geom_line(ggplot2::aes(color = .data[[moderator]]), linewidth = 1.2)
    }
  } else {
    dodge <- ggplot2::position_dodge(width = 0.3)
    if (bw) {
      p <- p + ggplot2::geom_pointrange(ggplot2::aes(ymin = .data$ymin, ymax = .data$ymax, linetype = .data[[moderator]]),
                                        position = dodge, linewidth = 1)
    } else {
      p <- p + ggplot2::geom_pointrange(ggplot2::aes(ymin = .data$ymin, ymax = .data$ymax, color = .data[[moderator]]),
                                        position = dodge, linewidth = 1)
    }
  }

  # --- 6. Apply Aesthetics ---
  p <- p + ggplot2::labs(x = xlab, y = ylab, title = NULL) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      plot.caption = ggplot2::element_text(hjust = 0, size = 10)
    )

  if (bw) {
    p <- p + ggplot2::scale_color_grey(name = legend_title) +
      ggplot2::scale_linetype_discrete(name = legend_title)
  } else {
    p <- p + ggplot2::scale_color_brewer(palette = "Dark2", name = legend_title) +
      ggplot2::scale_fill_brewer(palette = "Dark2", name = legend_title)
  }

  # --- 7. Add P-value Annotation ---
  if (pval_position != "none") {
    # Set coordinates to corners
    x_pos <- ifelse(grepl("left", pval_position), -Inf, Inf)
    y_pos <- ifelse(grepl("bottom", pval_position), -Inf, Inf)

    # Set justification *outside* 0-1 range to create a margin
    h_just <- ifelse(grepl("left", pval_position), -0.2, 1.2)
    v_just <- ifelse(grepl("bottom", pval_position), -0.5, 1.5)

    p <- p + ggplot2::annotate("text", x = x_pos, y = y_pos, label = pval_text,
                               hjust = h_just, vjust = v_just, size = 4)
  }

  # --- 8. Final Output ---
  print(p)

  invisible(
    list(
      plot_data = plot_data,
      p_value_text = pval_text
    )
  )
}
