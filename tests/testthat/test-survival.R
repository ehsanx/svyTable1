# svykmplot ------------------------------------------------------------------

test_that("svykmplot returns a plot and an at-risk table", {
  des <- st1_design()
  des_f <- subset(des, sex == "Female")
  # se = FALSE keeps this fast; the confidence-band path (se = TRUE) is the
  # long-standing default and is exercised separately below on a small subset.
  km <- svykmplot(survival::Surv(stime, status) ~ caff, design = des_f,
                  time_unit = "days", time_breaks = seq(0, 240, by = 60),
                  show_pval = TRUE, se = FALSE)
  expect_type(km, "list")
  expect_true(all(c("plot", "table") %in% names(km)))
  expect_s3_class(km$plot, "ggplot")
})

test_that("svykmplot draws confidence bands when se = TRUE", {
  des <- st1_design()
  # A small subgroup keeps the (slow) svykm SE computation quick.
  des_small <- subset(des, sex == "Female" & race == "Mexican American")
  km <- svykmplot(survival::Surv(stime, status) ~ caff, design = des_small,
                  time_unit = "days", time_breaks = seq(0, 240, by = 60),
                  show_pval = FALSE, se = TRUE)
  expect_s3_class(km$plot, "ggplot")
})

# svycoxph_CE ----------------------------------------------------------------

test_that("svycoxph_CE returns a ggplot constant-effect diagnostic", {
  des <- st1_design()
  suppressMessages(invisible(utils::capture.output(
    p <- svycoxph_CE(formula_rhs = "sex + age", design = des,
                     var_to_test = "sexFemale", time_var = "stime",
                     status_var = "status", n_intervals = 3,
                     print_main_model = FALSE, print_split_summary = FALSE)
  )))
  expect_s3_class(p, "ggplot")
})
