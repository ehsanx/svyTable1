# Higher-level interaction reporting on the bundled survey design.

test_that("jointeffects returns one row per factor-level combination", {
  je <- jointeffects(st1_int_fit(), "sex", "insulin")
  expect_s3_class(je, "tbl_df")
  expect_true(all(c("Level1", "Level2", "Estimate", "SE", "CI.low", "CI.upp") %in%
                    names(je)))
  expect_equal(nrow(je), 4) # 2 x 2 factor combinations
})

test_that("addintlist returns the three additive-interaction measures", {
  al <- addintlist(model = st1_int_fit(), factor1_name = "sex",
                   factor2_name = "insulin", measures = "all")
  expect_s3_class(al, "tbl_df")
  expect_true(all(c("Factor1", "Level1", "Factor2", "Level2", "Measure",
                    "Estimate", "SE", "CI_low", "CI_upp") %in% names(al)))
  expect_setequal(unique(al$Measure), c("RERI", "AP", "S"))
})

test_that("addintlist labels the effect-measure scale (OR for logistic)", {
  al <- addintlist(model = st1_int_fit(), factor1_name = "sex",
                   factor2_name = "insulin", measures = "all")
  expect_true("Scale" %in% names(al))
  expect_equal(unique(al$Scale), "OR (logistic)")
})

test_that("reportint(output = 'list') returns the documented panels", {
  fit <- st1_int_fit()
  suppressMessages(invisible(utils::capture.output(
    rl <- reportint(fit, factor1_name = "sex", factor2_name = "insulin",
                    output = "list")
  )))
  expect_type(rl, "list")
  expect_true(all(c("joint_effects", "stratum_specific_effects",
                    "additive_interaction", "multiplicative_scale",
                    "effect_modification_report", "Interaction_report") %in%
                    names(rl)))
  expect_s3_class(rl$joint_effects, "data.frame")
})

test_that("reportint auto-detects a single interaction term", {
  fit <- st1_int_fit()
  suppressMessages(invisible(utils::capture.output(
    rl <- reportint(fit, output = "list")
  )))
  expect_type(rl, "list")
})

test_that("reportint errors when no interaction term is present", {
  des <- st1_design()
  fit <- survey::svyglm(htn01 ~ sex + insulin + age, design = des,
                        family = stats::quasibinomial())
  expect_error(
    suppressMessages(reportint(fit, output = "list")),
    "interaction"
  )
})
