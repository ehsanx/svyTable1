# svydiag --------------------------------------------------------------------

test_that("svydiag returns a tibble with the documented columns", {
  out <- svydiag(st1_fit())
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("Term", "Estimate", "SE", "p.value", "is_significant",
                    "CI_Lower", "CI_Upper", "CI_Width", "RSE_percent",
                    "is_rse_high") %in% names(out)))
  expect_type(out$is_significant, "logical")
  expect_true(all(out$CI_Width >= 0))
})

test_that("svydiag honours custom thresholds", {
  out <- svydiag(st1_fit(), p_threshold = 1)
  expect_true(all(out$is_significant))
})

# svygof ---------------------------------------------------------------------

test_that("svygof returns a one-row F-test on a standard design", {
  des <- st1_design()
  g <- svygof(st1_fit(des), des, G = 10)
  expect_s3_class(g, "data.frame")
  expect_equal(nrow(g), 1)
  expect_named(g, c("F_statistic", "df1", "df2", "p_value"))
  expect_gte(g$p_value, 0)
  expect_lte(g$p_value, 1)
})

test_that("svygof works on a replicate design (regression: it used to error)", {
  des <- st1_design()
  rep_des <- survey::as.svrepdesign(des)
  fit <- survey::svyglm(htn01 ~ age + sex + smoking, design = rep_des,
                        family = stats::quasibinomial())
  g <- svygof(fit, rep_des, G = 10)
  expect_s3_class(g, "data.frame")
  expect_gte(g$p_value, 0)
  expect_lte(g$p_value, 1)
})

test_that("svygof rejects a non-svyglm fit", {
  expect_error(svygof(stats::lm(mpg ~ wt, data = mtcars), st1_design()), "svyglm")
})

# svyAUC ---------------------------------------------------------------------

test_that("svyAUC returns a CI bounded within [0, 1]", {
  des <- st1_design()
  rep_des <- survey::as.svrepdesign(des)
  fit <- survey::svyglm(htn01 ~ age + sex + smoking, design = rep_des,
                        family = stats::quasibinomial())
  a <- svyAUC(fit, rep_des)
  expect_s3_class(a, "data.frame")
  expect_named(a, c("AUC", "SE", "CI_Lower", "CI_Upper"))
  expect_gte(a$CI_Lower, 0)
  expect_lte(a$CI_Upper, 1)
  expect_true(a$AUC >= a$CI_Lower && a$AUC <= a$CI_Upper)
})

test_that("svyAUC requires a replicate-weights design", {
  des <- st1_design()
  expect_error(svyAUC(st1_fit(des), des), "replicate")
})
