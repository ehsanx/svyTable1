test_that("svytable1 returns a data.frame with the expected structure", {
  des <- st1_design()
  tab <- svytable1(des, strata_var = "htn", table_vars = c("age", "sex", "smoking"))
  expect_s3_class(tab, "data.frame")
  expect_true(all(c("Variable", "Level", "Overall", "No", "Yes") %in% names(tab)))
  expect_equal(tab$Variable[1], "n")
})

test_that("svytable1 mixed-mode N row reports unweighted sample sizes", {
  d <- st1_data()
  des <- st1_design(d)
  tab <- svytable1(des, "htn", "age")
  # Default "mixed" mode: the Overall n is the real (unweighted) row count.
  expect_equal(tab$Overall[1], format(nrow(d), big.mark = ","))
})

test_that("svytable1 errors on variables not in the design", {
  des <- st1_design()
  expect_error(svytable1(des, "htn", c("age", "not_a_var")), "not found")
})

test_that("svytable1 errors loudly on a 0-row design", {
  des <- st1_design()
  empty <- subset(des, age > 1e6)
  expect_error(svytable1(empty, "htn", "age"), "0 rows")
})

test_that("svytable1 weighted and unweighted modes run", {
  des <- st1_design()
  expect_s3_class(svytable1(des, "htn", "age", mode = "weighted"), "data.frame")
  expect_s3_class(svytable1(des, "htn", "age", mode = "unweighted"), "data.frame")
})

test_that("svytable1 reliability metrics expose the documented NCHS flags", {
  des <- st1_design()
  res <- svytable1(des, "htn", c("age", "sex", "smoking"),
                   reliability_checks = TRUE, return_metrics = TRUE)
  expect_type(res, "list")
  expect_named(res, c("formatted_table", "reliability_metrics"))
  expect_true(all(c("fail_n_30", "fail_eff_n_30", "fail_df_8", "fail_ciw_30",
                    "fail_rciw_130", "fail_rse_30", "suppressed") %in%
                    names(res$reliability_metrics)))
  expect_type(res$reliability_metrics$suppressed, "logical")
})

test_that("svytable1 suppresses a deliberately tiny, unreliable cell", {
  # Build a tiny stratum so at least one categorical cell fails NCHS rules.
  d <- st1_data()
  des <- st1_design(d)
  res <- svytable1(des, "htn", c("race"),
                   reliability_checks = TRUE, return_metrics = TRUE)
  expect_true(any(res$reliability_metrics$suppressed))
  # A suppressed cell renders as "*" in the formatted table.
  expect_true(any(vapply(res$formatted_table, function(col) any(col == "*"), logical(1))))
})

test_that("svytable1 folds NA in the stratifier into a 'Missing' column", {
  d <- st1_data()
  set.seed(11)
  d$htn[sample(nrow(d), 50)] <- NA
  des <- st1_design(d)
  tab <- svytable1(des, "htn", "age")
  expect_true("Missing" %in% names(tab))
})

test_that("svytable1 reports a Missing row for a numeric variable with NAs", {
  d <- st1_data()
  set.seed(12)
  d$carbohyd[sample(nrow(d), 100)] <- NA
  des <- st1_design(d)
  tab <- svytable1(des, "htn", "carbohyd")
  expect_true(any(grepl("Missing", tab$Level)))
})
