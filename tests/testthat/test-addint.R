# Tier 1: pure-math additive interaction on FIXED coef/vcov. These numbers are
# fully deterministic (no survey variance machinery), so they are pinned tightly.

test_that("addint computes RERI/AP/S exactly (interaction parameterisation)", {
  beta <- c(A = 0.4, B = 0.3, `A:B` = 0.5)
  V <- diag(c(0.02, 0.03, 0.05))
  dimnames(V) <- list(names(beta), names(beta))
  m <- make_mock(beta, V)

  res <- addint(m, type = "interaction",
                coef_names = list(exp1_coef = "A", exp2_coef = "B",
                                  inter_coef = "A:B"),
                measures = "all")

  hr10 <- exp(beta[["A"]]); hr01 <- exp(beta[["B"]]); hr11 <- exp(sum(beta))
  reri <- hr11 - hr10 - hr01 + 1
  ap   <- reri / hr11
  s    <- (hr11 - 1) / ((hr10 - 1) + (hr01 - 1))

  expect_equal(unname(res$RERI[["RERI_Estimate"]]), reri, tolerance = 1e-8)
  expect_equal(unname(res$AP[["AP_Estimate"]]),     ap,   tolerance = 1e-8)
  expect_equal(unname(res$S[["S_Estimate"]]),       s,    tolerance = 1e-8)

  # CI is a symmetric Wald interval around the point estimate on its scale.
  z <- stats::qnorm(0.975)
  expect_equal(unname(res$RERI[["RERI_CI95_low"]]),
               reri - z * unname(res$RERI[["RERI_SE"]]), tolerance = 1e-8)
  expect_equal(unname(res$RERI[["RERI_CI95_upp"]]),
               reri + z * unname(res$RERI[["RERI_SE"]]), tolerance = 1e-8)

  # Delta-method SEs are deterministic: pin them as a regression guard.
  expect_equal(unname(res$RERI[["RERI_SE"]]), 0.8570119, tolerance = 1e-5)
  expect_equal(unname(res$AP[["AP_SE"]]),     0.1275353, tolerance = 1e-5)
})

test_that("addint joint and interaction parameterisations give the same point estimate", {
  beta <- c(A = 0.4, B = 0.3, `A:B` = 0.5)
  b_joint <- c(a10 = beta[["A"]], a01 = beta[["B"]], a11 = sum(beta))
  Vj <- diag(c(0.02, 0.03, 0.08))
  dimnames(Vj) <- list(names(b_joint), names(b_joint))
  mj <- make_mock(b_joint, Vj)

  rj <- addint(mj, type = "joint",
               coef_names = list(exp1_level = "a10", exp2_level = "a01",
                                 both_levels = "a11"),
               measures = "RERI")

  hr10 <- exp(0.4); hr01 <- exp(0.3); hr11 <- exp(1.2)
  expect_equal(unname(rj$RERI[["RERI_Estimate"]]),
               hr11 - hr10 - hr01 + 1, tolerance = 1e-8)
})

test_that("addint warns and returns NULL when a required coefficient is missing", {
  beta <- c(A = 0.4, B = 0.3)
  V <- diag(2)
  dimnames(V) <- list(names(beta), names(beta))
  m <- make_mock(beta, V)
  expect_warning(
    res <- addint(m, type = "interaction",
                  coef_names = list(exp1_coef = "A", exp2_coef = "B",
                                    inter_coef = "A:B")),
    "not found"
  )
  expect_null(res)
})

test_that("addint MOVER interval for RERI is valid and asymmetric (Zou 2008)", {
  beta <- c(A = 0.4, B = 0.3, `A:B` = 0.5)
  V <- matrix(c(0.02, 0.005, 0.004,
                0.005, 0.03, 0.006,
                0.004, 0.006, 0.05), 3, 3)
  dimnames(V) <- list(names(beta), names(beta))
  m <- make_mock(beta, V)
  cn <- list(exp1_coef = "A", exp2_coef = "B", inter_coef = "A:B")

  delta <- addint(m, type = "interaction", coef_names = cn, measures = "RERI",
                  ci_method = "delta")$RERI
  mover <- addint(m, type = "interaction", coef_names = cn, measures = "RERI",
                  ci_method = "mover")$RERI

  # Same point estimate; MOVER interval differs from the symmetric Wald one.
  expect_equal(unname(mover[["RERI_Estimate"]]),
               unname(delta[["RERI_Estimate"]]), tolerance = 1e-10)
  expect_lt(mover[["RERI_CI95_low"]], mover[["RERI_Estimate"]])
  expect_gt(mover[["RERI_CI95_upp"]], mover[["RERI_Estimate"]])

  # Asymmetric around the estimate (unlike the delta interval).
  upper_half <- mover[["RERI_CI95_upp"]] - mover[["RERI_Estimate"]]
  lower_half <- mover[["RERI_Estimate"]] - mover[["RERI_CI95_low"]]
  expect_false(isTRUE(all.equal(upper_half, lower_half)))

  # Pinned values, cross-checked against the interactionR package.
  expect_equal(unname(mover[["RERI_CI95_low"]]), 0.3191, tolerance = 1e-3)
  expect_equal(unname(mover[["RERI_CI95_upp"]]), 4.4499, tolerance = 1e-3)
})

test_that("addint validates the measures argument", {
  beta <- c(A = 0.4, B = 0.3, `A:B` = 0.5)
  V <- diag(3)
  dimnames(V) <- list(names(beta), names(beta))
  m <- make_mock(beta, V)
  expect_error(
    addint(m, type = "interaction",
           coef_names = list(exp1_coef = "A", exp2_coef = "B", inter_coef = "A:B"),
           measures = "bogus"),
    "Invalid measure"
  )
})
