# svyTable1: Survey-Weighted Tables and Diagnostics for Epidemiology

**svyTable1** is a teaching/helper R package for the UBC SPPH 604 course and the
[EpiMethods](https://ehsanx.github.io/EpiMethods/) book. It helps students build
publication-ready tables and diagnostics from complex survey data (such as
NHANES): descriptive "Table 1" summaries, survey-weighted regression and survival
diagnostics, additive-interaction reporting, and pooled multiply-imputed models.

All examples below use the bundled `nhanes_mortality` dataset, so they run without
any additional downloads.

---

## ✨ Key Features

- **Built on the `survey` package** — works with `svydesign` objects.
- **Guided design construction** — `svydesign_build()` validates inputs and does
  subpopulation analysis correctly (subset, not row pre-filtering).
- **Descriptive tables** — `svytable1()` with unweighted *n* + weighted %, automatic
  missing-data handling, and optional NCHS reliability suppression.
- **Regression diagnostics** — `svydiag()` (coefficient reliability), `svygof()`
  (Archer–Lemeshow goodness-of-fit), `svyAUC()` (design-correct AUC).
- **Interaction analysis** — additive interaction (`addint()`, `addintlist()`),
  joint effects (`jointeffects()`), simple effects (`inteffects()`), a multi-panel
  report (`reportint()`), and interaction plots (`plotint()`).
- **Survival analysis** — survey-weighted Kaplan–Meier plots with number-at-risk
  (`svykmplot()`) and constant-effect / proportional-hazards diagnostics
  (`svycoxph_CE()`, `svycoxph_CE_mi()`).
- **Multiple imputation** — fallacy-safe pooled tables from `mice` (`svypooled()`).

---

## 🧩 Installation

```r
# install.packages("devtools")
devtools::install_github("ehsanx/svyTable1", build_vignettes = TRUE, dependencies = TRUE)
```

---

## 🚀 Quick start

```r
library(svyTable1)
library(survey)

options(survey.lonely.psu = "adjust")   # correct SEs for NHANES-style designs

data(nhanes_mortality)
nhanes_mortality$htn01 <- as.numeric(nhanes_mortality$htn == "Yes")

design <- svydesign(
  id = ~psu, strata = ~strata, weights = ~survey_weight,
  nest = TRUE, data = nhanes_mortality
)
```

`svydesign_build()` is a friendlier alternative that takes plain column names,
validates the inputs, and does subpopulation analysis the **correct** way
(subsetting the design instead of pre-filtering rows, which would break the
standard errors):

```r
design <- svydesign_build(
  data = nhanes_mortality,
  ids = "psu", strata = "strata", weights = "survey_weight"
)

# Subpopulation (e.g. women only) done correctly:
design_women <- svydesign_build(
  nhanes_mortality, ids = "psu", strata = "strata",
  weights = "survey_weight", subpop = "sex == 'Female'"
)
```

### Descriptive "Table 1"

```r
svytable1(
  design     = design,
  strata_var = "htn",
  table_vars = c("age", "sex", "smoking", "bmi.cat")
)

# With NCHS reliability checks and a detailed metrics table:
svytable1(
  design     = design,
  strata_var = "htn",
  table_vars = c("age", "sex", "smoking", "bmi.cat"),
  reliability_checks = TRUE,
  return_metrics     = TRUE
)
```

---

## 🔍 Regression diagnostics

```r
fit <- svyglm(htn01 ~ age + sex + smoking, design = design, family = quasibinomial())

# Per-coefficient reliability (SE, p, CI width, RSE)
svydiag(fit)

# Archer-Lemeshow goodness-of-fit (use a standard, non-replicate design)
svygof(fit, design)

# Design-correct AUC (needs a replicate-weights design); CI is on the logit scale
rep_design <- as.svrepdesign(design)
fit_rep <- svyglm(htn01 ~ age + sex + smoking, design = rep_design,
                  family = quasibinomial())
svyAUC(fit_rep, rep_design)
```

---

## 🔗 Interaction analysis

```r
int_fit <- svyglm(htn01 ~ sex * insulin + age, design = design,
                  family = quasibinomial())

# Additive interaction measures (RERI, AP, Synergy index) for every level pair
addintlist(int_fit, factor1_name = "sex", factor2_name = "insulin", measures = "all")

# Joint effects of the two factors
jointeffects(int_fit, "sex", "insulin")

# Multi-panel interaction report (joint, stratum-specific, additive, multiplicative)
report <- reportint(int_fit, factor1_name = "sex", factor2_name = "insulin",
                    output = "list")
report$Interaction_report
```

> **Note on scale.** `addint()`/`addintlist()` compute RERI/AP/S by exponentiating
> the model coefficients. For logistic models these are odds ratios, which only
> approximate risk ratios when the outcome is rare. Interpret additive-interaction
> measures on the risk-ratio scale with care for common outcomes.

Plotting an interaction with a continuous moderator:

```r
int_fit2 <- svyglm(htn01 ~ insulin * age, design = design, family = quasibinomial())

plotint(model = int_fit2, effect = "insulin", moderator = "age",
        data = nhanes_mortality)
```

---

## ⏳ Survival analysis

```r
library(survival)

# Survey-weighted Kaplan-Meier curves with a number-at-risk table.
# se = TRUE adds confidence bands but is slow on large designs; use se = FALSE
# to draw curves only.
km <- svykmplot(
  formula     = Surv(stime, status) ~ caff,
  design      = subset(design, sex == "Female"),
  time_unit   = "days",
  time_breaks = seq(0, 240, by = 60),
  show_pval   = TRUE,
  se          = FALSE
)
km$plot
print(km$table)

# Constant-effect / proportional-hazards diagnostic for a Cox model
svycoxph_CE(
  formula_rhs = "sex + age",
  design      = design,
  var_to_test = "age",
  time_var    = "stime",
  status_var  = "status",
  n_intervals = 3
)
```

A multiple-imputation version of the Cox diagnostic
(`svycoxph_CE_mi()`) and a fallacy-safe pooled table (`svypooled()`) are
demonstrated in `?svycoxph_CE_mi`, `?svypooled`, and the package vignettes.

---

## 📚 Learn more

```r
browseVignettes("svyTable1")
```

The vignettes follow the SPPH 604 / EpiMethods workflow: descriptive tables →
model diagnostics → interaction analysis → survival analysis → multiple imputation.

---

## 🤝 Contributing

Contributions and bug reports are welcome via the
[issue tracker](https://github.com/ehsanx/svyTable1/issues).

## 📜 License

MIT License.
