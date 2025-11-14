# svyTable1: Create Publication-Ready Survey-Weighted Summary Tables

**svyTable1** is an R package for creating publication-ready tables and visualizations from complex survey and epidemiologic analyses. It streamlines the creation of descriptive “Table 1” summaries, supports multiply imputed regression models, provides diagnostic and goodness-of-fit tools, enables interaction effect reporting, and supports survey-weighted survival analysis.

---

## ✨ Key Features

- **Survey Design Integration:** Built on `svydesign` objects from the **survey** package.
- **Automatic Missing Data Handling:** Automatically reports missing values for each variable.
- **Best Practice Reporting:** Defaults to “mixed mode” with unweighted Ns and weighted estimates.
- **Reliability Checks:** Implements NCHS Data Presentation Standards for Proportions.
- **Flexible Output Modes:** `"mixed"`, `"weighted"`, and `"unweighted"`.
- **Survival Plotting:** `svykmplot()` for publication-ready survey-weighted Kaplan Meier plots with number at risk.
- **Regression Diagnostics:** `svydiag()` to evaluate coefficient reliability.
- **Goodness-of-Fit:** `svygof()` to perform Archer-Lemeshow GOF tests for survey logistic regression.
- **Design-Correct AUC:** `svyAUC()` for valid AUC estimation under complex survey designs.
- **Multiple Imputation Support:** `svypooled()` generates fallacy-safe pooled regression tables from `mice`.
- **Interaction Analysis:**
  - *Joint effects*: `jointeffects`
  - *Simple effects*: `inteffects`
  - *Additive interaction*: `addint`, `addintlist`
- **Survey Cox Regression with Contrast Estimation:**  
  `svycoxph_CE()` produces hazard ratio contrasts for groups or combinations.
- **MI-Compatible Cox Regression with Contrasts:**  
  `svycoxph_CE_mi()` extends the above to multiply imputed datasets.
- **Plotting Interaction Effects:**  
  `plotint()` generates publication-ready plots for interaction effects estimated with `addint`, `jointeffects`, or `inteffects`.
- **Automated Interaction Reporting:**  
  `reportint()` creates narrative-ready summaries of interaction effects, RERI, AP, and joint effects.

---

## 🧩 Installation

```r
# install.packages("devtools")
devtools::install_github("ehsanx/svyTable1", build_vignettes = TRUE, dependencies = TRUE)
```

---

## 🔍 Usage Examples (Selected)

Below are a few key examples. See vignettes for complete coverage.

---

## Example: Interaction Reporting with reportint()

```r
library(svyTable1)
library(survey)
library(dplyr)

data(NHANESraw)

analytic <- NHANESraw %>%
  filter(Age >= 20) %>%
  mutate(
    Obese = factor(ifelse(BMI >= 30, "Yes", "No")),
    Hypertension = factor(ifelse(BPSysAve >= 130, "Yes", "No"))
  ) %>%
  drop_na(Age, Race1, Obese, Hypertension, SDMVPSU, SDMVSTRA, WTMEC2YR)

design <- svydesign(
  id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR,
  nest = TRUE, data = analytic
)

model_int <- svyglm(
  Hypertension ~ Obese * Race1 + Age,
  design = design,
  family = quasibinomial()
)

reportint(model_int, factor1_name = "Obese", factor2_name = "Race1")
```

---

## Example: Plotting Interaction Effects with plotint()

```r
interaction_results <- addint(
  model = model_int,
  factor1_name = "Obese",
  factor2_name = "Race1",
  measures = "all"
)

plotint(interaction_results, measure = "RERI")
```

---

## Example: Survey Cox Regression with Contrast Estimation

```r
library(survival)

cox_model <- svycoxph(
  Surv(followup_time, death) ~ Obese + Age + Race1,
  design = design
)

svycoxph_CE(
  model = cox_model,
  contrast_var = "Obese",
  ref = "No",
  target = "Yes"
)
```

---

## Example: Cox Contrasts with Multiple Imputation

```r
library(mice)

imp <- mice(analytic, m = 5, seed = 123, printFlag = FALSE)

fit_list <- with(imp, {
  d <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR,
                 nest = TRUE, data = complete(data))
  svycoxph(Surv(followup_time, death) ~ Obese + Age + Race1, design = d)
})

svycoxph_CE_mi(fit_list, contrast_var = "Obese", ref = "No", target = "Yes")
```

---

## Example: Creating Interaction Tables

```r
addintlist(
  model = model_int,
  factor1_name = "Obese",
  factor2_name = "Race1",
  measures = "all"
)
```

---

## 🤝 Contributing

Contributions are welcome.

---

## 📜 License

MIT License.
