# svyTable1: Create Publication-Ready Survey-Weighted Summary Tables

<!-- badges: start -->
<!-- badges: end -->

**svyTable1** is a lightweight R package containing a single, focused function: `svytable1()`.  
Its purpose is to generate publication-ready *“Table 1”* descriptive statistics from complex survey data, integrating seamlessly with the widely-used **survey** package.

The package was developed to simplify a common task in epidemiology and public health research — presenting stratified summary statistics that correctly account for survey design features such as **weights, strata, and clusters**, while following best practices for transparency and readability.

---

## ✨ Key Features

- **Survey Design Integration:** Works directly with `svydesign` objects from the R **survey** package.  
- **Best Practice Reporting:** Defaults to the *“mixed mode”* display, showing unweighted sample sizes (N) alongside weighted percentages (%) or means. This provides a transparent view of both the actual sample and the target population.  
- **Flexible Output Modes:** Easily switch between `"mixed"`, `"weighted"`, and `"unweighted"` summaries depending on your reporting needs.  
- **Readability:** Option to format large numbers with commas for improved readability.  
- **Robust Error Handling:** Detects and reports common survey issues such as *lonely PSUs* (strata with only one primary sampling unit).

---

## 🧩 Installation

You can install the development version of **svyTable1** from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("ehsanx/svyTable1")
```

---

## 🧠 Usage Example

Below is a complete example showing how to create a stratified survey-weighted summary table using data from the **National Health and Nutrition Examination Survey (NHANES)**.

### 1. Load Libraries and Prepare Data

```r
library(svyTable1)
library(survey)
library(dplyr)

# Load example NHANES data
load(url("https://raw.githubusercontent.com/ehsanx/EpiMethods/main/Data/surveydata/cholesterolNHANES15part1.RData"))

# Prepare the data
anadata <- select(analytic, cholesterol, gender, age, born, race, education, 
                  married, income, bmi, diabetes, weight, psu, strata) %>%
  mutate(
    cholesterol.bin = factor(ifelse(cholesterol < 200, "healthy", "unhealthy"),
                             levels = c("healthy", "unhealthy")),
    across(c(gender, born, race, education, married, income, diabetes), as.factor)
  )

# Create the survey design object
w.design <- svydesign(id = ~psu, weights = ~weight, strata = ~strata,
                      nest = TRUE, data = anadata)
```

---

### 2. Generate the Table

```r
# Define variables to summarize
vars_for_table <- c("gender", "born", "race", "education", "married", 
                    "income", "bmi", "diabetes", "age")

# Generate the summary table (default mode = "mixed")
my_table <- svytable1(design = w.design, 
                      strata_var = "cholesterol.bin", 
                      table_vars = vars_for_table)

# Display table with nice formatting
knitr::kable(my_table, caption = "Table 1: Participant Characteristics by Cholesterol Status")
```

---

## 📊 Example Output

| Variable | Level | Overall | healthy | unhealthy |
|-----------|--------|----------|----------|------------|
| n |  | 1,267 | 738 | 529 |
| gender | Female | 496 (44.4%) | 269 (40.2%) | 227 (49.6%) |
|  | Male | 771 (55.6%) | 469 (59.8%) | 302 (50.4%) |
| race | Black | 246 (8.9%) | 164 (10.7%) | 82 (6.7%) |
|  | Hispanic | 337 (11.6%) | 183 (11.3%) | 154 (11.9%) |
| ... | ... | ... | ... | ... |
| age | Mean (SD) | 48.94 (16.35) | 47.09 (17.64) | 51.29 (14.23) |

> Example output illustrating the mixed-mode summary (Unweighted N, Weighted %).

---

## 🤝 Contributing

Contributions are welcome!  
If you find a bug or have suggestions for improvements, please open an issue or submit a pull request on the GitHub repository.

---

## 📜 License

This package is released under the **MIT License**.
