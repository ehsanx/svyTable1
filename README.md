
# svyTable1: Create Publication-Ready Survey-Weighted Summary Tables

**svyTable1** is a lightweight R package containing a single, focused
function: `svytable1()`.  
Its purpose is to generate publication-ready *“Table 1”* descriptive
statistics from complex survey data, integrating seamlessly with the
widely-used **survey** package.

The package was developed to simplify a common task in epidemiology and
public health research — presenting stratified summary statistics that
correctly account for survey design features such as **weights, strata,
and clusters**, while following best practices for transparency and
readability.

------------------------------------------------------------------------

## ✨ Key Features

- **Survey Design Integration:** Works directly with `svydesign` objects
  from the R **survey** package.
- **Automatic Missing Data Handling:** Detects and reports missing
  values for each variable, either as a “Missing” category for factors
  or a separate “Missing, n (%)” row for numeric variables.
- **Best Practice Reporting:** Defaults to the *“mixed mode”* display,
  showing unweighted sample sizes (N) alongside weighted percentages (%)
  or means. This provides a transparent view of both the actual sample
  and the target population.
- **Flexible Output Modes:** Easily switch between `"mixed"`,
  `"weighted"`, and `"unweighted"` summaries depending on your reporting
  needs.
- **Readability:** Option to format large numbers with commas for
  improved readability.

------------------------------------------------------------------------

## 🧩 Installation

You can install the development version of **svyTable1** from GitHub
with:

``` r
# install.packages("devtools")
# Downloads from GitHub and installs, building vignettes
devtools::install_github("ehsanx/svyTable1", build_vignettes = TRUE)
```

------------------------------------------------------------------------

## 🧠 Usage Examples

Below are two examples using data from the **National Health and
Nutrition Examination Survey (NHANES)**.

### 1. Data Preparation

First, we load the necessary libraries and the `NHANESraw` dataset.
We’ll filter for adults and create our stratification variable,
`ObeseStatus`.

``` r
library(svyTable1)
library(survey)
library(dplyr)
library(NHANES)

# Load the raw NHANES data (2009-2012)
data(NHANESraw)

# Prepare data for adults, keeping NAs to demonstrate missing data handling
nhanes_adults_with_na <- NHANESraw %>%
  filter(Age >= 20) %>%
  mutate(
    ObeseStatus = factor(ifelse(BMI >= 30, "Obese", "Not Obese"),
                         levels = c("Not Obese", "Obese"))
  )

# Create the survey design object
adult_design_with_na <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTMEC2YR,
  nest = TRUE,
  data = nhanes_adults_with_na
)
```

### 2. Generate Tables

#### Example A: Handling Variables with Missing Data (Primary Use Case)

This example shows how `svytable1` automatically handles and reports
missing values for each variable independently.

``` r
# Define variables, some with expected missing values
vars_with_missing <- c("Age", "Race1", "Education", "TotChol", "SmokeNow")

# Generate the summary table
table_with_missing <- svytable1(
  design = adult_design_with_na, 
  strata_var = "ObeseStatus", 
  table_vars = vars_with_missing
)

# Display table with nice formatting
knitr::kable(table_with_missing, caption = "Table 1: Participant Characteristics (with Missing Data)")
```

#### Example B: Summarizing Complete Data (No Missing Values)

If your analysis requires a dataset with no missing values, you can
perform a complete-case analysis using `tidyr::drop_na()` before
generating your table.

``` r
library(tidyr) # For drop_na()

# Define variables for a complete-case analysis
vars_for_complete_table <- c("Age", "Race1", "BPSysAve", "Pulse", "BMI")

# Create a new, complete-case data frame
nhanes_adults_complete <- nhanes_adults_with_na %>%
  drop_na(all_of(vars_for_complete_table))

# Create a new design object based on this complete data
adult_design_complete <- svydesign(
  id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR,
  nest = TRUE, data = nhanes_adults_complete
)

# Generate the table
table_without_missing <- svytable1(
  design = adult_design_complete, 
  strata_var = "ObeseStatus", 
  table_vars = c("Age", "Race1", "BPSysAve", "Pulse")
)

# Display table with nice formatting
knitr::kable(table_without_missing, caption = "Table 2: Participant Characteristics (No Missing Data)")
```

------------------------------------------------------------------------

## 📊 Example Output

Running `Example A` above will produce a table like the following
snippet. Note the automatic inclusion of a “Missing” column for the
stratification variable and “Missing” rows for summary variables.

| Variable | Level | Overall | Missing | Not Obese | Obese |
|:---|:---|:---|:---|:---|:---|
| n |  | 11,778 | 547 | 7,073 | 4,158 |
| Age | Mean (SD) | 47.18 (16.89) | 56.29 (19.15) | 46.45 (17.32) | 48.25 (15.87) |
| Race1 | Black | 2,577 (11.4%) | 108 (12.1%) | 1,296 (9.1%) | 1,173 (15.8%) |
|  | Hispanic | 1,210 (5.8%) | 62 (2.9%) | 714 (5.7%) | 434 (6.0%) |
|  | … | … | … | … | … |
| TotChol | Mean (SD) | 5.07 (1.07) | 5.00 (1.42) | 5.07 (1.08) | 5.06 (1.04) |
|  | Missing, n (%) | 1,169 (5.6%) | 426 (15.5%) | 480 (5.5%) | 263 (5.4%) |
| SmokeNow | No | 2,779 (24.2%) | 142 (29.1%) | 1,580 (23.2%) | 1,057 (26.0%) |
|  | Yes | 2,454 (20.1%) | 102 (19.5%) | 1,594 (21.4%) | 758 (17.6%) |
|  | Missing | 6,545 (55.7%) | 303 (51.4%) | 3,899 (55.4%) | 2,343 (56.4%) |

> Example output illustrating the mixed-mode summary and automatic
> handling of missing data.

------------------------------------------------------------------------

## 🤝 Contributing

Contributions are welcome!  
If you find a bug or have suggestions for improvements, please open an
issue or submit a pull request on the GitHub repository.

------------------------------------------------------------------------

## 📜 License

This package is released under the **MIT License**.
