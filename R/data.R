#' Processed NHANES Mortality Data for Diabetic Adults
#'
#' A subset of the NHANES 1999-2010 data, filtered for adults (>=18)
#' with diabetes and plausible caloric intake (500-3500 kcal).
#' This data is prepared for survival analysis on the relationship
#' between caffeine consumption and mortality.
#'
#' @format A data frame with 3780 rows and 23 variables:
#' \describe{
#'   \item{id}{Respondent sequence/ID number}
#'   \item{survey.weight}{Full sample 2-year weights}
#'   \item{psu}{Masked pseudo-PSU}
#'   \item{strata}{Masked pseudo-stratum}
#'   \item{caff}{Caffeine consumption (Factor: "No consumption", "<100 mg/day", etc.)}
#'   \item{stime}{Follow-up time in months}
#'   \item{status}{Mortality status (0=Censored, 1=Deceased)}
#'   \item{sex}{Sex (Factor: "Male", "Female")}
#'   \item{age}{Age in years at interview}
#'   \item{race}{Race/ethnicity (Factor)}
#'   \item{smoking}{Smoking status (Factor)}
#'   \item{bmi.cat}{BMI category (Factor)}
#'   \item{education}{Education level (Factor)}
#'   \item{carbohyd}{Carbohydrate in gm}
#'   \item{alcohol}{Alcohol consumption (Factor)}
#'   \item{htn}{Hypertension status (Factor)}
#'   \item{macrovascular}{Macrovascular complications (Character: "No", "Yes")}
#'   \item{insulin}{Insulin use (Factor: "No", "Yes")}
#'   \item{survey.cycle}{Survey cycle (Character: "1999-00", etc.)}
#'   \item{physical.activity}{Physical activity level (Factor)}
#'   \item{diabetes}{Diabetes status (Factor, all "Yes")}
#'   \item{cal.total}{Total calories in kcal}
#'   \item{survey_weight}{Adjusted 6-cycle survey weight (survey.weight / 6)}
#' }
#' @source Data originally from NHANES and processed according to the
#' "Survival analysis: NHANES" tutorial.
"nhanes_mortality"
