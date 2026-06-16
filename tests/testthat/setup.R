# Survey SEs for NHANES-style designs require an explicit lonely-PSU policy.
# Tests run in their own process, so setting it here does not leak to the user.
options(survey.lonely.psu = "adjust")
