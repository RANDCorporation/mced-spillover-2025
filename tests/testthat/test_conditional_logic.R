library(testthat)
library(tidyverse)
library(here)

# Load the actual data
dirPath <- paste0(here("data/"), "/")
dataDirPath <- paste0(dirPath, "cleaned_data/")
covariateDirPath <- paste0(dirPath, "raw_covariates/")
linkingDirPath <- paste0(dirPath, "linking_files/")
resultsDirPath <- paste0(dirPath, "results/")

baseData <- read.csv(paste0(dataDirPath, "base_data.csv"),
                     row.names = 1,
                     stringsAsFactors = FALSE)

# Define cancer groups (from main script)
cancer_groups <- list(
  primary_high_detection = c("Suspected head & neck cancer",
                            "Suspected lung cancer",
                            "Suspected upper gastrointestinal cancer"),
  sec_exp_high_detection = c("Suspected lower gastrointestinal cancer",
                            "Suspected head & neck cancer",
                            "Suspected lung cancer",
                            "Suspected upper gastrointestinal cancer",
                            "Suspected gynaecological cancer",
                            "Suspected haematological malignancies (excluding acute leukaemia)",
                            "Suspected urological malignancies (excluding testicular)"),
  sec_low_detection = c("Suspected skin cancer",
                       "Suspected breast cancer",
                       "Suspected sarcoma",
                       "Suspected brain/central nervous system tumours",
                       "Suspected testicular cancer",
                       "Suspected acute leukaemia",
                       "Suspected other cancer")
)

# ============================================================================
# TEST 1: Period Length and Period Number Calculations
# ============================================================================

test_that("Monthly analysis: periodNum equals monthNum", {
  
  periodLength <- "1m"
  test_data <- data.frame(monthNum = c(1, 2, 3, 6, 7, 8, 12))
  test_data$$periodNum = test_data$$monthNum
  
  expect_equal(test_data$$periodNum, test_data$$monthNum)
})

test_that("6-month analysis: periodNum correctly aggregates months", {
  
  periodLength <- "6m"
  test_data <- data.frame(monthNum = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  test_data$$periodNum = as.numeric(ceiling(test_data$$monthNum / 6))
  
  # Months 1-6 should be period 1
  expect_equal(test_data$periodNum[1:6], rep(1, 6))
  
  # Months 7-12 should be period 2
  expect_equal(test_data$periodNum[7:12], rep(2, 6))
})

test_that("Period adjustment for monthly analyses is 7", {
  
  periodLength <- "1m"
  
  if (periodLength == "1m") {
    period_adjustment <- 7
  } else {
    period_adjustment <- 0
  }
  
  expect_equal(period_adjustment, 7)
})

test_that("Period adjustment for 6-month analyses is 0", {
  
  periodLength <- "6m"
  
  if (periodLength == "1m") {
    period_adjustment <- 7
  } else {
    period_adjustment <- 0
  }
  
  expect_equal(period_adjustment, 0)
})

test_that("Period adjustment shifts monthly results correctly", {
  
  # Month 7 (October 2021, trial start) should become period 0
  periodLength <- "1m"
  period_adjustment <- 7
  
  test_data <- data.frame(periodNum = c(1, 6, 7, 8, 12))
  test_data$$adjusted = test_data$$periodNum - period_adjustment
  
  expect_equal(test_data$adjusted[1], -6)  # April 2021: -6 months before trial
  expect_equal(test_data$adjusted[2], -1)  # Sept 2021: -1 month before trial
  expect_equal(test_data$adjusted[3], 0)   # Oct 2021: trial start
  expect_equal(test_data$adjusted[4], 1)   # Nov 2021: +1 month after trial
})

# ============================================================================
# TEST 2: Washout Period Exclusion (FIXED)
# ============================================================================

test_that("Washout sensitivity analysis excludes months 6, 7, 8", {
  
  sensitivity_analysis <- "washout"
  test_data <- data.frame(
    monthNum = 1:12,
    value = rnorm(12)
  )
  
  if (sensitivity_analysis == "washout") {
    test_data <- test_data[test_data$monthNum != 6, , drop = FALSE]  # Keep as data frame
    test_data <- test_data[test_data$monthNum != 7, , drop = FALSE]
    test_data <- test_data[test_data$monthNum != 8, , drop = FALSE]
  }
  
  expect_false(6 %in% test_data$monthNum)
  expect_false(7 %in% test_data$monthNum)
  expect_false(8 %in% test_data$monthNum)
  expect_equal(nrow(test_data), 9) # 12 - 3 = 9
})

test_that("Non-washout analysis includes all months", {
  
  sensitivity_analysis <- ""
  test_data <- data.frame(
    monthNum = 1:12,
    value = rnorm(12)
  )
  
  if (sensitivity_analysis == "washout") {
    test_data <- test_data[test_data$monthNum != 6, , drop = FALSE]
    test_data <- test_data[test_data$monthNum != 7, , drop = FALSE]
    test_data <- test_data[test_data$monthNum != 8, , drop = FALSE]
  }
  
  expect_equal(nrow(test_data), 12)
  expect_true(all(1:12 %in% test_data$monthNum))
})

test_that("Washout uses dplyr filter (alternative approach)", {
  
  sensitivity_analysis <- "washout"
  test_data <- data.frame(
    monthNum = 1:12,
    value = rnorm(12)
  )
  
  if (sensitivity_analysis == "washout") {
    test_data <- test_data %>% filter(!(monthNum %in% c(6, 7, 8)))
  }
  
  expect_false(6 %in% test_data$monthNum)
  expect_false(7 %in% test_data$monthNum)
  expect_false(8 %in% test_data$monthNum)
  expect_equal(nrow(test_data), 9)
})

# ============================================================================
# TEST 3: Cancer Type/Group Filtering
# ============================================================================

test_that("Individual cancer type filtering works correctly", {
  
  cancers_included <- "Suspected skin cancer"
  
  test_data <- baseData %>%
    filter(suspected_cancer_or_breast_symptomatic %in% c("Suspected skin cancer", 
                                                          "Suspected lung cancer",
                                                          "Suspected breast cancer"))
  
  if (cancers_included %in% names(cancer_groups)) {
    filtered <- subset(test_data, (suspected_cancer_or_breast_symptomatic %in% cancer_groups[[cancers_included]]))
  } else {
    filtered <- subset(test_data, (suspected_cancer_or_breast_symptomatic == cancers_included))
  }
  
  expect_true(all(filtered$suspected_cancer_or_breast_symptomatic == "Suspected skin cancer"))
  expect_false(any(filtered$suspected_cancer_or_breast_symptomatic == "Suspected lung cancer"))
})

test_that("Cancer group filtering includes all expected cancers", {
  
  cancers_included <- "primary_high_detection"
  
  test_data <- baseData %>%
    filter(suspected_cancer_or_breast_symptomatic %in% c("Suspected head & neck cancer",
                                                          "Suspected lung cancer",
                                                          "Suspected upper gastrointestinal cancer",
                                                          "Suspected skin cancer"))
  
  if (cancers_included %in% names(cancer_groups)) {
    filtered <- subset(test_data, (suspected_cancer_or_breast_symptomatic %in% cancer_groups[[cancers_included]]))
  } else {
    filtered <- subset(test_data, (suspected_cancer_or_breast_symptomatic == cancers_included))
  }
  
  expected_cancers <- c("Suspected head & neck cancer",
                       "Suspected lung cancer",
                       "Suspected upper gastrointestinal cancer")
  
  expect_true(all(filtered$suspected_cancer_or_breast_symptomatic %in% expected_cancers))
  expect_false(any(filtered$suspected_cancer_or_breast_symptomatic == "Suspected skin cancer"))
})

test_that("Monthly analysis skips individual cancer types", {
  
  periodLength <- "1m"
  cancers_included <- "Suspected skin cancer" # Individual cancer, not in cancer_groups
  
  should_skip <- (periodLength == "1m" && (!(cancers_included %in% c(names(cancer_groups)))))
  
  expect_true(should_skip)
})

test_that("Monthly analysis runs for cancer groups", {
  
  periodLength <- "1m"
  cancers_included <- "primary_high_detection"
  
  should_skip <- (periodLength == "1m" && (!(cancers_included %in% c(names(cancer_groups)))))
  
  expect_false(should_skip)
})

test_that("6-month analysis runs for all cancer types", {
  
  periodLength <- "6m"
  
  # Individual cancer
  cancers_included_individual <- "Suspected skin cancer"
  should_skip_individual <- (periodLength == "1m" && (!(cancers_included_individual %in% c(names(cancer_groups)))))
  expect_false(should_skip_individual)
  
  # Cancer group
  cancers_included_group <- "primary_high_detection"
  should_skip_group <- (periodLength == "1m" && (!(cancers_included_group %in% c(names(cancer_groups)))))
  expect_false(should_skip_group)
})

# ============================================================================
# TEST 4: Dependent Variable Selection
# ============================================================================

test_that("Correct dependent variable name selected for delayRate", {
  
  depVar <- "delayRate"
  sensitivity_analysis <- ""
  
  if (depVar == "delayRate") {
    depVarName <- "percentage_not_told_diagnosis_outcome_within_28_days"
    if (sensitivity_analysis == "14days") {
      depVarName <- "percentage_not_told_diagnosis_outcome_within_14_days"
    } else if (sensitivity_analysis == "42days") {
      depVarName <- "percentage_not_told_diagnosis_outcome_within_42_days"
    } else if (sensitivity_analysis == "62days") {
      depVarName <- "percentage_not_told_diagnosis_outcome_within_62_days"
    }
  }
  
  expect_equal(depVarName, "percentage_not_told_diagnosis_outcome_within_28_days")
})

test_that("Correct dependent variable name for alternate thresholds", {
  
  depVar <- "delayRate"
  
  # 14 days
  sensitivity_analysis <- "14days"
  if (depVar == "delayRate") {
    depVarName <- "percentage_not_told_diagnosis_outcome_within_28_days"
    if (sensitivity_analysis == "14days") {
      depVarName <- "percentage_not_told_diagnosis_outcome_within_14_days"
    } else if (sensitivity_analysis == "42days") {
      depVarName <- "percentage_not_told_diagnosis_outcome_within_42_days"
    } else if (sensitivity_analysis == "62days") {
      depVarName <- "percentage_not_told_diagnosis_outcome_within_62_days"
    }
  }
  expect_equal(depVarName, "percentage_not_told_diagnosis_outcome_within_14_days")
  
  # 42 days
  sensitivity_analysis <- "42days"
  if (depVar == "delayRate") {
    depVarName <- "percentage_not_told_diagnosis_outcome_within_28_days"
    if (sensitivity_analysis == "14days") {
      depVarName <- "percentage_not_told_diagnosis_outcome_within_14_days"
    } else if (sensitivity_analysis == "42days") {
      depVarName <- "percentage_not_told_diagnosis_outcome_within_42_days"
    } else if (sensitivity_analysis == "62days") {
      depVarName <- "percentage_not_told_diagnosis_outcome_within_62_days"
    }
  }
  expect_equal(depVarName, "percentage_not_told_diagnosis_outcome_within_42_days")
  
  # 62 days
  sensitivity_analysis <- "62days"
  if (depVar == "delayRate") {
    depVarName <- "percentage_not_told_diagnosis_outcome_within_28_days"
    if (sensitivity_analysis == "14days") {
      depVarName <- "percentage_not_told_diagnosis_outcome_within_14_days"
    } else if (sensitivity_analysis == "42days") {
      depVarName <- "percentage_not_told_diagnosis_outcome_within_42_days"
    } else if (sensitivity_analysis == "62days") {
      depVarName <- "percentage_not_told_diagnosis_outcome_within_62_days"
    }
  }
  expect_equal(depVarName, "percentage_not_told_diagnosis_outcome_within_62_days")
})

# ============================================================================
# TEST 5: Covariate Selection
# ============================================================================

test_that("Default covariate is percentageAbsent", {
  
  sensitivity_analysis <- ""
  covariate <- "`percentageAbsent` + "
  
  if (sensitivity_analysis == "altCovariate") {
    covariate <- "`bedOccupancyRate` + "
  } else if (sensitivity_analysis == "noCovariate") {
    covariate <- ""
  }
  
  expect_equal(covariate, "`percentageAbsent` + ")
})

test_that("altCovariate uses bedOccupancyRate", {
  
  sensitivity_analysis <- "altCovariate"
  covariate <- "`percentageAbsent` + "
  
  if (sensitivity_analysis == "altCovariate") {
    covariate <- "`bedOccupancyRate` + "
  } else if (sensitivity_analysis == "noCovariate") {
    covariate <- ""
  }
  
  expect_equal(covariate, "`bedOccupancyRate` + ")
})

test_that("noCovariate uses empty string", {
  
  sensitivity_analysis <- "noCovariate"
  covariate <- "`percentageAbsent` + "
  
  if (sensitivity_analysis == "altCovariate") {
    covariate <- "`bedOccupancyRate` + "
  } else if (sensitivity_analysis == "noCovariate") {
    covariate <- ""
  }
  
  expect_equal(covariate, "")
})

# ============================================================================
# TEST 6: Formula Construction
# ============================================================================

test_that("Formula constructed correctly for delayRate", {
  
  depVar <- "delayRate"
  sensitivity_analysis <- ""
  depVarName <- "percentage_not_told_diagnosis_outcome_within_28_days"
  covariate <- "`percentageAbsent` + "
  dummy_vars <- c("dummy_period_02", "dummy_period_03")
  
  if (depVar == "delayRate") {
    formula_str <- paste(depVarName, " ~ ", covariate, paste(dummy_vars, collapse = " + "), "| Cancer.Alliance + period")
  }
  
  expected <- "percentage_not_told_diagnosis_outcome_within_28_days  ~  `percentageAbsent` +  dummy_period_02 + dummy_period_03 | Cancer.Alliance + period"
  expect_equal(formula_str, expected)
})

test_that("Formula constructed correctly for refRate", {
  
  depVar <- "refRate"
  covariate <- "`percentageAbsent` + "
  dummy_vars <- c("dummy_period_02", "dummy_period_03")
  
  if (depVar == "refRate") {
    formula_str <- paste("ref_rate ~ ", covariate, paste(dummy_vars, collapse = " + "), "| Cancer.Alliance + period")
  }
  
  expected <- "ref_rate ~  `percentageAbsent` +  dummy_period_02 + dummy_period_03 | Cancer.Alliance + period"
  expect_equal(formula_str, expected)
})

test_that("Formula constructed correctly for estWaitTime", {
  
  depVar <- "estWaitTime"
  covariate <- "`percentageAbsent` + "
  dummy_vars <- c("dummy_period_02", "dummy_period_03")
  
  if (depVar == "estWaitTime") {
    formula_str <- paste("est_wait_time ~ ", covariate, paste(dummy_vars, collapse = " + "), "| Cancer.Alliance + period")
  }
  
  expected <- "est_wait_time ~  `percentageAbsent` +  dummy_period_02 + dummy_period_03 | Cancer.Alliance + period"
  expect_equal(formula_str, expected)
})

test_that("Formula excludes covariate when noCovariate specified", {
  
  depVar <- "delayRate"
  sensitivity_analysis <- "noCovariate"
  depVarName <- "percentage_not_told_diagnosis_outcome_within_28_days"
  covariate <- ""
  dummy_vars <- c("dummy_period_02", "dummy_period_03")
  
  if (depVar == "delayRate") {
    formula_str <- paste(depVarName, " ~ ", covariate, paste(dummy_vars, collapse = " + "), "| Cancer.Alliance + period")
  }
  
  # Should not contain percentageAbsent or bedOccupancyRate
  expect_false(grepl("percentageAbsent", formula_str))
  expect_false(grepl("bedOccupancyRate", formula_str))
  expect_true(grepl("dummy_period_02", formula_str))
})

# ============================================================================
# TEST 7: Output File Conditions (FIXED)
# ============================================================================

test_that("Summary stats only generated for 6m, no sensitivity, delayRate", {
  
  # Should generate
  periodLength <- "6m"
  sensitivity_analysis <- ""
  depVar <- "delayRate"
  should_generate_1 <- (periodLength == "6m") && (sensitivity_analysis == "") && (depVar == "delayRate")
  expect_true(should_generate_1)
  
  # Should not generate (monthly)
  periodLength <- "1m"
  sensitivity_analysis <- ""
  depVar <- "delayRate"
  should_generate_2 <- (periodLength == "6m") && (sensitivity_analysis == "") && (depVar == "delayRate")
  expect_false(should_generate_2)
  
  # Should not generate (sensitivity analysis)
  periodLength <- "6m"
  sensitivity_analysis <- "washout"
  depVar <- "delayRate"
  should_generate_3 <- (periodLength == "6m") && (sensitivity_analysis == "") && (depVar == "delayRate")
  expect_false(should_generate_3)
  
  # Should not generate (refRate)
  periodLength <- "6m"
  sensitivity_analysis <- ""
  depVar <- "refRate"
  should_generate_4 <- (periodLength == "6m") && (sensitivity_analysis == "") && (depVar == "delayRate")
  expect_false(should_generate_4)
})

test_that("Trend plots only generated for 1m and cancer groups", {
  
  periodLength <- "1m"
  cancers_included <- "primary_high_detection"
  
  should_generate <- (periodLength == "1m" && cancers_included %in% c(names(cancer_groups)))
  expect_true(should_generate)
  
  # Should not generate for individual cancers
  cancers_included <- "Suspected skin cancer"
  should_generate <- (periodLength == "1m" && cancers_included %in% c(names(cancer_groups)))
  expect_false(should_generate)
  
  # Should not generate for 6m
  periodLength <- "6m"
  cancers_included <- "primary_high_detection"
  should_generate <- (periodLength == "1m" && cancers_included %in% c(names(cancer_groups)))
  expect_false(should_generate)
})

test_that("Unadjusted rates only generated for 6m, delayRate, no sensitivity, cancer groups", {
  
  periodLength <- "6m"
  depVar <- "delayRate"
  sensitivity_analysis <- ""
  cancers_included <- "primary_high_detection"
  
  should_generate <- (periodLength == "6m" && depVar == "delayRate" && 
                     sensitivity_analysis == "" && 
                     cancers_included %in% names(cancer_groups))
  expect_true(should_generate)
  
  # Should not generate for refRate
  depVar <- "refRate"
  should_generate <- (periodLength == "6m" && depVar == "delayRate" && 
                     sensitivity_analysis == "" && 
                     cancers_included %in% names(cancer_groups))
  expect_false(should_generate)
  
  # Should not generate for individual cancers
  depVar <- "delayRate"
  cancers_included <- "Suspected skin cancer"
  should_generate <- (periodLength == "6m" && depVar == "delayRate" && 
                     sensitivity_analysis == "" && 
                     cancers_included %in% names(cancer_groups))
  expect_false(should_generate)
})

test_that("DiffInDiff plots only for 1m, cancer groups, delayRate, main analysis", {
  
  periodLength <- "1m"
  cancers_included <- "primary_high_detection"
  depVar <- "delayRate"
  sensitivity_analysis <- ""
  
  should_generate <- (periodLength == "1m" && 
                     cancers_included %in% c(names(cancer_groups)) && 
                     depVar == "delayRate" && 
                     sensitivity_analysis == "")
  expect_true(should_generate)
  
  # Should not generate for 6m
  periodLength <- "6m"
  should_generate <- (periodLength == "1m" && 
                     cancers_included %in% c(names(cancer_groups)) && 
                     depVar == "delayRate" && 
                     sensitivity_analysis == "")
  expect_false(should_generate)
  
  # Should not generate for sensitivity analysis
  periodLength <- "1m"
  sensitivity_analysis <- "washout"
  should_generate <- (periodLength == "1m" && 
                     cancers_included %in% c(names(cancer_groups)) && 
                     depVar == "delayRate" && 
                     sensitivity_analysis == "")
  expect_false(should_generate)
})

test_that("Overall results table only generated for 6m", {
  
  periodLength <- "6m"
  should_generate <- (periodLength == "6m")
  expect_true(should_generate)
  
  periodLength <- "1m"
  should_generate <- (periodLength == "6m")
  expect_false(should_generate)
})

# ============================================================================
# TEST 8: Model Specification Switching
# ============================================================================

test_that("Model specifications are mutually exclusive", {
  
  # Only one of these should be true for any given analysis
  sensitivity_analysis_options <- c("", "noWeights", "propensityScore", "wildBootstrap")
  
  for (sa in sensitivity_analysis_options) {
    is_default <- (sa == "")
    is_noWeights <- (sa == "noWeights")
    is_ps <- (sa == "propensityScore")
    is_bootstrap <- (sa == "wildBootstrap")
    
    # Exactly one should be TRUE
    expect_equal(sum(is_default, is_noWeights, is_ps, is_bootstrap), 1)
  }
})

test_that("Wild bootstrap skips acute leukaemia", {
  
  sensitivity_analysis <- "wildBootstrap"
  cancers_included <- "Suspected acute leukaemia"
  
  should_skip <- (sensitivity_analysis == "wildBootstrap" && 
                 cancers_included == "Suspected acute leukaemia")
  
  expect_true(should_skip)
  
  # Should not skip other cancers
  cancers_included <- "Suspected skin cancer"
  should_skip <- (sensitivity_analysis == "wildBootstrap" && 
                 cancers_included == "Suspected acute leukaemia")
  expect_false(should_skip)
})

# ============================================================================
# TEST 9: Suffix Generation
# ============================================================================

test_that("Suffix correctly constructed from parameters", {
  
  periodLength <- "6m"
  sensitivity_analysis <- "washout"
  depVar <- "delayRate"
  
  suffix <- paste0("_", periodLength, "_", sensitivity_analysis, "_", depVar)
  suffix <- gsub("__", "_", suffix)
  
  expect_equal(suffix, "_6m_washout_delayRate")
})

test_that("Suffix handles empty sensitivity_analysis", {
  
  periodLength <- "6m"
  sensitivity_analysis <- ""
  depVar <- "delayRate"
  
  suffix <- paste0("_", periodLength, "_", sensitivity_analysis, "_", depVar)
  suffix <- gsub("__", "_", suffix)
  
  expect_equal(suffix, "_6m_delayRate")
  expect_false(grepl("__", suffix))
})

test_that("Suffix correctly appended with cancer type", {
  
  periodLength <- "6m"
  sensitivity_analysis <- ""
  depVar <- "delayRate"
  cancers_included <- "Suspected head & neck cancer"
  
  suffix <- paste0("_", periodLength, "_", sensitivity_analysis, "_", depVar)
  suffix <- gsub("__", "_", suffix)
  suffix2 <- paste0(suffix, "_", gsub("/", "", cancers_included))
  
  # Should remove "/" from cancer names
  expect_false(grepl("/", suffix2))
  expect_true(grepl("Suspected head & neck cancer", suffix2))
})

# ============================================================================
# TEST 10: Dummy Variable Creation Logic
# ============================================================================

test_that("Dummy period 01 is dropped as reference", {
  
  test_data <- data.frame(
    Cancer.Alliance = rep("Test", 3),
    periodNum = c(1, 2, 3),
    mced_treated = c(1, 1, 0)
  )
  
  # Create dummies
  test_with_dummies <- test_data %>%
    mutate(dummy = ifelse(mced_treated == 1, 1, 0)) %>%
    pivot_wider(names_from = periodNum, values_from = dummy,
                names_prefix = "dummy_period_", values_fill = list(dummy = 0)) %>%
    rename_with(~ sub("dummy_period_([0-9])$", "dummy_period_0\\1", .x)) %>%
    select(-dummy_period_01)
  
  # dummy_period_01 should not exist
  expect_false("dummy_period_01" %in% names(test_with_dummies))
  
  # Other periods should exist
  expect_true("dummy_period_02" %in% names(test_with_dummies))
  expect_true("dummy_period_03" %in% names(test_with_dummies))
})

test_that("Dummy variables are 1 only for treated regions", {
  
  test_data <- data.frame(
    Cancer.Alliance = c("Treated", "Untreated"),
    periodNum = c(2, 2),
    mced_treated = c(1, 0)
  )
  
  test_with_dummies <- test_data %>%
    mutate(dummy = ifelse(mced_treated == 1, 1, 0)) %>%
    pivot_wider(names_from = periodNum, values_from = dummy,
                names_prefix = "dummy_period_", values_fill = list(dummy = 0))
  
  # Treated region should have dummy = 1
  expect_equal(test_with_dummies$$dummy_period_2[test_with_dummies$$Cancer.Alliance == "Treated"], 1)
  
  # Untreated region should have dummy = 0
  expect_equal(test_with_dummies$$dummy_period_2[test_with_dummies$$Cancer.Alliance == "Untreated"], 0)
})

# ============================================================================
# TEST 11: Variables of Interest Selection for Results Table
# ============================================================================

test_that("Correct periods selected for results table", {
  
  variables_of_interest <- c("dummy_period_02", "dummy_period_03", "dummy_period_04", 
                            "dummy_period_05", "dummy_period_06", "dummy_period_07")
  
  # Should have 6 periods (period 1 is reference)
  expect_equal(length(variables_of_interest), 6)
  
  # Should start at 02 (period 01 dropped)
  expect_equal(variables_of_interest[1], "dummy_period_02")
  
  # Should end at 07
  expect_equal(variables_of_interest[6], "dummy_period_07")
})

test_that("Period labels match variables of interest", {
  
  variables_of_interest <- c("dummy_period_02", "dummy_period_03", "dummy_period_04", 
                            "dummy_period_05", "dummy_period_06", "dummy_period_07")
  
  period_labels <- c("Months0-5", "Months6-11", "Months12-17", 
                    "Months18-23", "Months24-29", "Months30-35")
  
  expect_equal(length(variables_of_interest), length(period_labels))
})

# ============================================================================
# TEST 12: P-value Formatting Logic
# ============================================================================

test_that("P-values < 0.001 formatted correctly", {
  
  p_value <- 0.0005
  p_value_text <- ifelse(p_value < 0.001, "p<0.001", paste0("p=", sprintf("%.3f", p_value)))
  
  expect_equal(p_value_text, "p<0.001")
})

test_that("P-values >= 0.001 formatted correctly", {
  
  p_value <- 0.0234
  p_value_text <- ifelse(p_value < 0.001, "p<0.001", paste0("p=", sprintf("%.3f", p_value)))
  
  expect_equal(p_value_text, "p=0.023")
})

test_that("P-values only added for cancer groups, not individual cancers", {
  
  cancers_included <- "primary_high_detection"
  should_include_p <- cancers_included %in% c(names(cancer_groups))
  expect_true(should_include_p)
  
  cancers_included <- "Suspected skin cancer"
  should_include_p <- cancers_included %in% c(names(cancer_groups))
  expect_false(should_include_p)
})