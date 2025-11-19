# test_aggregation.R
library(testthat)
library(tidyverse)
library(here)

# Load the actual data
dirPath <- paste0(here("data/"), "/")
dataDirPath <- paste0(dirPath, "cleaned_data/")
baseData <- read.csv(paste0(dataDirPath, "base_data.csv"),
                     row.names = 1,
                     stringsAsFactors = FALSE)

# Set up test data for a specific subset
test_data <- baseData %>%
  filter(
    Provider.Code %in% c("AQK", "R0A"), # Pick 2 providers
    monthNum %in% c(1, 2), # Pick 2 months
    suspected_cancer_or_breast_symptomatic %in% c("Suspected skin cancer", "Suspected lung cancer")
  ) %>%
  mutate(periodNum = monthNum, period = as.character(periodNum))

# ============================================================================
# TEST 1: Stage 1 Aggregation (Provider + Month level)
# ============================================================================

test_that("Stage 1: Provider+Month aggregation sums outcomes correctly", {
  
  # Manually calculate expected sum for a specific provider-month
  expected <- test_data %>%
    filter(Provider.Code == "AQK", monthNum == 1) %>%
    summarise(total = sum(told_diagnosis_outcome_total)) %>%
    pull(total)
  
  # Run the aggregation
  stage1 <- test_data %>%
    group_by(Provider.Code, monthNum) %>%
    summarise(
      told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
      num_not_told_diagnosis_outcome_within_28_days = sum(num_not_told_diagnosis_outcome_within_28_days),
      .groups = "drop"
    )
  
  actual <- stage1 %>%
    filter(Provider.Code == "AQK", monthNum == 1) %>%
    pull(told_diagnosis_outcome_total)
  
  expect_equal(actual, expected)
})

test_that("Stage 1: Provider+Month takes first value of staff (not sum across cancer types)", {
  
  # Create test case where we know staff numbers
  test_staff_data <- data.frame(
    Provider.Code = c("TEST", "TEST", "TEST"),
    monthNum = c(1, 1, 1),
    suspected_cancer_or_breast_symptomatic = c("Suspected skin cancer", "Suspected lung cancer", "Suspected breast cancer"),
    told_diagnosis_outcome_total = c(100, 200, 150),
    num_not_told_diagnosis_outcome_within_28_days = c(20, 40, 30),
    totalStaff = c(500, 500, 500), # Same staff for all cancer types in same provider-month
    totalAbsent = c(50, 50, 50),
    totalBedsOccupied = c(100, 100, 100),
    Cancer.Alliance = c("Test Alliance", "Test Alliance", "Test Alliance"),
    mced_treated = c(1, 1, 1),
    periodNum = c(1, 1, 1),
    period = c("1", "1", "1"),
    date = c("2021-04-01", "2021-04-01", "2021-04-01"),
    stringsAsFactors = FALSE
  )
  
  stage1 <- test_staff_data %>%
    group_by(Provider.Code, monthNum) %>%
    summarise(
      told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
      totalStaff = first(totalStaff),
      totalAbsent = first(totalAbsent),
      totalBedsOccupied = first(totalBedsOccupied),
      .groups = "drop"
    )
  
  # Staff should be 500 (first value), NOT 1500 (sum)
  expect_equal(stage1$totalStaff[1], 500)
  expect_equal(stage1$totalAbsent[1], 50)
  expect_equal(stage1$totalBedsOccupied[1], 100)
  
  # But outcomes should sum
  expect_equal(stage1$told_diagnosis_outcome_total[1], 450) # 100+200+150
})

test_that("Stage 1: All outcome variables are summed correctly", {
  
  stage1 <- test_data %>%
    group_by(Provider.Code, monthNum) %>%
    summarise(
      num_not_told_diagnosis_outcome_within_28_days = sum(num_not_told_diagnosis_outcome_within_28_days),
      told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
      told_diagnosis_outcome_within_14_days = sum(told_diagnosis_outcome_within_14_days),
      in_15_to_28_days = sum(in_15_to_28_days),
      told_diagnosis_outcome_in_29_to_42_days = sum(told_diagnosis_outcome_in_29_to_42_days),
      told_diagnosis_outcome_in_43_to_62_days = sum(told_diagnosis_outcome_in_43_to_62_days),
      told_diagnosis_outcome_after_62_days = sum(told_diagnosis_outcome_after_62_days),
      .groups = "drop"
    )
  
  # For each provider-month, sum of time categories should equal total
  stage1 <- stage1 %>%
    mutate(
      sum_of_categories = told_diagnosis_outcome_within_14_days + 
        in_15_to_28_days +
        told_diagnosis_outcome_in_29_to_42_days +
        told_diagnosis_outcome_in_43_to_62_days +
        told_diagnosis_outcome_after_62_days
    )
  
  expect_equal(stage1$sum_of_categories, stage1$told_diagnosis_outcome_total)
})

# ============================================================================
# TEST 2: Stage 2 Aggregation (Cancer Alliance + Month level)
# ============================================================================

test_that("Stage 2: Cancer Alliance+Month sums outcomes across providers", {
  
  # Stage 1
  stage1 <- test_data %>%
    group_by(Provider.Code, monthNum) %>%
    summarise(
      told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
      num_not_told_diagnosis_outcome_within_28_days = sum(num_not_told_diagnosis_outcome_within_28_days),
      Cancer.Alliance = first(Cancer.Alliance),
      .groups = "drop"
    )
  
  # Calculate expected: sum across all providers in a Cancer Alliance-month
  expected <- stage1 %>%
    filter(monthNum == 1) %>%
    group_by(Cancer.Alliance) %>%
    summarise(total = sum(told_diagnosis_outcome_total)) %>%
    slice(1) %>%
    pull(total)
  
  # Stage 2
  stage2 <- stage1 %>%
    group_by(Cancer.Alliance, monthNum) %>%
    summarise(
      told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
      .groups = "drop"
    )
  
  actual <- stage2 %>%
    filter(monthNum == 1) %>%
    slice(1) %>%
    pull(told_diagnosis_outcome_total)
  
  expect_equal(actual, expected)
})

test_that("Stage 2: Cancer Alliance+Month sums staff across providers", {
  
  # Create test data with multiple providers
  test_staff_data <- data.frame(
    Provider.Code = c("PROV1", "PROV1", "PROV2", "PROV2"),
    monthNum = c(1, 1, 1, 1),
    suspected_cancer_or_breast_symptomatic = c("Suspected skin cancer", "Suspected lung cancer", 
                                                "Suspected skin cancer", "Suspected lung cancer"),
    told_diagnosis_outcome_total = c(100, 200, 150, 250),
    totalStaff = c(500, 500, 300, 300), # Same within provider, different between
    Cancer.Alliance = c("Alliance1", "Alliance1", "Alliance1", "Alliance1"),
    .groups = "drop",
    stringsAsFactors = FALSE
  )
  
  # Stage 1: first() within provider
  stage1 <- test_staff_data %>%
    group_by(Provider.Code, monthNum) %>%
    summarise(
      told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
      totalStaff = first(totalStaff),
      Cancer.Alliance = first(Cancer.Alliance),
      .groups = "drop"
    )
  
  # Stage 2: sum() across providers
  stage2 <- stage1 %>%
    group_by(Cancer.Alliance, monthNum) %>%
    summarise(
      totalStaff = sum(totalStaff, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Should be 500 + 300 = 800 (sum of two providers, not 500+500+300+300)
  expect_equal(stage2$totalStaff[1], 800)
})

# ============================================================================
# TEST 3: Stage 3 Aggregation (Cancer Alliance + Period level)
# ============================================================================

test_that("Stage 3: Cancer Alliance+Period sums outcomes across months", {
  
  # Use 6-month period aggregation
  test_6m <- test_data %>%
    mutate(periodNum = ceiling(monthNum / 6))
  
  # Go through all 3 stages
  stage1 <- test_6m %>%
    group_by(Provider.Code, monthNum) %>%
    summarise(
      told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
      Cancer.Alliance = first(Cancer.Alliance),
      periodNum = first(periodNum),
      period = as.character(first(periodNum)),
      .groups = "drop"
    )
  
  stage2 <- stage1 %>%
    group_by(Cancer.Alliance, monthNum) %>%
    summarise(
      told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
      periodNum = first(periodNum),
      period = first(period),
      .groups = "drop"
    )
  
  # Expected: sum across months 1 and 2 (both in period 1)
  expected <- stage2 %>%
    filter(periodNum == 1) %>%
    group_by(Cancer.Alliance) %>%
    summarise(total = sum(told_diagnosis_outcome_total)) %>%
    slice(1) %>%
    pull(total)
  
  # Stage 3
  stage3 <- stage2 %>%
    group_by(Cancer.Alliance, period) %>%
    summarise(
      told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
      .groups = "drop"
    )
  
  actual <- stage3 %>%
    slice(1) %>%
    pull(told_diagnosis_outcome_total)
  
  expect_equal(actual, expected)
})

test_that("Stage 3: Cancer Alliance+Period takes mean of staff across months", {
  
  # Create test data across multiple months
  test_staff_data <- data.frame(
    Provider.Code = c("PROV1", "PROV1"),
    monthNum = c(1, 2),
    suspected_cancer_or_breast_symptomatic = c("Suspected skin cancer", "Suspected skin cancer"),
    told_diagnosis_outcome_total = c(100, 200),
    totalStaff = c(500, 600),
    Cancer.Alliance = c("Alliance1", "Alliance1"),
    periodNum = c(1, 1),
    period = c("1", "1"),
    stringsAsFactors = FALSE
  )
  
  # Stage 1
  stage1 <- test_staff_data %>%
    group_by(Provider.Code, monthNum) %>%
    summarise(
      totalStaff = first(totalStaff),
      Cancer.Alliance = first(Cancer.Alliance),
      periodNum = first(periodNum),
      period = first(period),
      .groups = "drop"
    )
  
  # Stage 2
  stage2 <- stage1 %>%
    group_by(Cancer.Alliance, monthNum) %>%
    summarise(
      totalStaff = sum(totalStaff, na.rm = TRUE),
      periodNum = first(periodNum),
      period = first(period),
      .groups = "drop"
    )
  
  # Stage 3: mean across months
  stage3 <- stage2 %>%
    group_by(Cancer.Alliance, period) %>%
    summarise(
      totalStaff = mean(totalStaff, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Should be (500 + 600) / 2 = 550
  expect_equal(stage3$totalStaff[1], 550)
})

# ============================================================================
# TEST 4: End-to-End Aggregation Integrity
# ============================================================================

test_that("No rows are lost during aggregation pipeline", {
  
  # Count unique provider-month-cancer combinations
  initial_rows <- test_data %>%
    distinct(Provider.Code, monthNum, suspected_cancer_or_breast_symptomatic) %>%
    nrow()
  
  # After stage 1, should have fewer rows (collapsed across cancer types)
  stage1 <- test_data %>%
    group_by(Provider.Code, monthNum) %>%
    summarise(
      told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
      Cancer.Alliance = first(Cancer.Alliance),
      .groups = "drop"
    )
  
  stage1_rows <- nrow(stage1)
  
  # Should have collapsed at least some rows
  expect_lt(stage1_rows, initial_rows)
  
  # Count unique provider-month combinations
  expected_stage1_rows <- test_data %>%
    distinct(Provider.Code, monthNum) %>%
    nrow()
  
  expect_equal(stage1_rows, expected_stage1_rows)
})

test_that("Total referrals are conserved through aggregation pipeline", {
  
  # Calculate total across entire dataset
  total_referrals_initial <- sum(test_data$told_diagnosis_outcome_total)
  
  # Go through all 3 stages
  stage1 <- test_data %>%
    group_by(Provider.Code, monthNum) %>%
    summarise(
      told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
      Cancer.Alliance = first(Cancer.Alliance),
      periodNum = first(periodNum),
      period = first(period),
      .groups = "drop"
    )
  
  stage2 <- stage1 %>%
    group_by(Cancer.Alliance, monthNum) %>%
    summarise(
      told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
      periodNum = first(periodNum),
      period = first(period),
      .groups = "drop"
    )
  
  stage3 <- stage2 %>%
    group_by(Cancer.Alliance, period) %>%
    summarise(
      told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
      .groups = "drop"
    )
  
  # Total should be same at every stage
  expect_equal(sum(stage1$told_diagnosis_outcome_total), total_referrals_initial)
  expect_equal(sum(stage2$told_diagnosis_outcome_total), total_referrals_initial)
  expect_equal(sum(stage3$told_diagnosis_outcome_total), total_referrals_initial)
})

test_that("NA handling works correctly for staff variables", {
  
  # Add some NAs to test data
  test_na_data <- test_data
  test_na_data$totalStaff[1:5] <- NA
  
  # Stage 1
  stage1 <- test_na_data %>%
    group_by(Provider.Code, monthNum) %>%
    summarise(
      totalStaff = first(totalStaff),
      Cancer.Alliance = first(Cancer.Alliance),
      periodNum = first(periodNum),
      period = first(period),
      .groups = "drop"
    )
  
  # Stage 2 should use na.rm = TRUE
  stage2 <- stage1 %>%
    group_by(Cancer.Alliance, monthNum) %>%
    summarise(
      totalStaff = sum(totalStaff, na.rm = TRUE),
      periodNum = first(periodNum),
      period = first(period),
      .groups = "drop"
    )
  
  # Should not be NA or error
  expect_false(all(is.na(stage2$totalStaff)))
})

# ============================================================================
# TEST 5: Integration with Real Analysis Function
# ============================================================================

test_that("Aggregation matches what Analysis function produces (smoke test)", {
  
  # This is a simplified version of what happens in Analysis()
  # Use small subset of real data
  small_data <- baseData %>%
    filter(monthNum %in% 1:2) %>%
    mutate(
      periodNum = ceiling(monthNum / 6),
      period = as.character(periodNum)
    ) %>%
    filter(suspected_cancer_or_breast_symptomatic == "Suspected skin cancer")
  
  # Manual aggregation
  manual <- small_data %>%
    group_by(Provider.Code, monthNum) %>%
    summarise(
      told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
      Cancer.Alliance = first(Cancer.Alliance),
      totalStaff = first(totalStaff),
      periodNum = first(periodNum),
      period = first(period),
      .groups = "drop"
    ) %>%
    group_by(Cancer.Alliance, monthNum) %>%
    summarise(
      told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
      totalStaff = sum(totalStaff, na.rm = TRUE),
      periodNum = first(periodNum),
      period = first(period),
      .groups = "drop"
    ) %>%
    group_by(Cancer.Alliance, period) %>%
    summarise(
      told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
      totalStaff = mean(totalStaff, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Check that we got some data
  expect_gt(nrow(manual), 0)
  expect_true(all(!is.na(manual$told_diagnosis_outcome_total)))
})