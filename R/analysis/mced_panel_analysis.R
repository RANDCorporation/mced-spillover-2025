# title: "Create MCED Panel Dataset"
# author: "Joshua Eagan and Sean Mann"
# date: "23 July 2025"
# output: github_document

# Description
# This program grabs data from the provided URLs and processes them to generate a panel dataset for analysis of diagnostic delays in NHS England.

# ### Read in Analytic Data

library(tidyverse)
library(fixest)
library(scales)
library(here)
library(fwildclusterboot)

# fwildclusterboot is no longer available on cran, so I installed the most recent version from the cran archive.
# it's important you use this line to install, as results for this package are not always backwards compatible.
# remotes::install_version("fwildclusterboot", "0.13.0")

# This sets the seed of the package used by fwildclusterboot to ensure reproducibility
dqrng::dqset.seed(20251023)

# # # Global parameters start # # #

# Set working directory path
dirPath <- paste0(here("data/"), "/")
dataDirPath <- paste0(dirPath, "cleaned_data/")
covariateDirPath <- paste0(dirPath, "raw_covariates/")
linkingDirPath <- paste0(dirPath, "linking_files/")
resultsDirPath <- paste0(dirPath, "results/")

cancer_types_to_loop_through <- c(
  "Suspected breast cancer",
  "Suspected lower gastrointestinal cancer",
  "Suspected skin cancer",
  "Suspected acute leukaemia",
  "Suspected brain/central nervous system tumours",
  "Suspected gynaecological cancer",
  "Suspected haematological malignancies (excluding acute leukaemia)",
  "Suspected head & neck cancer",
  "Suspected lung cancer",
  "Suspected other cancer",
  "Suspected sarcoma",
  "Suspected testicular cancer",
  "Suspected upper gastrointestinal cancer",
  "Suspected urological malignancies (excluding testicular)"
)

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

cancer_types_to_loop_through <- c(cancer_types_to_loop_through, names(cancer_groups))

# # # Global parameters end # # #

# Import dataset file that was output from mced_panel_datacollection.R
baseData <- read.csv(paste0(dataDirPath, "base_data.csv"),
                 row.names = 1,
                 stringsAsFactors = FALSE)

# Set up function for separate analyses
Analysis <- function(data, periodLength, sensitivity_analysis, depVar) {

  # Create suffix with analysis run parameters to append at end of resulting filenames
  suffix <- paste0("_", periodLength, "_", sensitivity_analysis, "_", depVar)
  suffix <- gsub("__", "_", suffix) # if no sensitivity analysis, replace double underscore with single

  # Set conversion factor for result, no scaling at all for referral or estimated wait time numbers, scale by 100 (%) for delay rate
  if (depVar %in% c("refRate", "estWaitTime")) {
    conversionFactor <- 1
  }
  if (depVar == "delayRate") {
    conversionFactor <- 100
  }

  overall_results <- data.frame()

  # For monthly analysis, adjust period labels so that period 0 is the first month following start of the trial (October 2021)
  if (periodLength == "1m") {
    period_adjustment <- 7
  } else {
    period_adjustment <- 0
  }

  # # Sensitivity analysis: Exclude data from Sep, Oct, Nov 2021 as washout or intervention phase-in period
  if (sensitivity_analysis == "washout") {
    data <- data[data$monthNum != 6, ]
    data <- data[data$monthNum != 7, ]
    data <- data[data$monthNum != 8, ]
  }

  # # Create period number variable
  # # In main analysis, which uses monthly time periods for event study analysis, periodNum is same as monthNum
  data$periodNum = data$monthNum

  # # For analysis for table, convert monthly numbers into six-month time periods
  if (periodLength == "6m") {
    data$periodNum = as.numeric(ceiling(data$monthNum / 6))
  }

  # Create column that holds period number as a string (for use in grouping operation)
  data$period = as.character(data$periodNum)

  # # # Generate summary stats for Table 1 # # #
  # Use data aggregated up to 6 month periods
  if (periodLength == "6m" && sensitivity_analysis == "" && depVar == "delayRate") {

    summary_stats <- data.frame(label = character(), value = numeric())

    # Just use data from the first 6m (pre-trial) period in the dataset
    preTrial <- subset(data, (periodNum == 1))

    # Separate data into participating and non-participating cancer alliances
    participating <- subset(preTrial, (mced_treated == 1))
    notParticipating <- subset(preTrial, (mced_treated == 0))

    summary_stats <- summary_stats %>%
      add_row(label = paste0("Table 1, number of providers, participating in trial", suffix),
              value = n_distinct(participating$Provider.Code))

    summary_stats <- summary_stats %>%
      add_row(label = paste0("Table 1, number of providers, not participating in trial", suffix),
              value = n_distinct(notParticipating$Provider.Code))

    summary_stats <- summary_stats %>%
      add_row(label = paste0("for calculation_Table 1, total referrals, participating in trial", suffix),
              value = sum(participating$told_diagnosis_outcome_total))

    summary_stats <- summary_stats %>%
      add_row(label = paste0("for calculation_Table 1, total referrals, not participating in trial", suffix),
              value = sum(notParticipating$told_diagnosis_outcome_total))

    # To count staff numbers... first group rows across cancer sites at the Provider.Code and month level
    # Done in this sequence in order to only count staff numbers once per provider-month (and not multiple times for each cancer site)
    dataGrouped <- preTrial %>%
      group_by(Provider.Code, monthNum) %>%
      summarise(
        num_not_told_diagnosis_outcome_within_28_days = sum(num_not_told_diagnosis_outcome_within_28_days),
        told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
        told_diagnosis_outcome_within_14_days = sum(told_diagnosis_outcome_within_14_days),
        in_15_to_28_days = sum(in_15_to_28_days),
        told_diagnosis_outcome_in_29_to_42_days = sum(told_diagnosis_outcome_in_29_to_42_days),
        told_diagnosis_outcome_in_43_to_62_days = sum(told_diagnosis_outcome_in_43_to_62_days),
        told_diagnosis_outcome_after_62_days = sum(told_diagnosis_outcome_after_62_days),
        mced_treated = mean(mced_treated),
        periodNum = mean(periodNum),
        period = first(period),
        Cancer.Alliance = first(Cancer.Alliance),
        totalStaff = first(totalStaff),
        totalAbsent = first(totalAbsent),
        totalBedsOccupied = first(totalBedsOccupied),
        date = first(date)
      )

    # Group data (which had been at provider- and month-level) up to the cancer alliance- and month-level
    # Done in this sequence in order to sum staff numbers across providers
    dataGrouped <- dataGrouped %>%
      group_by(Cancer.Alliance, monthNum) %>%
      summarise(
        num_not_told_diagnosis_outcome_within_28_days = sum(num_not_told_diagnosis_outcome_within_28_days),
        told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
        told_diagnosis_outcome_within_14_days = sum(told_diagnosis_outcome_within_14_days),
        in_15_to_28_days = sum(in_15_to_28_days),
        told_diagnosis_outcome_in_29_to_42_days = sum(told_diagnosis_outcome_in_29_to_42_days),
        told_diagnosis_outcome_in_43_to_62_days = sum(told_diagnosis_outcome_in_43_to_62_days),
        told_diagnosis_outcome_after_62_days = sum(told_diagnosis_outcome_after_62_days),
        mced_treated = mean(mced_treated),
        period = first(period),
        periodNum = mean(periodNum),
        totalStaff = sum(totalStaff, na.rm = TRUE),
        totalAbsent = sum(totalAbsent, na.rm = TRUE),
        totalBedsOccupied = sum(totalBedsOccupied, na.rm = TRUE),
        date = first(date)
      )

    # Group data (which had been at Cancer.Alliance- and month-level) up to the cancer alliance- and period-level
    # Done in this sequence in order to sum staff numbers across months
    dataGrouped <- dataGrouped %>%
      group_by(Cancer.Alliance, period) %>%
      summarise(
        num_not_told_diagnosis_outcome_within_28_days = sum(num_not_told_diagnosis_outcome_within_28_days),
        told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
        told_diagnosis_outcome_within_14_days = sum(told_diagnosis_outcome_within_14_days),
        in_15_to_28_days = sum(in_15_to_28_days),
        told_diagnosis_outcome_in_29_to_42_days = sum(told_diagnosis_outcome_in_29_to_42_days),
        told_diagnosis_outcome_in_43_to_62_days = sum(told_diagnosis_outcome_in_43_to_62_days),
        told_diagnosis_outcome_after_62_days = sum(told_diagnosis_outcome_after_62_days),
        mced_treated = mean(mced_treated),
        periodNum = mean(periodNum),
        totalStaff = mean(totalStaff, na.rm = TRUE),
        totalAbsent = mean(totalAbsent, na.rm = TRUE),
        totalBedsOccupied = mean(totalBedsOccupied, na.rm = TRUE),
        date = first(date)
      )

    # Separate grouped data into participating and non-participating cancer alliances
    participating <- subset(dataGrouped, (mced_treated == 1))
    notParticipating <- subset(dataGrouped, (mced_treated == 0))

    summary_stats <- summary_stats %>%
      add_row(label = paste0("Table 1, number of staff, participating in trial", suffix),
              value = sum(participating$totalStaff, na.rm = TRUE))

    summary_stats <- summary_stats %>%
      add_row(label = paste0("Table 1, number of staff, not participating in trial", suffix),
              value = sum(notParticipating$totalStaff, na.rm = TRUE))

    write.csv(summary_stats, paste0(resultsDirPath, "summaryStats.csv"), row.names = FALSE)
  }

  startingData <- data

  for (cancers_included in c(cancer_types_to_loop_through)) {
    # Only run 1m analyses to produce monthly figures for the main cancer groups, primary delay rate outcome, and main analysis
    if (periodLength == "1m" && (!(cancers_included %in% c(names(cancer_groups))))) {
      next
    }

    # Print cancer group or cancer name that is focus of this analysis iteration
    print(cancers_included)

    suffix2 <- paste0("_", periodLength, "_", sensitivity_analysis, depVar, "_", gsub("/", "", cancers_included))

    data <- startingData

    if (cancers_included %in% names(cancer_groups)) {
      data <- subset(data, (suspected_cancer_or_breast_symptomatic %in% cancer_groups[[cancers_included]]))
    }
    else {
      data <- subset(data, (suspected_cancer_or_breast_symptomatic == cancers_included))
    }

    # For monthly analyses and primary cancer groups, produce trend figures using unadjusted values
    if (periodLength == "1m" && cancers_included %in% c(names(cancer_groups))) {
      trendData<-data %>%
        group_by(mced_treated, period) %>%
        summarise(
          num_not_told_diagnosis_outcome_within_28_days = sum(num_not_told_diagnosis_outcome_within_28_days),
          told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
          mced_treated = mean(mced_treated),
          periodNum = mean(periodNum),
          date = first(date)
        )
      trendData$percentage_not_told_diagnosis_outcome_within_28_days = (trendData$num_not_told_diagnosis_outcome_within_28_days / trendData$told_diagnosis_outcome_total)

      trendData <- trendData %>%
        mutate(
          point_estimate = percentage_not_told_diagnosis_outcome_within_28_days,
          standard_error = sqrt((point_estimate * (1 - point_estimate)) / told_diagnosis_outcome_total),
          lower_ci = point_estimate - 1.96 * standard_error,
          upper_ci = point_estimate + 1.96 * standard_error,
          mced_treated = ifelse(mced_treated == 0, "Not participating in trial", "Participating in trial") # Change labels
        )

      # Set axis label in figure so that start of trial (Oct 2021) is 0, meaning pre-trial time periods are negative.
      trendData$periodNum = trendData$periodNum - period_adjustment

      # Find the first period for labeling
      label_data <- trendData %>%
        group_by(mced_treated) %>%
        filter(periodNum == min(periodNum))

      trendPlot = ggplot(trendData, aes(x = periodNum, y = point_estimate, color = factor(mced_treated))) +
        geom_point(position = position_dodge(width = 0.25), size = 0.35) +
        geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(width = 0.25)) +
        scale_x_continuous(limits = c(-6.7, 14.7), breaks = seq(-6, 14, by = 1), expand = c(0, 0)) +
        scale_y_continuous(labels = scales::percent_format(), limits = c(0.10, 0.50)) + # Set limits to start at 0%
        labs(
          x = "Length of time (months) relative to trial start",
          y = "Diagnostic delay rate"
        ) +
        theme(panel.grid.minor.x = element_blank(),
              legend.position = "none",
              axis.title = element_text(size = 14),
              axis.title.x = element_text(margin = margin(t = 5)),
              axis.title.y = element_text(margin = margin(r = 8)),
              axis.text = element_text(size = 12))

      file_path <- paste0(resultsDirPath, "TrendPlot_", gsub("/", "", cancers_included), suffix, ".png")
      ggsave(file_path, trendPlot, width = 12, height = 4.5, units = "in", dpi = 300)
    }

    # First group rows across cancer sites at the Provider.Code and month level
    # Done in this sequence in order to only count staff and absence and bed numbers once per provider-month (and not multiple times for each cancer site)
    dataGrouped <- data %>%
      group_by(Provider.Code, monthNum) %>%
      summarise(
        num_not_told_diagnosis_outcome_within_28_days = sum(num_not_told_diagnosis_outcome_within_28_days),
        told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
        told_diagnosis_outcome_within_14_days = sum(told_diagnosis_outcome_within_14_days),
        in_15_to_28_days = sum(in_15_to_28_days),
        told_diagnosis_outcome_in_29_to_42_days = sum(told_diagnosis_outcome_in_29_to_42_days),
        told_diagnosis_outcome_in_43_to_62_days = sum(told_diagnosis_outcome_in_43_to_62_days),
        told_diagnosis_outcome_after_62_days = sum(told_diagnosis_outcome_after_62_days),
        mced_treated = mean(mced_treated),
        periodNum = mean(periodNum),
        period = first(period),
        Cancer.Alliance = first(Cancer.Alliance),
        totalStaff = first(totalStaff),
        totalAbsent = first(totalAbsent),
        totalBedsOccupied = first(totalBedsOccupied),
        date = first(date)
      )

    # Group data (which had been at provider- and month-level) up to the cancer alliance- and month-level
    # Done in this sequence in order to sum staff and absence numbers across providers
    dataGrouped <- dataGrouped %>%
      group_by(Cancer.Alliance, monthNum) %>%
      summarise(
        num_not_told_diagnosis_outcome_within_28_days = sum(num_not_told_diagnosis_outcome_within_28_days),
        told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
        told_diagnosis_outcome_within_14_days = sum(told_diagnosis_outcome_within_14_days),
        in_15_to_28_days = sum(in_15_to_28_days),
        told_diagnosis_outcome_in_29_to_42_days = sum(told_diagnosis_outcome_in_29_to_42_days),
        told_diagnosis_outcome_in_43_to_62_days = sum(told_diagnosis_outcome_in_43_to_62_days),
        told_diagnosis_outcome_after_62_days = sum(told_diagnosis_outcome_after_62_days),
        mced_treated = mean(mced_treated),
        period = first(period),
        periodNum = mean(periodNum),
        totalStaff = sum(totalStaff, na.rm = TRUE),
        totalAbsent = sum(totalAbsent, na.rm = TRUE),
        totalBedsOccupied = sum(totalBedsOccupied, na.rm = TRUE),
        date = first(date)
      )

    # Group data (which had been at Cancer.Alliance- and month-level) up to the cancer alliance- and period-level
    # Done in this sequence in order to sum staff and absence numbers across months
    dataGrouped <- dataGrouped %>%
      group_by(Cancer.Alliance, period) %>%
      summarise(
        num_not_told_diagnosis_outcome_within_28_days = sum(num_not_told_diagnosis_outcome_within_28_days),
        told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
        told_diagnosis_outcome_within_14_days = sum(told_diagnosis_outcome_within_14_days),
        in_15_to_28_days = sum(in_15_to_28_days),
        told_diagnosis_outcome_in_29_to_42_days = sum(told_diagnosis_outcome_in_29_to_42_days),
        told_diagnosis_outcome_in_43_to_62_days = sum(told_diagnosis_outcome_in_43_to_62_days),
        told_diagnosis_outcome_after_62_days = sum(told_diagnosis_outcome_after_62_days),
        mced_treated = mean(mced_treated),
        periodNum = mean(periodNum),
        totalStaff = mean(totalStaff, na.rm = TRUE),
        totalAbsent = mean(totalAbsent, na.rm = TRUE),
        totalBedsOccupied = mean(totalBedsOccupied, na.rm = TRUE),
        date = first(date)
      )

    # calculating percentage of patients facing diagnostic delay
    dataGrouped$percentage_not_told_diagnosis_outcome_within_28_days = (dataGrouped$num_not_told_diagnosis_outcome_within_28_days / dataGrouped$told_diagnosis_outcome_total)

    # calculating percentage of patients facing diagnostic delay according to alternate thresholds for sensitivity analyses
    dataGrouped$percentage_not_told_diagnosis_outcome_within_14_days = ((dataGrouped$told_diagnosis_outcome_total - dataGrouped$told_diagnosis_outcome_within_14_days) / dataGrouped$told_diagnosis_outcome_total)
    dataGrouped$percentage_not_told_diagnosis_outcome_within_42_days = ((dataGrouped$told_diagnosis_outcome_in_43_to_62_days + dataGrouped$told_diagnosis_outcome_after_62_days) / dataGrouped$told_diagnosis_outcome_total)
    dataGrouped$percentage_not_told_diagnosis_outcome_within_62_days = (dataGrouped$told_diagnosis_outcome_after_62_days / dataGrouped$told_diagnosis_outcome_total)

    # Calculate and export unadjusted diagnostic delay rates for treated and untreated groups of regions
    if (periodLength == "6m" && depVar == "delayRate" && sensitivity_analysis == "" && cancers_included %in% names(cancer_groups)) {
      dataGroupedParticipating <- dataGrouped %>%
        group_by(mced_treated, period) %>%
        summarise(
          num_not_told_diagnosis_outcome_within_28_days = sum(num_not_told_diagnosis_outcome_within_28_days),
          told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
          told_diagnosis_outcome_within_14_days = sum(told_diagnosis_outcome_within_14_days),
          in_15_to_28_days = sum(in_15_to_28_days),
          told_diagnosis_outcome_in_29_to_42_days = sum(told_diagnosis_outcome_in_29_to_42_days),
          told_diagnosis_outcome_in_43_to_62_days = sum(told_diagnosis_outcome_in_43_to_62_days),
          told_diagnosis_outcome_after_62_days = sum(told_diagnosis_outcome_after_62_days),
          periodNum = mean(periodNum),
          totalStaff = mean(totalStaff, na.rm = TRUE),
          totalAbsent = mean(totalAbsent, na.rm = TRUE),
          totalBedsOccupied = mean(totalBedsOccupied, na.rm = TRUE),
          date = first(date)
        )

      dataGroupedParticipating$percentage_not_told_diagnosis_outcome_within_28_days = (dataGroupedParticipating$num_not_told_diagnosis_outcome_within_28_days / dataGroupedParticipating$told_diagnosis_outcome_total)
      write.csv(dataGroupedParticipating, paste0(resultsDirPath, "unadjustedRates", suffix2, ".csv"), row.names = FALSE)
    }

    # load population by cancer alliance region data from 2020 which is used to calculate per 100000 population rates
    # this file was downloaded on July 22, 2024 from https://digital.nhs.uk/binaries/content/assets/website-assets/national-disease-registration-service/ncras/data-outputs/cancer-prevalence-statistics-england-2020_download.xlsx
    # pre-processing involved calculating regional population data using cancer prevalence counts and per population rates, then saving as csv
    file_path <- paste0(covariateDirPath, "Cancer+Prevalence+Statistics+England+2020_download_popByCA.csv")
    pop_df <- read.csv(file_path, skip = 1)

    dataGrouped <- merge(dataGrouped, pop_df[, c("Cancer.Alliance", "population")],
                         by = "Cancer.Alliance", all.x = TRUE)

    # Calculate rate of referrals per 100,000 population
    if (depVar == 'refRate') {
      dataGrouped$ref_rate <- (dataGrouped$told_diagnosis_outcome_total / dataGrouped$population) * 100000
    }

    # estimate average wait times from counts within the 5 wait times categories: 0-14; 15-28; 28-42; 43-62; 62+ days
    # each observation in each category is assumed to have a wait time exactly in the middle of that category
    # (e.g. all referrals in the 0-14 days category are assumed to have a wait time of (0+14)/2 = 7 days
    dataGrouped$est_wait_time <-
      (dataGrouped$told_diagnosis_outcome_within_14_days * 7) +
      (dataGrouped$in_15_to_28_days * 21.5) +
      (dataGrouped$told_diagnosis_outcome_in_29_to_42_days * 35.5) +
      (dataGrouped$told_diagnosis_outcome_in_43_to_62_days * 52.5) +
      (dataGrouped$told_diagnosis_outcome_after_62_days * 72.5) # Upper bound of category set to 82; (63+82)/2 = 72.5 days
    dataGrouped$est_wait_time <- (dataGrouped$est_wait_time / dataGrouped$told_diagnosis_outcome_total)

    # calculating percentage of staff absent
    dataGrouped$percentageAbsent = (dataGrouped$totalAbsent / dataGrouped$totalStaff)

    # calculating rate of bed occupancy by COVID-19 patients by 100000 population
    dataGrouped$bedOccupancyRate <- (dataGrouped$totalBedsOccupied / dataGrouped$population) * 100000

    # create a dummy variable for each time period, set to 1 when a row is in that time period and is for a cancer alliance region participating in the trial
    dataGrouped <- dataGrouped %>%
      mutate(dummy = ifelse(mced_treated == 1, 1, 0)) %>%
      pivot_wider(names_from = periodNum, values_from = dummy,
                  names_prefix = "dummy_period_", values_fill = list(dummy = 0)) %>%
      rename_with(~ sub("dummy_period_([0-9])$", "dummy_period_0\\1", .x)) %>%
      select(-dummy_period_01) # drop the dummy period for the first time period, this will serve as reference time period in event study analysis

    # Extracting dummy period columns and sort them by time period number
    dummy_cols <- grep("dummy_period_", names(dataGrouped), value = TRUE)

    # Ensure other columns are maintained in their original order
    non_dummy_cols <- setdiff(names(dataGrouped), dummy_cols)

    # Reorder the dummy columns according to numeric value sorting above
    dataGrouped <- dataGrouped %>%
      select(all_of(non_dummy_cols), all_of(sort(dummy_cols)))

    # Get all of the names of the dummy period variables
    dummy_vars <- names(dataGrouped)[grepl("dummy_period_", names(dataGrouped))]

    # set covariate for all but two sensitivity analyses to be percentage staff absent
    covariate <- "`percentageAbsent` + "

    if (sensitivity_analysis == "altCovariate") {
      covariate <- "`bedOccupancyRate` + " }

    else if (sensitivity_analysis == "noCovariate") {
      covariate <- "" }

    # Formula uses % patients facing diagnostic delay as outcome, percentageAbsent as covariate,
    # dummy variables as main independent variables of interest, and time period- and cancer alliance- fixed effects variables
    if (depVar == "delayRate") {
      depVarName <- "percentage_not_told_diagnosis_outcome_within_28_days"
      if (sensitivity_analysis == "14days") {
        depVarName <- "percentage_not_told_diagnosis_outcome_within_14_days"
        }
      else if (sensitivity_analysis == "42days") {
        depVarName <- "percentage_not_told_diagnosis_outcome_within_42_days"
        }
      else if (sensitivity_analysis == "62days") {
        depVarName <- "percentage_not_told_diagnosis_outcome_within_62_days"
        }
      formula_str <- paste(depVarName, " ~ ", covariate, paste(dummy_vars, collapse = " + "), "| Cancer.Alliance + period")
      }
    else if (depVar == "refRate") {
      formula_str <- paste("ref_rate ~ ", covariate, paste(dummy_vars, collapse = " + "), "| Cancer.Alliance + period")
      }
    else if (depVar == "estWaitTime") {
      formula_str <- paste("est_wait_time ~ ", covariate, paste(dummy_vars, collapse = " + "), "| Cancer.Alliance + period")
      }

    # Run model with no weights for sensitivity analysis
    if (sensitivity_analysis == "noWeights") {
      model = feols(fml= as.formula(formula_str),
                    data=dataGrouped,
                    cluster="Cancer.Alliance")
    }

    # Run model with propensity score weights for sensitivity analysis
    else if (sensitivity_analysis == "propensityScore") {

      # merge on propensity score weights
      psw_weight = read.csv(paste0(dirPath, "PS_wts_for_sensruns.csv"))
      dataGrouped = dataGrouped %>%
        left_join((psw_weight %>%
                     select(Cancer.Alliance = CancerAlliance,
                            psw)))
      print("Are there any Missing PSW weights `table(is.na(dataGrouped$psw))`?\n")
      print(table(is.na(dataGrouped$psw)))

      # running the model with PS weights
      model = feols(fml= as.formula(formula_str),
                    weights= ~psw,
                    data=dataGrouped,
                    cluster="Cancer.Alliance")
    }

    # Run model with wild bootstrapping for sensitivity analysis
    else if (sensitivity_analysis == "wildBootstrap") {

      # Skip acute leukaemia, which has too few referral observations to conduct wild bootstrap analysis
      if (cancers_included == "Suspected acute leukaemia") {
        next
      }

      # running the model
      model <- feols(
        fml = as.formula(formula_str),
        weights = ~told_diagnosis_outcome_total,
        data = (dataGrouped %>%
                  mutate(
                    Cancer.Alliance = as.factor(Cancer.Alliance),
                    period = as.factor(period)
                  )),
        cluster = ~Cancer.Alliance
      )

      # Wild cluster bootstrap using Rademacher weights
      out = list()

      # grab only the X variables from the formula (but not fixed effects)
      x_vars <- strsplit(strsplit(formula_str, "~|\\|")[[1]][2], "\\+")[[1]] %>%
        trimws() %>%                    # Remove whitespace
        gsub("`", "", .)

      # looping through the coeficients to adjust each p value
      for(var in x_vars){
        boot_res <- boottest(
          model,
          param = var,
          B = 999,
          clustid = "Cancer.Alliance",
          type = "rademacher"
        )
        out[[var]] = boot_res
      }
    }

    # Run default model for all other analyses (including primary analyses), weighted by total number of referrals that get diagnostic outcome, with errors clustered at cancer alliance level
    else {
      model = feols(fml= as.formula(formula_str),
                    weights= ~told_diagnosis_outcome_total,
                    data=dataGrouped,
                    cluster="Cancer.Alliance")
    }

    # When periodLength is set to 6m we want to start populating table results
    if (periodLength == "6m") {
      # Extract coefficients (estimates)
      estimates <- coef(model)

      # P-values: use bootstrap p-values and confidence intervals for wildBootstrap, otherwise use standard
      if (sensitivity_analysis == "wildBootstrap") {

        # browser()

        p_values <- sapply(out, function(x){x$p_val})
        conf_intervals <- bind_rows(lapply(out, confint, level = 0.95)) %>%
          t %>%
          as.data.frame %>%
          setNames(c("2.5 %", "97.5 %")) %>%
          as.matrix()
      } else {

        conf_intervals <- confint(model, level = 0.95)
        p_values <- coeftable(model)[, "Pr(>|t|)"]
      }

      # Select specific variables to capture in table
      variables_of_interest <- c("dummy_period_02", "dummy_period_03", "dummy_period_04", "dummy_period_05", "dummy_period_06", "dummy_period_07")

      # Create a data frame containing estimates and confidence intervals
      results_df <- data.frame(
        Estimate = estimates[variables_of_interest],
        P_Value = p_values[variables_of_interest],
        Lower = conf_intervals[variables_of_interest, "2.5 %"],
        Upper = conf_intervals[variables_of_interest, "97.5 %"]
      )

      # Convert to percentage points and format with two decimal places
      results_df$Formatted <- paste(
        sprintf("%.2f", results_df$Estimate * conversionFactor),
        " (",
        sprintf("%.2f", results_df$Lower * conversionFactor),
        "-",
        sprintf("%.2f", results_df$Upper * conversionFactor),
        sep = ""
      )

      if (cancers_included %in% c(names(cancer_groups))) {
        # Format p-values: if less than 0.001, use p<0.001, otherwise show exact value
        p_value_text <- ifelse(results_df$P_Value < 0.001,
                               "p<0.001",
                               paste0("p=", sprintf("%.3f", results_df$P_Value)))

        results_df$Formatted <- paste(results_df$Formatted,
                                      ", ",
                                      p_value_text,
                                      sep = ""
                                      )
      }

      results_df$Formatted <- paste(results_df$Formatted, ")", sep = "")

      results_df <- data.frame(Variable = rownames(results_df), results_df, check.names = FALSE)
      rownames(results_df) <- NULL  # Remove the original row names to avoid confusion

      results_df <- results_df %>%
        mutate(CancerType = case_when(
          Variable == "dummy_period_02" ~ "Months0-5",
          Variable == "dummy_period_03" ~ "Months6-11",
          Variable == "dummy_period_04" ~ "Months12-17",
          Variable == "dummy_period_05" ~ "Months18-23",
          Variable == "dummy_period_06" ~ "Months24-29",
          Variable == "dummy_period_07" ~ "Months30-35"
        ))

      results_transposed <- results_df %>%
        select(CancerType, Formatted) %>%  # select only necessary columns
        spread(key = CancerType, value = Formatted)

      # Add the new CancerType column and populate it
      results_transposed <- results_transposed %>%
        mutate(CancerType = cancers_included) %>%
        select(CancerType, "Months0-5", "Months6-11", "Months12-17", "Months18-23", "Months24-29", "Months30-35")  # Move CancerType to the first position

      overall_results <- rbind(overall_results, results_transposed)
    }

    if (periodLength == "1m" && cancers_included %in% c(names(cancer_groups)) && depVar == "delayRate" && sensitivity_analysis == "") {
      # Display model results in a figure where x axis is time periods and y axis is percentage point difference
      coefficients <- coef(model) * conversionFactor # Convert to percentage point scale
      standard_errors <- se(model) * conversionFactor # Convert to percentage point scale

      results <- data.frame(
        estimate = coefficients,
        se = standard_errors
      )

      results <- results[grep("dummy_period_", rownames(results)), ]
      results$period <- as.numeric(sub("dummy_period_", "", rownames(results)))

      # Set axis label in figure so that start of trial (Oct 2021) is 0, meaning pre-trial time periods are negative.
      results$period = results$period - period_adjustment

      # Add confidence intervals
      results$lower <- results$estimate - 1.96 * results$se
      results$upper <- results$estimate + 1.96 * results$se

      # Create and export figure plot as image file
      plot <- ggplot(results, aes(x = period)) +
        geom_line(aes(y = estimate), size = 1) +
        geom_line(aes(y = lower), linetype = "dotted", size = 0.5) +
        geom_line(aes(y = upper), linetype = "dotted", size = 0.5) +
        scale_y_continuous(labels = label_percent(scale = 1), limits = c(-12.4, 12.4)) +  # Format as percentage
        scale_x_continuous(limits = c(-6.7, 35.7), breaks = seq(-6, 35, by = 1), expand = c(0, 0)) +
        labs(
          x = "Length of time (months) relative to trial start",
          y = "Percentage points"
        ) +
        theme(panel.grid.minor.x = element_blank(),
              legend.position = "none",
              axis.title = element_text(size = 14),
              axis.title.x = element_text(margin = margin(t = 5)),
              axis.title.y = element_text(margin = margin(r = 0)),
              axis.text = element_text(size = 12))
      file_path <- paste0(resultsDirPath, "DiffInDiff_", gsub("/", "", cancers_included), suffix, ".png")

      ggsave(file_path, plot, width = 12, height = 4.5, units = "in", dpi = 300)
    }
  }

  if (periodLength == "6m") {
    # Read the linking file to get cancer type labels and order for tabular display in manuscript
    linking_file <- read.csv(paste0(linkingDirPath, "Results Table Labels and Order.csv"),
                             stringsAsFactors = FALSE)

    # Merge the overall_results with the linking file to get labels and order
    overall_results <- merge(overall_results,
                             linking_file[, c("CancerType", "CancerTypeLabel", "Order")],
                             by = "CancerType",
                             all.x = TRUE)

    # Replace CancerType with CancerTypeLabel
    overall_results$CancerType <- overall_results$CancerTypeLabel

    # Remove the CancerTypeLabel column as it's no longer needed
    overall_results <- overall_results %>% select(-CancerTypeLabel)

    # Sort by the Order column
    overall_results <- overall_results %>%
      arrange(Order) %>%
      select(-Order)  # Remove the Order column after sorting
    write.csv(overall_results, paste0(resultsDirPath, "overallResults", suffix, ".csv"), row.names = FALSE)
  }
}

Analysis(baseData, periodLength = "1m", sensitivity_analysis = "", depVar = "delayRate")

Analysis(baseData, periodLength = "6m", sensitivity_analysis = "", depVar = "delayRate") # produces overallResults_6m_delayRate.csv
Analysis(baseData, periodLength = "6m", sensitivity_analysis = "", depVar = "refRate") # produces overallResults_6m_refRate.csv

Analysis(baseData, periodLength = "6m", sensitivity_analysis = "washout", depVar = "delayRate") # produces overallResults_6m_washout_delayRate.csv
Analysis(baseData, periodLength = "6m", sensitivity_analysis = "washout", depVar = "refRate") # produces overallResults_6m_washout_refRate.csv

Analysis(baseData, periodLength = "6m", sensitivity_analysis = "altCovariate", depVar = "delayRate") # produces overallResults_6m_altCovariate_delayRate.csv
Analysis(baseData, periodLength = "6m", sensitivity_analysis = "altCovariate", depVar = "refRate") # produces overallResults_6m_altCovariate_refRate.csv

Analysis(baseData, periodLength = "6m", sensitivity_analysis = "noCovariate", depVar = "delayRate") # produces overallResults_6m_noCovariate_delayRate.csv
Analysis(baseData, periodLength = "6m", sensitivity_analysis = "noCovariate", depVar = "refRate") # produces overallResults_6m_noCovariate_refRate.csv

Analysis(baseData, periodLength = "6m", sensitivity_analysis = "noWeights", depVar = "delayRate") # produces overallResults_6m_noWeights_delayRate.csv
Analysis(baseData, periodLength = "6m", sensitivity_analysis = "noWeights", depVar = "refRate") # produces overallResults_6m_noWeights_refRate.csv

Analysis(baseData, periodLength = "6m", sensitivity_analysis = "wildBootstrap", depVar = "delayRate") # produces overallResults_6m_wildBootstrap_delayRate.csv
Analysis(baseData, periodLength = "6m", sensitivity_analysis = "wildBootstrap", depVar = "refRate") # produces overallResults_6m_wildBootstrap_refRate.csv

Analysis(baseData, periodLength = "6m", sensitivity_analysis = "propensityScore", depVar = "delayRate") # produces overallResults_6m_wildBootstrap_delayRate.csv
Analysis(baseData, periodLength = "6m", sensitivity_analysis = "propensityScore", depVar = "refRate") # produces overallResults_6m_wildBootstrap_refRate.csv

Analysis(baseData, periodLength = "6m", sensitivity_analysis = "14days", depVar = "delayRate") # produces overallResults_6m_14days_delayRate.csv
Analysis(baseData, periodLength = "6m", sensitivity_analysis = "42days", depVar = "delayRate") # produces overallResults_6m_42days_delayRate.csv
Analysis(baseData, periodLength = "6m", sensitivity_analysis = "62days", depVar = "delayRate") # produces overallResults_6m_62days_delayRate.csv

Analysis(baseData, periodLength = "6m", sensitivity_analysis = "", depVar = "estWaitTime") # produces overallResults_6m_estWaitTime.csv
