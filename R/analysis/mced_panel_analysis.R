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
library(patchwork)
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

# ---- Shared formatting constants (used by Analysis function + post-analysis figures) ----
sz_uniform <- 16
base_date  <- as.Date("2021-10-01")  # month 0 = Oct 2021

# X-axis labels for 3-panel figures (months -6 to 36)
breaks_vec   <- seq(-6, 36, by = 1)
month_dates  <- base_date %m+% months(breaks_vec)
month_abbrs  <- format(month_dates, "%b")
x_labels_2line <- ifelse(breaks_vec %% 2 == 0,
                         paste0(breaks_vec, "\n", month_abbrs), "")

# Alternate labels offset by 1 (odd months: Jan, Mar, May, Jul, ...)
x_labels_2line_alt <- ifelse(breaks_vec %% 2 != 0,
                             paste0(breaks_vec, "\n", month_abbrs), "")

# X-axis labels for DiffInDiff figures (months -6 to 35)
breaks_vec_did   <- seq(-6, 35, by = 1)
month_dates_did  <- base_date %m+% months(breaks_vec_did)
month_abbrs_did  <- format(month_dates_did, "%b")
x_labels_2line_did <- ifelse(breaks_vec_did %% 2 == 0,
                              paste0(breaks_vec_did, "\n", month_abbrs_did), "")

# Matching theme for facet_wrap figures (mirrors dots_panel_theme used by Figure 1)
facet_panel_theme <- theme_bw(base_size = sz_uniform) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    panel.border       = element_blank(),
    axis.line.x        = element_line(color = "black", linewidth = 0.4),
    axis.line.y        = element_line(color = "black", linewidth = 0.4),
    axis.title         = element_text(size = sz_uniform),
    axis.title.x       = element_text(margin = margin(t = 3)),
    axis.title.y       = element_text(margin = margin(r = 8)),
    axis.text          = element_text(size = sz_uniform),
    strip.text         = element_text(size = sz_uniform, face = "bold", hjust = 0),
    strip.background   = element_blank(),
    legend.text        = element_text(size = sz_uniform * 0.85),
    legend.key         = element_rect(fill = "white", color = NA),
    plot.title         = element_text(size = sz_uniform, margin = margin(b = 3))
  )

# Compute annotation positions proportional to y-axis range (matching Figure 1 ratios)
annotation_positions <- function(y_min, y_max) {
  yr <- y_max - y_min
  list(
    bracket_y     = y_min - 0.16 * yr,
    tick_y_top    = y_min - 0.13 * yr,
    label_y       = y_min - 0.21 * yr,
    panel_label_y = y_max + 0.05 * yr,
    panel_title_y = y_max + 0.06 * yr
  )
}

# Year brackets for DiffInDiff figures (x-range ends at 35 not 36)
year_brackets_did <- data.frame(
  year_label = c("2021", "2022", "2023", "2024"),
  x_start = c(-6, 3, 15, 27),
  x_end   = c(2, 14, 26, 35),
  stringsAsFactors = FALSE
)

# Common theme for JAMA-style manual panels (coord_cartesian + clip="off" for annotations)
dots_panel_theme <- theme_bw(base_size = sz_uniform) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black", linewidth = 0.4),
    axis.line.y = element_line(color = "black", linewidth = 0.4),
    axis.title = element_text(size = sz_uniform),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.text = element_text(size = sz_uniform),
    legend.position = "none",
    plot.title = element_text(size = sz_uniform, margin = margin(b = 3)),
    plot.margin = margin(t = 40, r = 12, b = 55, l = 8)
  )

# Panel labels and titles for 3-panel figures
panel_labels <- c("A", "B", "C")
panel_titles <- c("Primary high-detection group", "Expanded high-detection group", "Low-detection group")

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
  pretrend_slope_results <- data.frame()

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
              value = n_distinct(subset(data, mced_treated == 1)$Provider.Code))

    summary_stats <- summary_stats %>%
      add_row(label = paste0("Table 1, number of providers, not participating in trial", suffix),
              value = n_distinct(subset(data, mced_treated == 0)$Provider.Code))

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

    # IMD (Index of Multiple Deprivation) summary statistics for Table 1
    imd_table1 <- read.csv(paste0(dataDirPath, "imd_by_cancer_alliance.csv"))
    mced_indicator <- read.csv(paste0(linkingDirPath, "McedToCAMappingByHand.csv"))
    imd_table1 <- merge(imd_table1, mced_indicator, by = "Cancer.Alliance")

    # Pop-weighted average IMD score by treatment group
    for (treated_val in c(1, 0)) {
      group_label <- ifelse(treated_val == 1, "participating in trial", "not participating in trial")
      group_data <- subset(imd_table1, mced_treated == treated_val)

      pop_weighted_imd <- sum(group_data$imd_pop_weighted_avg * group_data$imd_population) /
        sum(group_data$imd_population)

      summary_stats <- summary_stats %>%
        add_row(label = paste0("Table 1, pop-weighted avg IMD score, ", group_label, suffix),
                value = pop_weighted_imd)

      # Pop-weighted % in each IMD quintile by treatment group
      for (q in 1:5) {
        q_col <- paste0("imd_q", q, "_pct")
        pop_weighted_q_pct <- sum(group_data[[q_col]] * group_data$imd_population) /
          sum(group_data$imd_population)

        summary_stats <- summary_stats %>%
          add_row(label = paste0("Table 1, pct population in IMD quintile ", q, ", ", group_label, suffix),
                  value = pop_weighted_q_pct)
      }
    }

    # Pop-weighted average IMD score by treatment group (50-77 age-weighted)
    for (treated_val in c(1, 0)) {
      group_label <- ifelse(treated_val == 1, "participating in trial", "not participating in trial")
      group_data <- subset(imd_table1, mced_treated == treated_val)

      pop_weighted_imd_5077 <- sum(group_data$imd_pop_weighted_avg_50_77 * group_data$imd_population_50_77) /
        sum(group_data$imd_population_50_77)

      summary_stats <- summary_stats %>%
        add_row(label = paste0("Table 1, pop-weighted avg IMD score (50-77), ", group_label, suffix),
                value = pop_weighted_imd_5077)

      # Pop-weighted % in each IMD quintile by treatment group (50-77)
      for (q in 1:5) {
        q_col <- paste0("imd_q", q, "_pct_50_77")
        pop_weighted_q_pct_5077 <- sum(group_data[[q_col]] * group_data$imd_population_50_77) /
          sum(group_data$imd_population_50_77)

        summary_stats <- summary_stats %>%
          add_row(label = paste0("Table 1, pct population in IMD quintile ", q, " (50-77), ", group_label, suffix),
                  value = pop_weighted_q_pct_5077)
      }
    }

    write.csv(summary_stats, paste0(resultsDirPath, "summaryStats.csv"), row.names = FALSE)

    # # # Generate provider list CSV # # #
    # Build provider lookup table: one row per provider
    provider_list <- data %>%
      select(Provider.Code, accountable_provider, Cancer.Alliance, mced_treated) %>%
      distinct() %>%
      group_by(Provider.Code) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(
        Region = gsub(" Cancer Alliance", "", Cancer.Alliance),
        `Participating in Trial` = ifelse(mced_treated == 1, "Yes", "No")
      ) %>%
      rename(
        Code = Provider.Code,
        `Provider Name` = accountable_provider
      ) %>%
      select(`Provider Name`, Code, Region, `Participating in Trial`) %>%
      arrange(desc(`Participating in Trial`), Region, `Provider Name`)

    provider_file <- paste0(resultsDirPath, "provider_list.csv")
    write.csv(provider_list, provider_file, row.names = FALSE)
    print(paste0("Provider list written to: ", provider_file))
  }

  startingData <- data

  # Accumulator for combining DiD plot data across cancer groups into a single figure
  did_plot_accumulator <- data.frame()

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

    # For monthly analyses of primary cancer groups, produce trend figures using unadjusted values
    if (periodLength == "1m" && cancers_included %in% c(names(cancer_groups)) && depVar %in% c("delayRate", "refRate") && sensitivity_analysis == "") {
    if (!SKIP_EXTRA_OUTPUTS) {
      trendData<-data %>%
        group_by(mced_treated, period) %>%
        summarise(
          num_not_told_diagnosis_outcome_within_28_days = sum(num_not_told_diagnosis_outcome_within_28_days),
          told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
          mced_treated = mean(mced_treated),
          periodNum = mean(periodNum),
          date = first(date)
        )

      if (depVar == "delayRate") {
        trendData$percentage_not_told_diagnosis_outcome_within_28_days = (trendData$num_not_told_diagnosis_outcome_within_28_days / trendData$told_diagnosis_outcome_total)

        trendData <- trendData %>%
          mutate(
            point_estimate = percentage_not_told_diagnosis_outcome_within_28_days,
            standard_error = sqrt((point_estimate * (1 - point_estimate)) / told_diagnosis_outcome_total),
            lower_ci = point_estimate - 1.96 * standard_error,
            upper_ci = point_estimate + 1.96 * standard_error,
            mced_treated = ifelse(mced_treated == 0, "Not participating in trial", "Participating in trial")
          )

        trend_y_scale <- scale_y_continuous(labels = scales::percent_format(), limits = c(0.10, 0.50))
        trend_y_label <- "Diagnostic delay rate"

      } else if (depVar == "refRate") {
        # Read population data and compute population totals by treatment group
        trend_pop_df <- read.csv(paste0(covariateDirPath, "Cancer+Prevalence+Statistics+England+2020_download_popByCA.csv"), skip = 1)
        ca_treated <- data %>% select(Cancer.Alliance, mced_treated) %>% distinct()
        pop_by_treated <- ca_treated %>%
          left_join(trend_pop_df[, c("Cancer.Alliance", "population")], by = "Cancer.Alliance") %>%
          group_by(mced_treated) %>%
          summarise(population = sum(population))

        trendData <- trendData %>%
          left_join(pop_by_treated, by = "mced_treated") %>%
          mutate(
            point_estimate = (told_diagnosis_outcome_total / population) * 100000,
            standard_error = (sqrt(told_diagnosis_outcome_total) / population) * 100000,
            lower_ci = point_estimate - 1.96 * standard_error,
            upper_ci = point_estimate + 1.96 * standard_error,
            mced_treated = ifelse(mced_treated == 0, "Not participating in trial", "Participating in trial")
          )

        trend_y_scale <- scale_y_continuous()
        trend_y_label <- "Referrals per 100,000 population"
      }

      # Set axis label in figure so that start of trial (Oct 2021) is 0, meaning pre-trial time periods are negative.
      trendData$periodNum = trendData$periodNum - period_adjustment

      trendPlot = ggplot(trendData, aes(x = periodNum, y = point_estimate, color = factor(mced_treated))) +
        geom_point(position = position_dodge(width = 0.25), size = 0.35) +
        geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(width = 0.25)) +
        scale_x_continuous(limits = c(-6.7, 14.7), breaks = seq(-6, 14, by = 1), expand = c(0, 0)) +
        trend_y_scale +
        labs(
          x = "Length of time (months) relative to trial start",
          y = trend_y_label
        ) +
        theme(panel.grid.minor.x = element_blank(),
              legend.position = "none",
              axis.title = element_text(size = 14),
              axis.title.x = element_text(margin = margin(t = 5)),
              axis.title.y = element_text(margin = margin(r = 8)),
              axis.text = element_text(size = 12))

      file_path <- paste0(resultsDirPath, "TrendPlot_", gsub("/", "", cancers_included), suffix, ".png")
      ggsave(file_path, trendPlot, width = 12, height = 4.5, units = "in", dpi = 300)
      ggsave(sub("\\.png$", ".eps", file_path), trendPlot, width = 12, height = 4.5, units = "in", device = "eps")

      # --- Spaghetti-plus-mean plot: individual Cancer Alliance trajectories + group means ---
      # Aggregate to Cancer.Alliance x month level
      spaghettiData <- data %>%
        group_by(Cancer.Alliance, monthNum) %>%
        summarise(
          num_not_told_diagnosis_outcome_within_28_days = sum(num_not_told_diagnosis_outcome_within_28_days),
          told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
          mced_treated = mean(mced_treated),
          .groups = "drop"
        )

      if (depVar == "delayRate") {
        spaghettiData <- spaghettiData %>%
          mutate(outcome = num_not_told_diagnosis_outcome_within_28_days / told_diagnosis_outcome_total)
        spaghetti_y_scale <- scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.0), expand = c(0, 0))
        spaghetti_y_label <- "Diagnostic delay rate"
      } else if (depVar == "refRate") {
        spaghetti_pop_df <- read.csv(paste0(covariateDirPath, "Cancer+Prevalence+Statistics+England+2020_download_popByCA.csv"), skip = 1)
        spaghettiData <- spaghettiData %>%
          left_join(spaghetti_pop_df[, c("Cancer.Alliance", "population")], by = "Cancer.Alliance") %>%
          mutate(outcome = (told_diagnosis_outcome_total / population) * 100000)
        spaghetti_y_scale <- scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA))
        spaghetti_y_label <- "Referrals per 100,000 population"
      }

      spaghettiData <- spaghettiData %>%
        mutate(
          periodNum = monthNum - period_adjustment,
          group = ifelse(mced_treated == 1, "Participating in trial", "Not participating in trial")
        )

      # Compute group means per month
      spaghettiMeans <- spaghettiData %>%
        group_by(group, periodNum) %>%
        summarise(outcome = mean(outcome), .groups = "drop")

      spaghettiPlot <- ggplot() +
        # Individual region trajectories (spaghetti)
        geom_line(data = spaghettiData,
                  aes(x = periodNum, y = outcome, group = Cancer.Alliance, color = group),
                  alpha = 0.25, linewidth = 0.4) +
        # Group mean lines
        geom_line(data = spaghettiMeans,
                  aes(x = periodNum, y = outcome, color = group),
                  linewidth = 1.2) +
        # Vertical line at trial start
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
        scale_color_manual(
          values = c("Participating in trial" = "#E69F00", "Not participating in trial" = "#56B4E9"),
          name = NULL
        ) +
        scale_x_continuous(limits = c(-6.7, 36.7), breaks = seq(-6, 36, by = 2), expand = c(0, 0)) +
        spaghetti_y_scale +
        labs(
          x = "Length of time (months) relative to trial start",
          y = spaghetti_y_label
        ) +
        theme(panel.grid.minor.x = element_blank(),
              legend.position = "bottom",
              axis.title = element_text(size = 14),
              axis.title.x = element_text(margin = margin(t = 5)),
              axis.title.y = element_text(margin = margin(r = 8)),
              axis.text = element_text(size = 12))

      file_path <- paste0(resultsDirPath, "SpaghettiPlot_", gsub("/", "", cancers_included), suffix, ".png")
      ggsave(file_path, spaghettiPlot, width = 12, height = 4.5, units = "in", dpi = 300)
      ggsave(sub("\\.png$", ".eps", file_path), spaghettiPlot, width = 12, height = 4.5, units = "in", device = "eps")

      # --- Dots-plus-mean plot: individual Cancer Alliance values as points + group means ---
      # Uses the same spaghettiData and spaghettiMeans computed above
      dotsPlot <- ggplot() +
        # Individual region values as dodged dots (blue left, orange right)
        geom_point(data = spaghettiData,
                   aes(x = periodNum, y = outcome, color = group),
                   alpha = 0.3, size = 1.0,
                   position = position_dodge(width = 0.6)) +
        # Group mean lines (centered, no dodge)
        geom_line(data = spaghettiMeans,
                  aes(x = periodNum, y = outcome, color = group),
                  linewidth = 0.8) +
        # Group mean points (centered, no dodge)
        geom_point(data = spaghettiMeans,
                   aes(x = periodNum, y = outcome, fill = group),
                   shape = 21, size = 1.4, color = "black", stroke = 0.38, alpha = 0.6) +
        # Vertical line at trial start (between month -1 and month 0)
        geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey40", linewidth = 0.5) +
        scale_color_manual(
          values = c("Participating in trial" = "#E69F00", "Not participating in trial" = "#56B4E9"),
          name = NULL
        ) +
        scale_x_continuous(limits = c(-6.7, 36.7), breaks = seq(-6, 36, by = 2), expand = c(0, 0)) +
        spaghetti_y_scale +
        labs(
          x = "Length of time (months) relative to trial start",
          y = spaghetti_y_label
        ) +
        theme(panel.grid.minor.x = element_blank(),
              legend.position = "bottom",
              axis.title = element_text(size = 14),
              axis.title.x = element_text(margin = margin(t = 5)),
              axis.title.y = element_text(margin = margin(r = 8)),
              axis.text = element_text(size = 12))


      file_path <- paste0(resultsDirPath, "DotsPlot_", gsub("/", "", cancers_included), suffix, ".png")
      ggsave(file_path, dotsPlot, width = 12, height = 4.5, units = "in", dpi = 300)
      ggsave(sub("\\.png$", ".eps", file_path), dotsPlot, width = 12, height = 4.5, units = "in", device = "eps")
    }
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

    # JOSHUA- Sean- the next two code chunks (dataGrouped <- dataGrouped ...) seem redundant. Can we consolidate this?
    # we could just skip the aggregation by monthNum and only use periodNum which would handle the 6 month and 1 month cases
    # This is relevant in a few spots in this file.

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

    # Compute total referrals across all Cancer Alliances and all periods for this cancer type/group
    total_referrals <- dataGrouped %>%
      ungroup() %>%
      summarise(total = sum(told_diagnosis_outcome_total, na.rm = TRUE)) %>%
      pull(total)

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

    # Merge IMD (Index of Multiple Deprivation) measures by cancer alliance
    imd_df <- read.csv(paste0(dataDirPath, "imd_by_cancer_alliance.csv"))
    dataGrouped <- merge(dataGrouped, imd_df, by = "Cancer.Alliance", all.x = TRUE)

    # Calculate rate of referrals per 100,000 population
    if (depVar == 'refRate') {
      dataGrouped$ref_rate <- (dataGrouped$told_diagnosis_outcome_total / dataGrouped$population) * 100000
    }

    # Calculate and export unadjusted referral rates for treated and untreated groups of regions
    if (periodLength == "6m" && depVar == "refRate" && sensitivity_analysis == "" && cancers_included %in% names(cancer_groups)) {
      dataGroupedRef <- dataGrouped %>%
        group_by(mced_treated, period) %>%
        summarise(
          told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
          population = sum(population),
          .groups = "drop"
        )
      dataGroupedRef$ref_rate <- (dataGroupedRef$told_diagnosis_outcome_total / dataGroupedRef$population) * 100000
      write.csv(dataGroupedRef, paste0(resultsDirPath, "unadjustedRates", suffix2, ".csv"), row.names = FALSE)
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

    # Compute raw unadjusted pre-trial baseline rate for participating regions
    if (depVar == "delayRate") {
      rateColName <- "percentage_not_told_diagnosis_outcome_within_28_days"
      if (sensitivity_analysis == "14days") {
        rateColName <- "percentage_not_told_diagnosis_outcome_within_14_days"
      } else if (sensitivity_analysis == "42days") {
        rateColName <- "percentage_not_told_diagnosis_outcome_within_42_days"
      } else if (sensitivity_analysis == "62days") {
        rateColName <- "percentage_not_told_diagnosis_outcome_within_62_days"
      }
      weightColName <- "told_diagnosis_outcome_total"
    } else if (depVar == "refRate") {
      rateColName <- "ref_rate"
      weightColName <- "population"
    } else if (depVar == "estWaitTime") {
      rateColName <- "est_wait_time"
      weightColName <- "told_diagnosis_outcome_total"
    }

    pretrial_baseline <- dataGrouped %>%
      filter(mced_treated == 1, periodNum == 1) %>%
      summarise(rate = weighted.mean(.data[[rateColName]], .data[[weightColName]], na.rm = TRUE)) %>%
      pull(rate) * conversionFactor

    # create a dummy variable for each time period, set to 1 when a row is in that time period and is for a cancer alliance region participating in the trial
    dataGrouped <- dataGrouped %>%
      mutate(dummy = ifelse(mced_treated == 1, 1, 0)) %>%
      pivot_wider(names_from = periodNum, values_from = dummy,
                  names_prefix = "dummy_period_", values_fill = list(dummy = 0)) %>%
      rename_with(~ sub("dummy_period_([0-9])$", "dummy_period_0\\1", .x)) %>%
      # Determine reference period to drop (default: period 01; ref-N sensitivity analyses use month -N as reference)
      {
        ref_period_to_drop <- "dummy_period_01"
        if (grepl("^ref-", sensitivity_analysis)) {
          ref_period_to_drop <- sprintf("dummy_period_%02d", 7 - as.numeric(sub("ref-", "", sensitivity_analysis)))
        }
        select(., -all_of(ref_period_to_drop))
      }

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
      psw_weight = read.csv(paste0(dataDirPath, "PS_wts_for_sensruns.csv"))
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

    # Run model with IMD-based propensity score weights for sensitivity analysis
    else if (sensitivity_analysis == "propensityScoreIMD") {

      # merge on IMD propensity score weights
      psw_imd_weight = read.csv(paste0(dataDirPath, "PS_wts_IMD_for_sensruns.csv"))
      dataGrouped = dataGrouped %>%
        left_join((psw_imd_weight %>%
                     select(Cancer.Alliance = CancerAlliance,
                            psw_imd)))
      print("Are there any Missing IMD PSW weights?\n")
      print(table(is.na(dataGrouped$psw_imd)))

      # running the model with IMD PS weights
      model = feols(fml= as.formula(formula_str),
                    weights= ~psw_imd,
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
      weight_var <- if (depVar == "refRate") ~population else ~told_diagnosis_outcome_total
      model <- feols(
        fml = as.formula(formula_str),
        weights = weight_var,
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

    # Run default model for all other analyses (including primary analyses),
    # weighted by referral volume (for delay rates) or population (for referral rates), with errors clustered at cancer alliance level
    else {
      weight_var <- if (depVar == "refRate") ~population else ~told_diagnosis_outcome_total
      model = feols(fml= as.formula(formula_str),
                    weights= weight_var,
                    data=dataGrouped,
                    cluster="Cancer.Alliance")
    }

    # When periodLength is set to 6m we want to start populating table results
    if (periodLength == "6m") {
      # Extract coefficients (estimates)
      estimates <- coef(model)

      # P-values: use bootstrap p-values and confidence intervals for wildBootstrap, otherwise use standard
      if (sensitivity_analysis == "wildBootstrap") {

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
        # Format p-values: no leading zero, 2 decimals (3 if <.01), <.001 for very small
        p_value_text <- ifelse(results_df$P_Value < 0.001,
                               "p<.001",
                               ifelse(results_df$P_Value < 0.01,
                                      paste0("p=", sub("^0", "", sprintf("%.3f", results_df$P_Value))),
                                      paste0("p=", sub("^0", "", sprintf("%.2f", results_df$P_Value)))))

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
        mutate(CancerType = cancers_included)

      # Conditionally add Total No. of Referrals and Pre-Trial Baseline columns
      if (depVar %in% c("delayRate", "estWaitTime")) {
        results_transposed <- results_transposed %>%
          mutate(
            `Total No. of Referrals` = format(total_referrals, big.mark = ",", trim = TRUE),
            `Pre-Trial Baseline` = sprintf("%.1f", pretrial_baseline)
          ) %>%
          select(CancerType, `Total No. of Referrals`, `Pre-Trial Baseline`, "Months0-5", "Months6-11", "Months12-17", "Months18-23", "Months24-29", "Months30-35")
      } else {
        results_transposed <- results_transposed %>%
          mutate(`Pre-Trial Baseline` = sprintf("%.1f", pretrial_baseline)) %>%
          select(CancerType, `Pre-Trial Baseline`, "Months0-5", "Months6-11", "Months12-17", "Months18-23", "Months24-29", "Months30-35")
      }

      overall_results <- rbind(overall_results, results_transposed)
    }

    if (periodLength == "1m" && cancers_included %in% c(names(cancer_groups)) && depVar %in% c("delayRate", "refRate") && (sensitivity_analysis == "" | grepl("^ref-", sensitivity_analysis))) {

      # --- Pre-trend slope test (Miller 2023, JEP, footnote 11) ---
      # Regress pre-period event-study gammas on event time j and test H0: slope = 0.
      # A significant slope indicates violation of the parallel trends assumption.
      if (sensitivity_analysis == "") {

        all_coefs <- coef(model)
        V_model <- vcov(model)

        # Pre-period dummy names (months -5 to -1; month -6 is the omitted reference)
        pre_names <- paste0("dummy_period_0", 2:6)

        # Full 6-element pre-period coefficient vector: gamma_{-6}=0, gamma_{-5},...,gamma_{-1}
        pre_coefs_full <- c(0, all_coefs[pre_names])

        # Full 6x6 pre-period vcov (zero row/column for omitted reference period)
        V_pre_full <- matrix(0, nrow = 6, ncol = 6)
        V_pre_full[2:6, 2:6] <- V_model[pre_names, pre_names]

        # OLS slope weights over event times j = -6, -5, ..., -1
        j_all <- -6:-1
        j_bar <- mean(j_all)           # = -3.5
        ss_j <- sum((j_all - j_bar)^2) # = 17.5
        w <- (j_all - j_bar) / ss_j

        # Slope estimate and SE via delta method
        slope_est <- sum(w * pre_coefs_full)
        slope_var <- as.numeric(t(w) %*% V_pre_full %*% w)
        slope_se <- sqrt(slope_var)

        # t-test with df = n_clusters - 2 (t-distribution for few-cluster inference)
        n_clusters <- n_distinct(dataGrouped$Cancer.Alliance)
        slope_df <- n_clusters - 2
        slope_tstat <- slope_est / slope_se
        slope_pval <- 2 * pt(abs(slope_tstat), df = slope_df, lower.tail = FALSE)

        # Accumulate result (scale to percentage points / per-100k for reporting)
        pretrend_slope_results <- rbind(pretrend_slope_results, data.frame(
          cancer_group = cancers_included,
          dep_var = depVar,
          slope_per_month = slope_est * conversionFactor,
          slope_se = slope_se * conversionFactor,
          t_statistic = slope_tstat,
          df = slope_df,
          p_value = slope_pval,
          n_clusters = n_clusters,
          stringsAsFactors = FALSE
        ))

        cat(sprintf(
          "\n  Pre-trend slope test (%s, %s): slope = %.4f/month, SE = %.4f, t(%d) = %.3f, p = %.4f\n",
          cancers_included, depVar,
          slope_est * conversionFactor, slope_se * conversionFactor,
          slope_df, slope_tstat, slope_pval
        ))
      }

      # --- Standard normalization (single reference period pinned to 0) ---
      # Display model results in a figure where x axis is time periods and y axis is percentage point difference
      coefficients <- coef(model) * conversionFactor # Convert to percentage point scale
      standard_errors <- se(model) * conversionFactor # Convert to percentage point scale

      results <- data.frame(
        estimate = coefficients,
        se = standard_errors
      )

      results <- results[grep("dummy_period_", rownames(results)), ]
      results$period <- as.numeric(sub("dummy_period_", "", rownames(results)))

      # Add the omitted reference period as zero (it is the normalization point by construction)
      ref_periodNum <- 1  # default reference period
      if (grepl("^ref-", sensitivity_analysis)) {
        ref_periodNum <- 7 - as.numeric(sub("ref-", "", sensitivity_analysis))
      }
      results <- rbind(results, data.frame(estimate = 0, se = 0, period = ref_periodNum, row.names = "ref"))

      # Set axis label in figure so that start of trial (Oct 2021) is 0, meaning pre-trial time periods are negative.
      results$period = results$period - period_adjustment

      # Add confidence intervals
      results$lower <- results$estimate - 1.96 * results$se
      results$upper <- results$estimate + 1.96 * results$se

      # Accumulate results for combined DiD plot
      results$cancer_group <- cancers_included
      # Compute baseline (reference period) rate for treated group to enable % change figures
      ref_periodNum <- 1
      if (grepl("^ref-", sensitivity_analysis)) {
        ref_periodNum <- 7 - as.numeric(sub("ref-", "", sensitivity_analysis))
      }
      baseline_treated <- dataGrouped %>%
        filter(mced_treated == 1, as.numeric(period) == ref_periodNum)
      if (depVar == "refRate") {
        results$baseline_rate <- weighted.mean(baseline_treated$ref_rate,
                                               baseline_treated$told_diagnosis_outcome_total)
      } else if (depVar == "delayRate") {
        results$baseline_rate <- weighted.mean(baseline_treated$percentage_not_told_diagnosis_outcome_within_28_days,
                                               baseline_treated$told_diagnosis_outcome_total) * 100
      }
      did_plot_accumulator <- rbind(did_plot_accumulator, results)
    }
  }

  # Create combined DiD figure from accumulated data across cancer groups
  if (nrow(did_plot_accumulator) > 0) {
    group_labels <- c(
      "primary_high_detection" = "Primary high-detection",
      "sec_exp_high_detection" = "Expanded high-detection",
      "sec_low_detection" = "Low-detection"
    )
    did_plot_accumulator$group_label <- factor(
      group_labels[did_plot_accumulator$cancer_group],
      levels = group_labels
    )

    if (depVar == "delayRate") {
      did_y_label <- "Diagnostic delay rate, percentage points"
      did_y_scale <- scale_y_continuous(labels = label_percent(scale = 1), limits = c(-13, 13))
    } else if (depVar == "refRate") {
      did_y_label <- "Referrals per 100,000 population"
      did_y_scale <- scale_y_continuous()
    }

    group_colors <- c(
      "Primary high-detection" = "#08306B",
      "Expanded high-detection" = "#6BAED6",
      "Low-detection" = "#D55E00"
    )

    if (!SKIP_EXTRA_OUTPUTS) {
    did_combined_plot <- ggplot(did_plot_accumulator, aes(x = period)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
      geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey30", linewidth = 0.5) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = group_label), alpha = 0.15, color = NA) +
      geom_line(aes(y = estimate, color = group_label), linewidth = 0.8) +
      geom_point(aes(y = estimate, fill = group_label), shape = 21, size = 1.4,
                 color = "black", stroke = 0.38, alpha = 0.6) +
      scale_color_manual(values = group_colors, name = NULL) +
      scale_fill_manual(values = group_colors, name = NULL) +
      did_y_scale +
      scale_x_continuous(limits = c(-6.7, 35.7),
                         breaks = seq(-6, 35, by = 1),
                         labels = ifelse(seq(-6, 35, by = 1) %% 2 == 0, seq(-6, 35, by = 1), ""),
                         expand = c(0, 0)) +
      labs(
        x = "Length of time (months) relative to trial start",
        y = did_y_label
      ) +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            legend.position = "bottom",
            axis.title = element_text(size = 14),
            axis.title.x = element_text(margin = margin(t = 5)),
            axis.title.y = element_text(margin = margin(r = 0)),
            axis.text = element_text(size = 12))

    file_path <- paste0(resultsDirPath, "DiffInDiff_combinedDotsLinesAndShadedArea", suffix, ".png")
    ggsave(file_path, did_combined_plot, width = 12, height = 4.5, units = "in", dpi = 300)
    ggsave(sub("\\.png$", ".eps", file_path), did_combined_plot, width = 12, height = 4.5, units = "in", device = "eps")
    }

    if (!SKIP_EXTRA_OUTPUTS) {
    # Alternate combined figure: dots for estimates, vertical error bars with whiskers for 95% CI,
    # three groups slightly offset horizontally (dodged)
    dodge_width <- 0.45

    did_dots_bars_plot <- ggplot(did_plot_accumulator, aes(x = period, color = group_label)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
      geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey30", linewidth = 0.5) +
      geom_errorbar(aes(ymin = lower, ymax = upper),
                    width = 0, linewidth = 0.4,
                    position = position_dodge(width = dodge_width)) +
      geom_point(aes(y = estimate, fill = group_label), shape = 21, size = 1.6,
                 stroke = 0.38, color = "black",
                 position = position_dodge(width = dodge_width)) +
      scale_color_manual(values = group_colors, name = NULL) +
      scale_fill_manual(values = group_colors, name = NULL) +
      did_y_scale +
      scale_x_continuous(limits = c(-6.7, 35.7),
                         breaks = seq(-6, 35, by = 1),
                         labels = ifelse(seq(-6, 35, by = 1) %% 2 == 0, seq(-6, 35, by = 1), ""),
                         expand = c(0, 0)) +
      labs(
        x = "Length of time (months) relative to trial start",
        y = did_y_label
      ) +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            legend.position = "bottom",
            axis.title = element_text(size = 14),
            axis.title.x = element_text(margin = margin(t = 5)),
            axis.title.y = element_text(margin = margin(r = 0)),
            axis.text = element_text(size = 12))

    file_path <- paste0(resultsDirPath, "DiffInDiff_combinedDotsAndBars", suffix, ".png")
    ggsave(file_path, did_dots_bars_plot, width = 12, height = 4.5, units = "in", dpi = 300)
    ggsave(sub("\\.png$", ".eps", file_path), did_dots_bars_plot, width = 12, height = 4.5, units = "in", device = "eps")
    }

    # Alternate combined figure: three-panel faceted plot, one panel per cancer group,
    # with shared y-axis scale for comparison. Uses dots + error bars style.
    group_colors_3panel <- c(
      "Primary high-detection" = "#009E73",
      "Expanded high-detection" = "#0072B2",
      "Low-detection" = "#D55E00"
    )

    # Build individual DiffInDiff panels (manual assembly matching Figure 1 style)
    group_levels_did <- levels(did_plot_accumulator$group_label)

    # Compute shared y-range for all panels
    if (depVar == "delayRate") {
      did_y_min <- -13; did_y_max <- 13
    } else {
      did_y_min <- min(did_plot_accumulator$lower, na.rm = TRUE)
      did_y_max <- max(did_plot_accumulator$upper, na.rm = TRUE)
      y_pad <- (did_y_max - did_y_min) * 0.05
      did_y_min <- did_y_min - y_pad
      did_y_max <- did_y_max + y_pad
    }

    build_did_panel <- function(i) {
      glabel <- group_levels_did[i]
      gdata <- did_plot_accumulator %>% filter(group_label == glabel)

      pos <- annotation_positions(did_y_min, did_y_max)
      sz_annot <- sz_uniform / ggplot2::.pt

      p <- ggplot(gdata, aes(x = period)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
        geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey30", linewidth = 0.5) +
        geom_errorbar(aes(ymin = lower, ymax = upper), color = group_colors_3panel[glabel],
                      width = 0.4, linewidth = 0.4) +
        geom_point(aes(y = estimate), fill = group_colors_3panel[glabel], shape = 21, size = 1.6,
                   stroke = 0.38, color = "black") +
        scale_x_continuous(limits = c(-6.7, 35.7), breaks = breaks_vec_did,
                           labels = x_labels_2line_did, expand = c(0, 0)) +
        coord_cartesian(ylim = c(did_y_min, did_y_max), clip = "off") +
        labs(title = NULL, x = NULL, y = did_y_label) +
        dots_panel_theme

      # Y-axis labels (without limits, since coord_cartesian handles visible range)
      if (depVar == "delayRate") {
        p <- p + scale_y_continuous(labels = label_percent(scale = 1), expand = c(0, 0))
      } else {
        p <- p + scale_y_continuous(expand = c(0, 0))
      }

      # Panel label and title
      p <- p +
        annotate("label", x = -6.5, y = pos$panel_label_y, label = panel_labels[i],
                 fontface = "bold", size = sz_annot, hjust = 0, vjust = 0,
                 linewidth = 0.4, label.r = unit(0, "lines"),
                 fill = "white", color = "black",
                 label.padding = unit(0.2, "lines")) +
        annotate("text", x = -5.3, y = pos$panel_title_y, label = panel_titles[i],
                 fontface = "plain", size = sz_annot, hjust = 0, vjust = 0)

      # Year brackets
      for (b in seq_len(nrow(year_brackets_did))) {
        xs <- year_brackets_did$x_start[b]
        xe <- year_brackets_did$x_end[b]
        xm <- (xs + xe) / 2
        p <- p + annotate("segment", x = xs, xend = xe, y = pos$bracket_y, yend = pos$bracket_y,
                           color = "black", linewidth = 0.4)
        p <- p + annotate("segment", x = xs, xend = xs, y = pos$bracket_y, yend = pos$tick_y_top,
                           color = "black", linewidth = 0.4)
        p <- p + annotate("segment", x = xe, xend = xe, y = pos$bracket_y, yend = pos$tick_y_top,
                           color = "black", linewidth = 0.4)
        p <- p + annotate("text", x = xm, y = pos$label_y, label = year_brackets_did$year_label[b],
                           size = sz_annot, fontface = "plain")
      }

      p
    }

    did_panels <- lapply(seq_along(group_levels_did), build_did_panel)
    did_3panel_plot <- wrap_plots(did_panels, ncol = 1)

    fig3_prefix <- ifelse(sensitivity_analysis == "" && depVar == "delayRate", "Figure3_", "SuppFigure_")
    file_path <- paste0(resultsDirPath, fig3_prefix, "DiffInDiff_Combined3Panel", suffix, ".png")
    ggsave(file_path, did_3panel_plot, width = 18, height = 20, units = "in", dpi = 300)
    ggsave(sub("\\.png$", ".eps", file_path), did_3panel_plot, width = 18, height = 20, units = "in", device = "eps")

    # Alternate 3-panel figure for refRate: convert DiD estimates to % change from baseline
    if (!SKIP_EXTRA_OUTPUTS) {
    if (depVar == "refRate" && "baseline_rate" %in% names(did_plot_accumulator)) {
      pct_change_data <- did_plot_accumulator %>%
        mutate(
          pct_estimate = (estimate / baseline_rate) * 100,
          pct_lower    = (lower / baseline_rate) * 100,
          pct_upper    = (upper / baseline_rate) * 100
        )

      did_3panel_pct_plot <- ggplot(pct_change_data, aes(x = period, color = group_label)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
        geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey30", linewidth = 0.5) +
        geom_errorbar(aes(ymin = pct_lower, ymax = pct_upper),
                      width = 0.4, linewidth = 0.4) +
        geom_point(aes(y = pct_estimate, fill = group_label), shape = 21, size = 1.6,
                   stroke = 0.38, color = "black") +
        scale_color_manual(values = group_colors_3panel, name = NULL) +
        scale_fill_manual(values = group_colors_3panel, name = NULL) +
        scale_y_continuous(labels = function(x) paste0(x, "%")) +
        scale_x_continuous(limits = c(-6.7, 35.7),
                           breaks = seq(-6, 35, by = 1),
                           labels = ifelse(seq(-6, 35, by = 1) %% 2 == 0, seq(-6, 35, by = 1), ""),
                           expand = c(0, 0)) +
        facet_wrap(~ group_label, ncol = 1) +
        labs(
          x = "Length of time (months) relative to trial start",
          y = "% change in referral rate from baseline"
        ) +
        theme(panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              legend.position = "none",
              strip.text = element_text(size = 12, face = "bold", hjust = 0),
              axis.title = element_text(size = 14),
              axis.title.x = element_text(margin = margin(t = 5)),
              axis.title.y = element_text(margin = margin(r = 0)),
              axis.text = element_text(size = 12))

      pct_suffix <- sub("refRate", "refRatePctChange", suffix)
      file_path <- paste0(resultsDirPath, "DiffInDiff_Combined3Panel", pct_suffix, ".png")
      ggsave(file_path, did_3panel_pct_plot, width = 12, height = 10, units = "in", dpi = 300)
      ggsave(sub("\\.png$", ".eps", file_path), did_3panel_pct_plot, width = 12, height = 10, units = "in", device = "eps")

      # Fixed y-axis version: +/- 25%
      did_3panel_pct_fixed_plot <- did_3panel_pct_plot +
        scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-40, 40))

      pct_fixed_suffix <- sub("refRate", "refRatePctChangeFixed", suffix)
      file_path <- paste0(resultsDirPath, "DiffInDiff_Combined3Panel", pct_fixed_suffix, ".png")
      ggsave(file_path, did_3panel_pct_fixed_plot, width = 12, height = 10, units = "in", dpi = 300)
      ggsave(sub("\\.png$", ".eps", file_path), did_3panel_pct_fixed_plot, width = 12, height = 10, units = "in", device = "eps")
    }
    }

    # Alternate 3-panel figure for delayRate: convert DiD estimates to % change from baseline
    if (!SKIP_EXTRA_OUTPUTS) {
    if (depVar == "delayRate" && "baseline_rate" %in% names(did_plot_accumulator)) {
      pct_change_data <- did_plot_accumulator %>%
        mutate(
          pct_estimate = (estimate / baseline_rate) * 100,
          pct_lower    = (lower / baseline_rate) * 100,
          pct_upper    = (upper / baseline_rate) * 100
        )

      did_3panel_pct_plot <- ggplot(pct_change_data, aes(x = period, color = group_label)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
        geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey30", linewidth = 0.5) +
        geom_errorbar(aes(ymin = pct_lower, ymax = pct_upper),
                      width = 0.4, linewidth = 0.4) +
        geom_point(aes(y = pct_estimate, fill = group_label), shape = 21, size = 1.6,
                   stroke = 0.38, color = "black") +
        scale_color_manual(values = group_colors_3panel, name = NULL) +
        scale_fill_manual(values = group_colors_3panel, name = NULL) +
        scale_y_continuous(labels = function(x) paste0(x, "%")) +
        scale_x_continuous(limits = c(-6.7, 35.7),
                           breaks = seq(-6, 35, by = 1),
                           labels = ifelse(seq(-6, 35, by = 1) %% 2 == 0, seq(-6, 35, by = 1), ""),
                           expand = c(0, 0)) +
        facet_wrap(~ group_label, ncol = 1) +
        labs(
          x = "Length of time (months) relative to trial start",
          y = "% change in delay rate from baseline"
        ) +
        theme(panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              legend.position = "none",
              strip.text = element_text(size = 12, face = "bold", hjust = 0),
              axis.title = element_text(size = 14),
              axis.title.x = element_text(margin = margin(t = 5)),
              axis.title.y = element_text(margin = margin(r = 0)),
              axis.text = element_text(size = 12))

      pct_suffix <- sub("delayRate", "delayRatePctChange", suffix)
      file_path <- paste0(resultsDirPath, "DiffInDiff_Combined3Panel", pct_suffix, ".png")
      ggsave(file_path, did_3panel_pct_plot, width = 12, height = 10, units = "in", dpi = 300)
      ggsave(sub("\\.png$", ".eps", file_path), did_3panel_pct_plot, width = 12, height = 10, units = "in", device = "eps")

      # Fixed y-axis version: +/- 25%
      did_3panel_pct_fixed_plot <- did_3panel_pct_plot +
        scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-40, 40))

      pct_fixed_suffix <- sub("delayRate", "delayRatePctChangeFixed", suffix)
      file_path <- paste0(resultsDirPath, "DiffInDiff_Combined3Panel", pct_fixed_suffix, ".png")
      ggsave(file_path, did_3panel_pct_fixed_plot, width = 12, height = 10, units = "in", dpi = 300)
      ggsave(sub("\\.png$", ".eps", file_path), did_3panel_pct_fixed_plot, width = 12, height = 10, units = "in", device = "eps")
    }
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
    write.csv(overall_results, paste0(resultsDirPath, "SuppTable_overallResults", suffix, ".csv"), row.names = FALSE)
  }

  # Export pre-trend slope test results (Miller 2023)
  if (nrow(pretrend_slope_results) > 0) {
    write.csv(pretrend_slope_results,
              paste0(resultsDirPath, "pretrendSlopeTest", suffix, ".csv"),
              row.names = FALSE)
  }

}

# Set to FALSE to skip extra/intermediate outputs and only produce main + supplemental figures/tables
SKIP_EXTRA_OUTPUTS <- TRUE
# Set to TRUE to only produce Figure 1 (for fast iteration on formatting)
FIGURE1_ONLY <- FALSE

if (!FIGURE1_ONLY) {
Analysis(baseData, periodLength = "1m", sensitivity_analysis = "", depVar = "delayRate")
Analysis(baseData, periodLength = "1m", sensitivity_analysis = "", depVar = "refRate")

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

Analysis(baseData, periodLength = "6m", sensitivity_analysis = "propensityScoreIMD", depVar = "delayRate") # produces overallResults_6m_propensityScoreIMD_delayRate.csv
Analysis(baseData, periodLength = "6m", sensitivity_analysis = "propensityScoreIMD", depVar = "refRate") # produces overallResults_6m_propensityScoreIMD_refRate.csv

Analysis(baseData, periodLength = "6m", sensitivity_analysis = "14days", depVar = "delayRate") # produces overallResults_6m_14days_delayRate.csv
Analysis(baseData, periodLength = "6m", sensitivity_analysis = "42days", depVar = "delayRate") # produces overallResults_6m_42days_delayRate.csv
Analysis(baseData, periodLength = "6m", sensitivity_analysis = "62days", depVar = "delayRate") # produces overallResults_6m_62days_delayRate.csv

Analysis(baseData, periodLength = "6m", sensitivity_analysis = "", depVar = "estWaitTime") # produces overallResults_6m_estWaitTime.csv

# Reference period sensitivity analyses for monthly event study figures
for (ref_month in 1:5) {
  Analysis(baseData, periodLength = "1m", sensitivity_analysis = paste0("ref-", ref_month), depVar = "delayRate")
  Analysis(baseData, periodLength = "1m", sensitivity_analysis = paste0("ref-", ref_month), depVar = "refRate")
}

library(patchwork)

# ============================================================
# 2-panel DotsPlots: delay rate (top) + referral rate (bottom) for each cancer group
# ============================================================

pop_df_2panel <- read.csv(paste0(covariateDirPath, "Cancer+Prevalence+Statistics+England+2020_download_popByCA.csv"), skip = 1)
period_adj_2panel <- 7  # for 1m analyses

if (!SKIP_EXTRA_OUTPUTS) {
for (group_name in names(cancer_groups)) {
  groupData <- baseData %>%
    filter(suspected_cancer_or_breast_symptomatic %in% cancer_groups[[group_name]])

  # Aggregate to Cancer.Alliance x month level
  aggData <- groupData %>%
    group_by(Cancer.Alliance, monthNum) %>%
    summarise(
      num_not_told_diagnosis_outcome_within_28_days = sum(num_not_told_diagnosis_outcome_within_28_days),
      told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
      mced_treated = mean(mced_treated),
      .groups = "drop"
    ) %>%
    left_join(pop_df_2panel[, c("Cancer.Alliance", "population")], by = "Cancer.Alliance") %>%
    mutate(
      delayRate = num_not_told_diagnosis_outcome_within_28_days / told_diagnosis_outcome_total,
      refRate = (told_diagnosis_outcome_total / population) * 100000,
      periodNum = monthNum - period_adj_2panel,
      group = ifelse(mced_treated == 1, "Participating in trial", "Not participating in trial")
    )

  dots_colors <- c("Participating in trial" = "#E69F00", "Not participating in trial" = "#56B4E9")

  # --- Top panel: delay rate ---
  means_delay <- aggData %>%
    group_by(group, periodNum) %>%
    summarise(outcome = mean(delayRate), .groups = "drop")

  panel_delay <- ggplot() +
    geom_point(data = aggData,
               aes(x = periodNum, y = delayRate, color = group),
               alpha = 0.3, size = 1.0, position = position_dodge(width = 0.6)) +
    geom_line(data = means_delay,
              aes(x = periodNum, y = outcome, color = group), linewidth = 0.8) +
    geom_point(data = means_delay,
               aes(x = periodNum, y = outcome, fill = group),
               shape = 21, size = 1.4, color = "black", stroke = 0.38, alpha = 0.6) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey40", linewidth = 0.5) +
    scale_color_manual(values = dots_colors, name = NULL) +
    scale_fill_manual(values = dots_colors, name = NULL) +
    scale_x_continuous(limits = c(-6.7, 36.7), breaks = seq(-6, 36, by = 2), expand = c(0, 0)) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.0), expand = c(0, 0)) +
    labs(x = NULL, y = "Diagnostic delay rate") +
    theme(panel.grid.minor.x = element_blank(),
          legend.position = "bottom",
          axis.title = element_text(size = 14),
          axis.title.y = element_text(margin = margin(r = 8)),
          axis.text = element_text(size = 12))

  # --- Bottom panel: referral rate ---
  means_ref <- aggData %>%
    group_by(group, periodNum) %>%
    summarise(outcome = mean(refRate), .groups = "drop")

  panel_ref <- ggplot() +
    geom_point(data = aggData,
               aes(x = periodNum, y = refRate, color = group),
               alpha = 0.3, size = 1.0, position = position_dodge(width = 0.6)) +
    geom_line(data = means_ref,
              aes(x = periodNum, y = outcome, color = group), linewidth = 0.8) +
    geom_point(data = means_ref,
               aes(x = periodNum, y = outcome, fill = group),
               shape = 21, size = 1.4, color = "black", stroke = 0.38, alpha = 0.6) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey40", linewidth = 0.5) +
    scale_color_manual(values = dots_colors, name = NULL) +
    scale_fill_manual(values = dots_colors, name = NULL) +
    scale_x_continuous(limits = c(-6.7, 36.7), breaks = seq(-6, 36, by = 2), expand = c(0, 0)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
    labs(x = "Length of time (months) relative to trial start",
         y = "Referrals per 100,000 population") +
    theme(panel.grid.minor.x = element_blank(),
          legend.position = "bottom",
          axis.title = element_text(size = 14),
          axis.title.x = element_text(margin = margin(t = 5)),
          axis.title.y = element_text(margin = margin(r = 8)),
          axis.text = element_text(size = 12))


  # Combine into 2-panel figure; legend on bottom panel only
  panel_delay <- panel_delay + theme(legend.position = "none")
  combined_2panel <- wrap_plots(panel_delay, panel_ref, ncol = 1)

  file_path <- paste0(resultsDirPath, "DotsPlot_2Panel_", gsub("/", "", group_name), "_1m.png")
  ggsave(file_path, combined_2panel, width = 12, height = 8, units = "in", dpi = 300)
  ggsave(sub("\\.png$", ".eps", file_path), combined_2panel, width = 12, height = 8, units = "in", device = "eps")
}
}
} # end FIGURE1_ONLY skip block for Analysis calls

# Load data and packages needed for 3-panel figures
library(patchwork)
pop_df_2panel <- read.csv(paste0(covariateDirPath, "Cancer+Prevalence+Statistics+England+2020_download_popByCA.csv"), skip = 1)
period_adj_2panel <- 7  # for 1m analyses

# ============================================================
# 3-panel plots: one panel per cancer group, for delay rate and referral rate
# ============================================================

group_display_labels_dots <- c(
  "primary_high_detection" = "Primary high-detection",
  "sec_exp_high_detection" = "Expanded high-detection",
  "sec_low_detection" = "Low-detection"
)

# Aggregate data for all three cancer groups
dots_3panel_data <- map_dfr(names(cancer_groups), function(gname) {
  gdata <- baseData %>%
    filter(suspected_cancer_or_breast_symptomatic %in% cancer_groups[[gname]])

  gdata %>%
    group_by(Cancer.Alliance, monthNum) %>%
    summarise(
      num_not_told_diagnosis_outcome_within_28_days = sum(num_not_told_diagnosis_outcome_within_28_days),
      told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
      mced_treated = mean(mced_treated),
      .groups = "drop"
    ) %>%
    left_join(pop_df_2panel[, c("Cancer.Alliance", "population")], by = "Cancer.Alliance") %>%
    mutate(
      delayRate = num_not_told_diagnosis_outcome_within_28_days / told_diagnosis_outcome_total,
      refRate = (told_diagnosis_outcome_total / population) * 100000,
      periodNum = monthNum - period_adj_2panel,
      group = factor(ifelse(mced_treated == 1, "Participating in trial", "Not participating in trial"),
                     levels = c("Participating in trial", "Not participating in trial")),
      cancer_group = factor(group_display_labels_dots[gname], levels = group_display_labels_dots)
    )
})

dots_colors_3panel <- c("Participating in trial" = "#E69F00", "Not participating in trial" = "#56B4E9")

# --- 3-panel delay rate ---
means_delay_3panel <- dots_3panel_data %>%
  group_by(cancer_group, group, periodNum) %>%
  summarise(outcome = mean(delayRate), .groups = "drop")

group_levels <- levels(dots_3panel_data$cancer_group)

# Year bracket data
year_brackets <- data.frame(
  year_label = c("2021", "2022", "2023", "2024"),
  x_start = c(-6, 3, 15, 27),
  x_end = c(2, 14, 26, 36),
  stringsAsFactors = FALSE
)

# Helper: build a single spaghetti delay rate panel
build_spaghetti_delay_panel <- function(i, x_labels) {
  gdata <- dots_3panel_data %>% filter(cancer_group == group_levels[i])
  gmeans <- means_delay_3panel %>% filter(cancer_group == group_levels[i])
  is_first <- (i == 1)

  # Same y-range as Figure 1 (0-1.0)
  bracket_y <- -0.16
  tick_y_top <- -0.13
  label_y <- -0.21
  sz_annot <- sz_uniform / ggplot2::.pt

  p <- ggplot() +
    geom_line(data = gdata,
              aes(x = periodNum, y = delayRate, group = Cancer.Alliance, color = group),
              alpha = 0.25, linewidth = 0.4) +
    geom_line(data = gmeans,
              aes(x = periodNum, y = outcome, color = group),
              linewidth = 1.2) +
    geom_point(data = gmeans,
               aes(x = periodNum, y = outcome, fill = group),
               shape = 21, size = 2.0, color = "black", stroke = 0.38, alpha = 0.7) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey40", linewidth = 0.5) +
    scale_color_manual(values = dots_colors_3panel, name = NULL,
                       labels = c("Participating in trial" = "Participating regions",
                                  "Not participating in trial" = "Non-participating regions")) +
    scale_fill_manual(values = dots_colors_3panel, guide = "none") +
    scale_x_continuous(limits = c(-6.7, 36.7), breaks = breaks_vec, labels = x_labels,
                       expand = c(0, 0)) +
    scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
    coord_cartesian(ylim = c(0, 1.0), clip = "off") +
    labs(title = NULL, x = NULL, y = "Diagnostic delay rate") +
    dots_panel_theme

  # Panel label and title
  p <- p +
    annotate("label", x = -6.5, y = 1.05, label = panel_labels[i],
             fontface = "bold", size = sz_annot, hjust = 0, vjust = 0,
             linewidth = 0.4, label.r = unit(0, "lines"),
             fill = "white", color = "black",
             label.padding = unit(0.2, "lines")) +
    annotate("text", x = -5.3, y = 1.06, label = panel_titles[i],
             fontface = "plain", size = sz_annot, hjust = 0, vjust = 0)

  # Year brackets
  for (b in seq_len(nrow(year_brackets))) {
    xs <- year_brackets$x_start[b]
    xe <- year_brackets$x_end[b]
    xm <- (xs + xe) / 2
    p <- p + annotate("segment", x = xs, xend = xe, y = bracket_y, yend = bracket_y,
                       color = "black", linewidth = 0.4)
    p <- p + annotate("segment", x = xs, xend = xs, y = bracket_y, yend = tick_y_top,
                       color = "black", linewidth = 0.4)
    p <- p + annotate("segment", x = xe, xend = xe, y = bracket_y, yend = tick_y_top,
                       color = "black", linewidth = 0.4)
    p <- p + annotate("text", x = xm, y = label_y, label = year_brackets$year_label[b],
                       size = sz_annot, fontface = "plain")
  }

  # Legend on first panel (upper right of plot area)
  if (is_first) {
    p <- p + guides(color = guide_legend(override.aes = list(linewidth = 1.2, alpha = 1))) +
      theme(legend.position = c(0.86, 0.95),
            legend.justification = c(0.5, 1),
            legend.text = element_text(size = sz_uniform * 0.85),
            legend.key = element_rect(fill = "white", color = NA),
            legend.key.size = unit(1.2, "lines"),
            legend.spacing.y = unit(0.6, "lines"),
            legend.background = element_rect(fill = "white", color = "grey50", linewidth = 0.3),
            legend.margin = margin(t = 10, b = 10, l = 14, r = 14))
  }

  p
}

# Main version (even months: Apr, Jun, Aug, Oct, ...)
spaghetti_delay_panels <- lapply(seq_along(group_levels), function(i) {
  build_spaghetti_delay_panel(i, x_labels_2line)
})
spaghetti_3panel_delay <- wrap_plots(spaghetti_delay_panels, ncol = 1)
ggsave(paste0(resultsDirPath, "Figure1_SpaghettiPlot_3Panel_delayRate_1m.png"),
       spaghetti_3panel_delay, width = 18, height = 20, units = "in", dpi = 300)
ggsave(paste0(resultsDirPath, "Figure1_SpaghettiPlot_3Panel_delayRate_1m.eps"),
       spaghetti_3panel_delay, width = 18, height = 20, units = "in", device = "eps")

if (!FIGURE1_ONLY) { # skip everything below when iterating on Figure 1

# --- 3-panel referral rate ---
means_ref_3panel <- dots_3panel_data %>%
  group_by(cancer_group, group, periodNum) %>%
  summarise(outcome = mean(refRate), .groups = "drop")

# --- 3-panel spaghetti referral rate ---
# Helper: build a single spaghetti referral rate panel
build_spaghetti_ref_panel <- function(i, x_labels) {
  gdata <- dots_3panel_data %>% filter(cancer_group == group_levels[i])
  gmeans <- means_ref_3panel %>% filter(cancer_group == group_levels[i])
  is_first <- (i == 1)

  y_min <- 0
  y_max <- max(c(gdata$refRate, gmeans$outcome), na.rm = TRUE) * 1.05
  pos <- annotation_positions(y_min, y_max)
  sz_annot <- sz_uniform / ggplot2::.pt

  p <- ggplot() +
    geom_line(data = gdata,
              aes(x = periodNum, y = refRate, group = Cancer.Alliance, color = group),
              alpha = 0.25, linewidth = 0.4) +
    geom_line(data = gmeans,
              aes(x = periodNum, y = outcome, color = group),
              linewidth = 1.2) +
    geom_point(data = gmeans,
               aes(x = periodNum, y = outcome, fill = group),
               shape = 21, size = 2.0, color = "black", stroke = 0.38, alpha = 0.7) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey40", linewidth = 0.5) +
    scale_color_manual(values = dots_colors_3panel, name = NULL,
                       labels = c("Participating in trial" = "Participating regions",
                                  "Not participating in trial" = "Non-participating regions")) +
    scale_fill_manual(values = dots_colors_3panel, guide = "none") +
    scale_x_continuous(limits = c(-6.7, 36.7), breaks = breaks_vec, labels = x_labels,
                       expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(ylim = c(y_min, y_max), clip = "off") +
    labs(title = NULL, x = NULL, y = "Referrals per 100,000 population") +
    dots_panel_theme

  # Panel label and title
  p <- p +
    annotate("label", x = -6.5, y = pos$panel_label_y, label = panel_labels[i],
             fontface = "bold", size = sz_annot, hjust = 0, vjust = 0,
             linewidth = 0.4, label.r = unit(0, "lines"),
             fill = "white", color = "black",
             label.padding = unit(0.2, "lines")) +
    annotate("text", x = -5.3, y = pos$panel_title_y, label = panel_titles[i],
             fontface = "plain", size = sz_annot, hjust = 0, vjust = 0)

  # Year brackets
  for (b in seq_len(nrow(year_brackets))) {
    xs <- year_brackets$x_start[b]
    xe <- year_brackets$x_end[b]
    xm <- (xs + xe) / 2
    p <- p + annotate("segment", x = xs, xend = xe, y = pos$bracket_y, yend = pos$bracket_y,
                       color = "black", linewidth = 0.4)
    p <- p + annotate("segment", x = xs, xend = xs, y = pos$bracket_y, yend = pos$tick_y_top,
                       color = "black", linewidth = 0.4)
    p <- p + annotate("segment", x = xe, xend = xe, y = pos$bracket_y, yend = pos$tick_y_top,
                       color = "black", linewidth = 0.4)
    p <- p + annotate("text", x = xm, y = pos$label_y, label = year_brackets$year_label[b],
                       size = sz_annot, fontface = "plain")
  }

  # Legend on first panel (upper right of plot area)
  if (is_first) {
    p <- p + guides(color = guide_legend(override.aes = list(linewidth = 1.2, alpha = 1))) +
      theme(legend.position = c(0.86, 0.95),
            legend.justification = c(0.5, 1),
            legend.text = element_text(size = sz_uniform * 0.85),
            legend.key = element_rect(fill = "white", color = NA),
            legend.key.size = unit(1.2, "lines"),
            legend.spacing.y = unit(0.6, "lines"),
            legend.background = element_rect(fill = "white", color = "grey50", linewidth = 0.3),
            legend.margin = margin(t = 10, b = 10, l = 14, r = 14))
  }

  p
}

spaghetti_ref_panels <- lapply(seq_along(group_levels), function(i) build_spaghetti_ref_panel(i, x_labels_2line))
spaghetti_3panel_ref <- wrap_plots(spaghetti_ref_panels, ncol = 1)
ggsave(paste0(resultsDirPath, "SuppFigure_SpaghettiPlot_3Panel_refRate_1m.png"),
       spaghetti_3panel_ref, width = 18, height = 20, units = "in", dpi = 300)
ggsave(paste0(resultsDirPath, "SuppFigure_SpaghettiPlot_3Panel_refRate_1m.eps"),
       spaghetti_3panel_ref, width = 18, height = 20, units = "in", device = "eps")

if (!SKIP_EXTRA_OUTPUTS) {
# ============================================================
# 3-panel demeaned spaghetti plot: change in delay rate from pre-trial baseline
# Individual region trajectories + group mean lines
# ============================================================

# Compute pre-trial (months -6 to -1, i.e. periodNum in -6:-1) average delay rate per region × cancer group
pre_trial_means <- dots_3panel_data %>%
  filter(periodNum >= -6, periodNum <= -1) %>%
  group_by(Cancer.Alliance, cancer_group) %>%
  summarise(pre_trial_avg = mean(delayRate), .groups = "drop")

# Merge and demean: subtract each region's pre-trial average
demeaned_data <- dots_3panel_data %>%
  left_join(pre_trial_means, by = c("Cancer.Alliance", "cancer_group")) %>%
  mutate(demeaned_delay = (delayRate - pre_trial_avg) * 100)  # convert to percentage points

# Compute group means of the demeaned trajectories
demeaned_means <- demeaned_data %>%
  group_by(cancer_group, group, periodNum) %>%
  summarise(demeaned_delay = mean(demeaned_delay), .groups = "drop")

demeaned_spaghetti_3panel <- ggplot() +
  # Individual region trajectories (thin, semi-transparent)
  geom_line(data = demeaned_data,
            aes(x = periodNum, y = demeaned_delay, group = Cancer.Alliance, color = group),
            alpha = 0.25, linewidth = 0.4) +
  # Group mean lines (thick, opaque)
  geom_line(data = demeaned_means,
            aes(x = periodNum, y = demeaned_delay, color = group),
            linewidth = 1.2) +
  # Horizontal baseline reference
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  # Vertical trial start line (between month -1 and month 0)
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  scale_color_manual(
    values = c("Participating in trial" = "#E69F00", "Not participating in trial" = "#56B4E9"),
    name = NULL
  ) +
  scale_x_continuous(limits = c(-6.7, 36.7), breaks = seq(-6, 36, by = 2), expand = c(0, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  facet_wrap(~ cancer_group, ncol = 1) +
  labs(
    x = "Length of time (months) relative to trial start",
    y = "Change in diagnostic delay rate from\npre-trial baseline (percentage points)"
  ) +
  theme(panel.grid.minor.x = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size = 12, face = "bold", hjust = 0),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 5)),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.text = element_text(size = 12))

ggsave(paste0(resultsDirPath, "DemeanedSpaghetti_3Panel_delayRate_1m.png"),
       demeaned_spaghetti_3panel, width = 12, height = 10, units = "in", dpi = 300)
ggsave(paste0(resultsDirPath, "DemeanedSpaghetti_3Panel_delayRate_1m.eps"),
       demeaned_spaghetti_3panel, width = 12, height = 10, units = "in", device = "eps")
}

# ============================================================
# Leave-one-out robustness analysis: 6m, all cancer groups
# ============================================================

# Helper: prepare grouped data and run the 6m model, returning the feols model object
run_6m_model <- function(input_data, depVar = "delayRate", group_name = "primary_high_detection") {
  d <- input_data %>%
    filter(suspected_cancer_or_breast_symptomatic %in% cancer_groups[[group_name]])

  d$periodNum <- as.numeric(ceiling(d$monthNum / 6))
  d$period <- as.character(d$periodNum)

  # Aggregate: Provider.Code x monthNum
  dg <- d %>%
    group_by(Provider.Code, monthNum) %>%
    summarise(
      num_not_told_diagnosis_outcome_within_28_days = sum(num_not_told_diagnosis_outcome_within_28_days),
      told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
      mced_treated = mean(mced_treated),
      periodNum = mean(periodNum),
      period = first(period),
      Cancer.Alliance = first(Cancer.Alliance),
      totalStaff = first(totalStaff),
      totalAbsent = first(totalAbsent),
      .groups = "drop"
    )

  # Aggregate: Cancer.Alliance x monthNum
  dg <- dg %>%
    group_by(Cancer.Alliance, monthNum) %>%
    summarise(
      num_not_told_diagnosis_outcome_within_28_days = sum(num_not_told_diagnosis_outcome_within_28_days),
      told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
      mced_treated = mean(mced_treated),
      period = first(period),
      periodNum = mean(periodNum),
      totalStaff = sum(totalStaff, na.rm = TRUE),
      totalAbsent = sum(totalAbsent, na.rm = TRUE),
      .groups = "drop"
    )

  # Aggregate: Cancer.Alliance x period
  dg <- dg %>%
    group_by(Cancer.Alliance, period) %>%
    summarise(
      num_not_told_diagnosis_outcome_within_28_days = sum(num_not_told_diagnosis_outcome_within_28_days),
      told_diagnosis_outcome_total = sum(told_diagnosis_outcome_total),
      mced_treated = mean(mced_treated),
      periodNum = mean(periodNum),
      totalStaff = mean(totalStaff, na.rm = TRUE),
      totalAbsent = mean(totalAbsent, na.rm = TRUE),
      .groups = "drop"
    )

  dg$percentage_not_told_diagnosis_outcome_within_28_days <-
    dg$num_not_told_diagnosis_outcome_within_28_days / dg$told_diagnosis_outcome_total
  dg$percentageAbsent <- dg$totalAbsent / dg$totalStaff

  if (depVar == "refRate") {
    loo_pop_df <- read.csv(paste0(covariateDirPath, "Cancer+Prevalence+Statistics+England+2020_download_popByCA.csv"), skip = 1)
    dg <- merge(dg, loo_pop_df[, c("Cancer.Alliance", "population")], by = "Cancer.Alliance", all.x = TRUE)
    dg$ref_rate <- (dg$told_diagnosis_outcome_total / dg$population) * 100000
  }

  # Create dummy variables
  dg <- dg %>%
    mutate(dummy = ifelse(mced_treated == 1, 1, 0)) %>%
    pivot_wider(names_from = periodNum, values_from = dummy,
                names_prefix = "dummy_period_", values_fill = list(dummy = 0)) %>%
    rename_with(~ sub("dummy_period_([0-9])$", "dummy_period_0\\1", .x)) %>%
    select(-dummy_period_01)

  dummy_cols <- sort(grep("dummy_period_", names(dg), value = TRUE))
  non_dummy_cols <- setdiff(names(dg), dummy_cols)
  dg <- dg %>% select(all_of(non_dummy_cols), all_of(dummy_cols))
  dummy_vars <- names(dg)[grepl("dummy_period_", names(dg))]

  if (depVar == "delayRate") {
    formula_str <- paste("percentage_not_told_diagnosis_outcome_within_28_days ~ `percentageAbsent` +",
                          paste(dummy_vars, collapse = " + "), "| Cancer.Alliance + period")
  } else {
    formula_str <- paste("ref_rate ~ `percentageAbsent` +",
                          paste(dummy_vars, collapse = " + "), "| Cancer.Alliance + period")
  }

  weight_var <- if (depVar == "refRate") ~population else ~told_diagnosis_outcome_total
  model <- feols(fml = as.formula(formula_str),
                 weights = weight_var,
                 data = dg,
                 cluster = "Cancer.Alliance")
  return(model)
}

# Get all region names and their treatment status
region_info <- baseData %>%
  select(Cancer.Alliance, mced_treated) %>%
  distinct() %>%
  arrange(Cancer.Alliance)

# Period mapping
loo_periods <- data.frame(
  dummy_var = paste0("dummy_period_0", 2:7),
  period_label = c("Months 0\u20135", "Months 6\u201311", "Months 12\u201317",
                    "Months 18\u201323", "Months 24\u201329", "Months 30\u201335"),
  period_file = c("Months0-5", "Months6-11", "Months12-17",
                   "Months18-23", "Months24-29", "Months30-35"),
  period_col = c("Months0-5", "Months6-11", "Months12-17",
                  "Months18-23", "Months24-29", "Months30-35"),
  stringsAsFactors = FALSE
)

# Colors matching spaghetti plots
loo_colors <- c("Full sample" = "black", "Participating" = "#E69F00", "Non-participating" = "#56B4E9")

# Helper: extract LOO results for a given depVar and cancer group, returning full-sample + LOO data
run_loo_analysis <- function(depVar, group_name = "primary_high_detection") {
  conv <- ifelse(depVar == "delayRate", 100, 1)

  # Full-sample model
  full_model <- run_6m_model(baseData, depVar, group_name)
  full_coefs <- coef(full_model) * conv
  full_cis <- confint(full_model, level = 0.95) * conv
  full_pvals <- coeftable(full_model)[, "Pr(>|t|)"]

  # LOO for each region
  loo_results <- map_dfr(seq_len(nrow(region_info)), function(r) {
    excluded_region <- region_info$Cancer.Alliance[r]
    excluded_treated <- region_info$mced_treated[r]
    cat("  LOO (", depVar, ",", group_name, "): excluding", excluded_region, "\n")

    subset_data <- baseData %>% filter(Cancer.Alliance != excluded_region)
    loo_model <- run_6m_model(subset_data, depVar, group_name)
    loo_coefs <- coef(loo_model) * conv
    loo_cis <- confint(loo_model, level = 0.95) * conv
    loo_pvals <- coeftable(loo_model)[, "Pr(>|t|)"]

    map_dfr(seq_len(nrow(loo_periods)), function(p) {
      dv <- loo_periods$dummy_var[p]
      tibble(
        excluded_region = excluded_region,
        mced_treated = excluded_treated,
        period_label = loo_periods$period_label[p],
        period_file = loo_periods$period_file[p],
        period_col = loo_periods$period_col[p],
        estimate = loo_coefs[dv],
        lower_ci = loo_cis[dv, "2.5 %"],
        upper_ci = loo_cis[dv, "97.5 %"],
        p_value = loo_pvals[dv]
      )
    })
  })

  # Full-sample results as a tibble
  full_results <- map_dfr(seq_len(nrow(loo_periods)), function(p) {
    dv <- loo_periods$dummy_var[p]
    tibble(
      excluded_region = "Full sample",
      mced_treated = NA_real_,
      period_label = loo_periods$period_label[p],
      period_file = loo_periods$period_file[p],
      period_col = loo_periods$period_col[p],
      estimate = full_coefs[dv],
      lower_ci = full_cis[dv, "2.5 %"],
      upper_ci = full_cis[dv, "97.5 %"],
      p_value = full_pvals[dv]
    )
  })

  list(full = full_results, loo = loo_results)
}

# Run LOO for primary high-detection (both depVars, for figures + tables)
loo_primary_delay <- run_loo_analysis("delayRate", "primary_high_detection")
loo_primary_ref <- run_loo_analysis("refRate", "primary_high_detection")

# Run LOO for expanded high-detection and low-detection (both depVars, tables only)
loo_expanded_delay <- run_loo_analysis("delayRate", "sec_exp_high_detection")
loo_expanded_ref <- run_loo_analysis("refRate", "sec_exp_high_detection")
loo_low_delay <- run_loo_analysis("delayRate", "sec_low_detection")
loo_low_ref <- run_loo_analysis("refRate", "sec_low_detection")

if (!SKIP_EXTRA_OUTPUTS) {
# --- Create forest plot figures (primary high-detection, delayRate) ---
# Rows ordered: Full sample at top, then participating alphabetically, then non-participating alphabetically
for (p in seq_len(nrow(loo_periods))) {
  dv <- loo_periods$dummy_var[p]
  p_label <- loo_periods$period_label[p]
  p_file <- loo_periods$period_file[p]

  full_row <- loo_primary_delay$full %>% filter(period_file == p_file)
  pdata <- loo_primary_delay$loo %>%
    filter(period_file == p_file) %>%
    mutate(section = ifelse(mced_treated == 1, "Participating", "Non-participating"))

  # Sort alphabetically within sections (participating first, then non-participating)
  pdata <- pdata %>% arrange(desc(mced_treated), excluded_region) %>% mutate(row_order = row_number())

  n_part <- sum(pdata$mced_treated == 1)
  n_nonpart <- sum(pdata$mced_treated == 0)
  gap <- 1.5
  full_gap <- 1.5

  pdata <- pdata %>%
    mutate(y_pos = case_when(
      mced_treated == 1 ~ row_order + n_nonpart + gap,
      TRUE ~ row_order - n_part
    ))

  # Full sample row at top
  full_y <- max(pdata$y_pos) + full_gap + 1.0
  full_plot_row <- tibble(
    excluded_region = "Full sample", estimate = full_row$estimate,
    lower_ci = full_row$lower_ci, upper_ci = full_row$upper_ci,
    section = "Full sample", y_pos = full_y
  )

  header_part_y <- max(pdata$y_pos[pdata$mced_treated == 1]) + 1.0
  header_nonpart_y <- max(pdata$y_pos[pdata$mced_treated == 0]) + 1.0
  sep_line_y <- (min(pdata$y_pos[pdata$mced_treated == 1]) + max(pdata$y_pos[pdata$mced_treated == 0])) / 2
  sep_full_y <- (full_y + max(pdata$y_pos)) / 2 + 0.3

  all_plot_data <- bind_rows(pdata %>% select(excluded_region, estimate, lower_ci, upper_ci, section, y_pos),
                             full_plot_row)

  loo_plot <- ggplot(all_plot_data, aes(y = y_pos)) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey40", linewidth = 0.5) +
    annotate("segment", x = -Inf, xend = Inf, y = sep_line_y, yend = sep_line_y,
             color = "grey70", linewidth = 0.3) +
    annotate("segment", x = -Inf, xend = Inf, y = sep_full_y, yend = sep_full_y,
             color = "grey50", linewidth = 0.4) +
    geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci, color = section),
                   height = 0.3, linewidth = 0.5) +
    geom_point(aes(x = estimate, color = section), size = 2.2, shape = 15) +
    annotate("text", x = min(all_plot_data$lower_ci, na.rm = TRUE) - 0.3, y = header_part_y,
             label = "Excluding participating region", hjust = 0, fontface = "bold", size = 3.5) +
    annotate("text", x = min(all_plot_data$lower_ci, na.rm = TRUE) - 0.3, y = header_nonpart_y,
             label = "Excluding non-participating region", hjust = 0, fontface = "bold", size = 3.5) +
    scale_color_manual(values = loo_colors, guide = "none") +
    scale_y_continuous(
      breaks = all_plot_data$y_pos,
      labels = all_plot_data$excluded_region,
      expand = expansion(add = c(0.5, 0.8))
    ) +
    labs(
      title = paste0("Leave-one-out analysis: Primary high-detection group, ", p_label),
      x = "Adjusted difference-in-differences, percentage points (95% CI)",
      y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
      axis.text.y = element_text(size = 8),
      axis.ticks.y = element_blank(),
      plot.title = element_text(size = 12, face = "bold"),
      plot.margin = margin(t = 10, r = 15, b = 10, l = 5)
    )

  ggsave(paste0(resultsDirPath, "LeaveOneOut_primary_high_detection_6m_delayRate_", p_file, ".png"),
         loo_plot, width = 10, height = 8, units = "in", dpi = 300)
  ggsave(paste0(resultsDirPath, "LeaveOneOut_primary_high_detection_6m_delayRate_", p_file, ".eps"),
         loo_plot, width = 10, height = 8, units = "in", device = "eps")
}
}

# --- Create formatted CSV tables for all groups and both depVars ---
# Row order: Full sample, then participating (alphabetical), then non-participating (alphabetical)
format_loo_table <- function(loo_analysis, depVar) {
  full <- loo_analysis$full
  loo <- loo_analysis$loo

  # Alphabetical within sections: participating first, then non-participating
  ordered_regions <- loo %>%
    select(excluded_region, mced_treated) %>%
    distinct() %>%
    arrange(desc(mced_treated), excluded_region) %>%
    pull(excluded_region)

  all_rows <- c("Full sample", ordered_regions)
  all_data <- bind_rows(full, loo)

  table_out <- map_dfr(all_rows, function(region) {
    row_data <- all_data %>% filter(excluded_region == region)
    cells <- setNames(map_chr(loo_periods$period_col, function(pc) {
      rd <- row_data %>% filter(period_col == pc)
      p_text <- ifelse(rd$p_value < 0.001, "p<.001",
                       ifelse(rd$p_value < 0.01,
                              paste0("p=", sub("^0", "", sprintf("%.3f", rd$p_value))),
                              paste0("p=", sub("^0", "", sprintf("%.2f", rd$p_value)))))
      paste0(sprintf("%.2f", rd$estimate), " (",
             sprintf("%.2f", rd$lower_ci), "-",
             sprintf("%.2f", rd$upper_ci), ", ",
             p_text, ")")
    }), loo_periods$period_col)
    tibble(ExcludedRegion = region, !!!cells)
  })

  table_out
}

# Primary high-detection tables
write.csv(format_loo_table(loo_primary_delay, "delayRate"),
          paste0(resultsDirPath, "SuppTable_LeaveOneOut_primary_high_detection_6m_delayRate.csv"), row.names = FALSE)
write.csv(format_loo_table(loo_primary_ref, "refRate"),
          paste0(resultsDirPath, "SuppTable_LeaveOneOut_primary_high_detection_6m_refRate.csv"), row.names = FALSE)

# Expanded high-detection tables
write.csv(format_loo_table(loo_expanded_delay, "delayRate"),
          paste0(resultsDirPath, "SuppTable_LeaveOneOut_sec_exp_high_detection_6m_delayRate.csv"), row.names = FALSE)
write.csv(format_loo_table(loo_expanded_ref, "refRate"),
          paste0(resultsDirPath, "SuppTable_LeaveOneOut_sec_exp_high_detection_6m_refRate.csv"), row.names = FALSE)

# Low-detection tables
write.csv(format_loo_table(loo_low_delay, "delayRate"),
          paste0(resultsDirPath, "SuppTable_LeaveOneOut_sec_low_detection_6m_delayRate.csv"), row.names = FALSE)
write.csv(format_loo_table(loo_low_ref, "refRate"),
          paste0(resultsDirPath, "SuppTable_LeaveOneOut_sec_low_detection_6m_refRate.csv"), row.names = FALSE)

cat("Leave-one-out figures and tables saved.\n")

# ============================================================
# Figure 2: Bhatla-style hybrid figure-table (forest plot)
# Three cancer groups × six biannual periods
# Main analysis, diagnostic delay rates
#
# Append this code after all Analysis() calls in the main script.
# Requires: tidyverse (already loaded), patchwork (new dependency)
# ============================================================

library(patchwork)

# --- Helper: parse formatted DiD string into numeric components ---
parse_did_string <- function(s) {
  m <- str_match(s, "^\\s*(-?\\d+\\.\\d+)\\s*\\((-?\\d+\\.\\d+)-(-?\\d+\\.\\d+),\\s*p([=<])(\\d*\\.\\d+)")
  data.frame(
    estimate = as.numeric(m[, 2]),
    lower_ci = as.numeric(m[, 3]),
    upper_ci = as.numeric(m[, 4]),
    p_op     = m[, 5],
    p_value  = as.numeric(m[, 6])
  )
}

# --- 1. Read overall results and extract three group-level rows ---

results_csv <- read.csv(paste0(resultsDirPath, "SuppTable_overallResults_6m_delayRate.csv"),
                        check.names = FALSE, stringsAsFactors = FALSE)

group_rows <- results_csv %>% filter(grepl("p[=<]", `Months0-5`))

group_display_labels <- c(
  "Primary high-detection group",
  "Expanded high-detection group",
  "Low-detection group"
)

period_col_names <- c("Months0-5", "Months6-11", "Months12-17",
                      "Months18-23", "Months24-29", "Months30-35")
period_display_labels <- c("Months 0\u20135", "Months 6\u201311", "Months 12\u201317",
                           "Months 18\u201323", "Months 24\u201329", "Months 30\u201335")

fig2_data <- map_dfr(seq_len(nrow(group_rows)), function(i) {
  map_dfr(seq_along(period_col_names), function(j) {
    parsed <- parse_did_string(group_rows[[period_col_names[j]]][i])
    tibble(
      group_num   = i,
      group_label = group_display_labels[i],
      period_num  = j,
      period_label = period_display_labels[j],
      estimate    = parsed$estimate,
      lower_ci    = parsed$lower_ci,
      upper_ci    = parsed$upper_ci,
      p_op        = parsed$p_op,
      p_value     = parsed$p_value
    )
  })
})

# --- 2. Read unadjusted rates ---

unadj_file_keys <- c("primary_high_detection",
                     "sec_exp_high_detection",
                     "sec_low_detection")

fig2_data$pre_part      <- NA_real_
fig2_data$post_part     <- NA_real_
fig2_data$pre_nonpart   <- NA_real_
fig2_data$post_nonpart  <- NA_real_

for (i in seq_along(unadj_file_keys)) {
  unadj <- read.csv(
    paste0(resultsDirPath, "unadjustedRates_6m_delayRate_", unadj_file_keys[i], ".csv"),
    stringsAsFactors = FALSE
  )
  unadj$period <- as.numeric(unadj$period)

  pre_part <- unadj %>%
    filter(period == 1, mced_treated == 1) %>%
    pull(percentage_not_told_diagnosis_outcome_within_28_days) * 100
  pre_nonpart <- unadj %>%
    filter(period == 1, mced_treated == 0) %>%
    pull(percentage_not_told_diagnosis_outcome_within_28_days) * 100

  fig2_data$pre_part[fig2_data$group_num == i]    <- pre_part
  fig2_data$pre_nonpart[fig2_data$group_num == i] <- pre_nonpart

  for (j in 1:6) {
    post_part_val <- unadj %>%
      filter(period == j + 1, mced_treated == 1) %>%
      pull(percentage_not_told_diagnosis_outcome_within_28_days) * 100
    post_nonpart_val <- unadj %>%
      filter(period == j + 1, mced_treated == 0) %>%
      pull(percentage_not_told_diagnosis_outcome_within_28_days) * 100

    fig2_data$post_part[fig2_data$group_num == i & fig2_data$period_num == j]    <- post_part_val
    fig2_data$post_nonpart[fig2_data$group_num == i & fig2_data$period_num == j] <- post_nonpart_val
  }
}

# --- 3. Assign y-positions ---

row_spacing     <- 1.0
header_to_data  <- 0.8
data_to_sep     <- 0.5
sep_to_header   <- 0.5

g3_data_top <- 5 * row_spacing
g3_header <- g3_data_top + header_to_data
sep_2_3 <- g3_header + sep_to_header
g2_data_bottom <- sep_2_3 + data_to_sep
g2_data_top <- g2_data_bottom + 5 * row_spacing
g2_header <- g2_data_top + header_to_data
sep_1_2 <- g2_header + sep_to_header
g1_data_bottom <- sep_1_2 + data_to_sep
g1_data_top <- g1_data_bottom + 5 * row_spacing
g1_header <- g1_data_top + header_to_data

fig2_data <- fig2_data %>%
  arrange(group_num, period_num) %>%
  mutate(
    y_pos = case_when(
      group_num == 1 ~ g1_data_top - (period_num - 1) * row_spacing,
      group_num == 2 ~ g2_data_top - (period_num - 1) * row_spacing,
      group_num == 3 ~ g3_data_top - (period_num - 1) * row_spacing
    )
  )

header_data <- tibble(
  group_num   = 1:3,
  group_label = paste0(group_display_labels, c("\u1d9c", "\u1d48", "\u1d49")),
  y_pos       = c(g1_header, g2_header, g3_header)
)

sep_lines <- tibble(y_pos = c(sep_1_2, sep_2_3))

group_header_sep_lines <- tibble(
  y_sep = c(
    (g1_header + g1_data_top) / 2,
    (g2_header + g2_data_top) / 2,
    (g3_header + g3_data_top) / 2
  )
)

row_sep_lines <- fig2_data %>%
  group_by(group_num) %>%
  filter(period_num < 6) %>%
  mutate(y_sep = y_pos - row_spacing * 0.5) %>%
  ungroup() %>%
  select(y_sep)

# --- 4. Format display text ---

fig2_data <- fig2_data %>%
  mutate(
    change_part    = post_part - pre_part,
    change_nonpart = post_nonpart - pre_nonpart,
    pre_part_text       = sprintf("%.1f", pre_part),
    post_part_text      = sprintf("%.1f", post_part),
    change_part_text    = sprintf("%.1f", change_part),
    pre_nonpart_text    = sprintf("%.1f", pre_nonpart),
    post_nonpart_text   = sprintf("%.1f", post_nonpart),
    change_nonpart_text = sprintf("%.1f", change_nonpart),
    did_text            = sprintf("%.2f (%.2f to %.2f)", estimate, lower_ci, upper_ci),
    p_text = case_when(
      p_op == "<" ~ "<.001",
      p_value < 0.01 ~ sub("^0", "", sprintf("%.3f", p_value)),
      TRUE ~ sub("^0", "", sprintf("%.2f", p_value))
    )
  )

fig2_data <- fig2_data %>%
  mutate(
    pre_part_text    = ifelse(period_num == 1, pre_part_text, ""),
    pre_nonpart_text = ifelse(period_num == 1, pre_nonpart_text, "")
  )

# --- 5. Build figure ---

y_header_line <- g1_header + sep_to_header
y_col_header2 <- y_header_line + row_spacing * 0.55  # sub-headers sit a bit higher for 2-line labels
y_mid_header_line <- y_col_header2 + row_spacing * 0.55
y_col_header1 <- y_mid_header_line + row_spacing * 0.15

y_max <- y_col_header1 + row_spacing * 0.9
y_min <- min(fig2_data$y_pos) - row_spacing * 0.25

# X positions — left edge of each column for left-aligned data values
x_period        <- 1.0
x_pre_part      <- 4.3
x_post_part     <- 5.2
x_change_part   <- 6.1
x_pre_nonp      <- 7.1
x_post_nonp     <- 8.0
x_change_nonp   <- 8.9
x_did           <- 9.7
x_pval          <- 12.0
x_did_right     <- 12.6

# Single uniform font size
sz_all <- 2.8

# Line colors
col_group_sep  <- "grey50"
col_row_sep    <- "grey85"
col_header_sub <- "grey70"

y_top_data <- max(fig2_data$y_pos) + row_spacing * 0.4

# ----- Left panel: text table -----
text_panel <- ggplot() +
  coord_cartesian(xlim = c(0.3, x_did_right + 0.1), ylim = c(y_min, y_max), clip = "off") +

  # --- Top-level column headers (bold, LEFT-ALIGNED at sub-column left edge) ---
  annotate("text", x = x_pre_part, y = y_col_header1,
           label = "Participating\nregions, % (n=8)",
           hjust = 0, vjust = 0, size = sz_all, fontface = "bold", lineheight = 0.85) +
  annotate("text", x = x_pre_nonp, y = y_col_header1,
           label = "Non-participating\nregions, % (n=13)",
           hjust = 0, vjust = 0, size = sz_all, fontface = "bold", lineheight = 0.85) +

  # --- Far-left column header (lower-left, just above header line) ---
  annotate("text", x = x_period, y = y_header_line + row_spacing * 0.15,
           label = "Six-month period\nfollowing trial start",
           hjust = 0, vjust = 0, size = sz_all, fontface = "bold", lineheight = 0.85) +

  # --- DiD header (lower-left, just above header line) ---
  annotate("text", x = x_did, y = y_header_line + row_spacing * 0.15,
           label = "Adjusted difference-\nin-differences (95% CI),\npercentage points\u1d47",
           hjust = 0, vjust = 0, size = sz_all, fontface = "bold", lineheight = 0.85) +

  # --- P-value header ---
  annotate("text", x = x_pval, y = y_header_line + row_spacing * 0.15,
           label = "P\nvalue",
           hjust = 0, vjust = 0, size = sz_all, fontface = "bold", lineheight = 0.85) +

  # --- Line between top-level and sub-level headers ---
  annotate("segment",
           x = x_pre_part - 0.2, xend = x_change_nonp + 0.7,
           y = y_mid_header_line, yend = y_mid_header_line,
           color = col_header_sub, linewidth = 0.3) +

  # --- Sub-headers (bold, two-line, left-aligned at column left edge, bottom-anchored) ---
  annotate("text", x = x_pre_part, y = y_header_line + row_spacing * 0.15,
           label = "Before\ntrial\u1d43",
           hjust = 0, vjust = 0, size = sz_all, fontface = "bold", lineheight = 0.85) +
  annotate("text", x = x_post_part, y = y_header_line + row_spacing * 0.15,
           label = "During\ntrial",
           hjust = 0, vjust = 0, size = sz_all, fontface = "bold", lineheight = 0.85) +
  annotate("text", x = x_change_part, y = y_header_line + row_spacing * 0.15,
           label = "Change",
           hjust = 0, vjust = 0, size = sz_all, fontface = "bold", lineheight = 0.85) +
  annotate("text", x = x_pre_nonp, y = y_header_line + row_spacing * 0.15,
           label = "Before\ntrial",
           hjust = 0, vjust = 0, size = sz_all, fontface = "bold", lineheight = 0.85) +
  annotate("text", x = x_post_nonp, y = y_header_line + row_spacing * 0.15,
           label = "During\ntrial",
           hjust = 0, vjust = 0, size = sz_all, fontface = "bold", lineheight = 0.85) +
  annotate("text", x = x_change_nonp, y = y_header_line + row_spacing * 0.15,
           label = "Change",
           hjust = 0, vjust = 0, size = sz_all, fontface = "bold", lineheight = 0.85) +

  # --- Header separator line ---
  annotate("segment", x = 0.5, xend = x_did_right,
           y = y_header_line, yend = y_header_line,
           color = col_group_sep, linewidth = 0.4) +

  # --- Group separator lines ---
  {
    annotations <- list()
    for (i in seq_len(nrow(sep_lines))) {
      annotations <- c(annotations, list(
        annotate("segment", x = 0.5, xend = x_did_right,
                 y = sep_lines$y_pos[i], yend = sep_lines$y_pos[i],
                 color = col_group_sep, linewidth = 0.4)
      ))
    }
    annotations
  } +

  # --- Lines below group headers ---
  {
    annotations <- list()
    for (i in seq_len(nrow(group_header_sep_lines))) {
      annotations <- c(annotations, list(
        annotate("segment", x = 0.5, xend = x_did_right,
                 y = group_header_sep_lines$y_sep[i], yend = group_header_sep_lines$y_sep[i],
                 color = col_header_sub, linewidth = 0.3)
      ))
    }
    annotations
  } +

  # --- Light row separators ---
  {
    annotations <- list()
    for (i in seq_len(nrow(row_sep_lines))) {
      annotations <- c(annotations, list(
        annotate("segment", x = 0.5, xend = x_did_right,
                 y = row_sep_lines$y_sep[i], yend = row_sep_lines$y_sep[i],
                 color = col_row_sep, linewidth = 0.25)
      ))
    }
    annotations
  } +

  # --- Group headers (bold) ---
  geom_text(data = header_data,
            aes(x = x_period, y = y_pos, label = group_label),
            hjust = 0, size = sz_all, fontface = "bold") +

  # --- Data rows (plain, left-aligned at column left edge) ---
  geom_text(data = fig2_data,
            aes(x = x_period + 0.3, y = y_pos, label = period_label),
            hjust = 0, size = sz_all, fontface = "plain") +
  geom_text(data = fig2_data,
            aes(x = x_pre_part, y = y_pos, label = pre_part_text),
            hjust = 0, size = sz_all, fontface = "plain") +
  geom_text(data = fig2_data,
            aes(x = x_post_part, y = y_pos, label = post_part_text),
            hjust = 0, size = sz_all, fontface = "plain") +
  geom_text(data = fig2_data,
            aes(x = x_change_part, y = y_pos, label = change_part_text),
            hjust = 0, size = sz_all, fontface = "plain") +
  geom_text(data = fig2_data,
            aes(x = x_pre_nonp, y = y_pos, label = pre_nonpart_text),
            hjust = 0, size = sz_all, fontface = "plain") +
  geom_text(data = fig2_data,
            aes(x = x_post_nonp, y = y_pos, label = post_nonpart_text),
            hjust = 0, size = sz_all, fontface = "plain") +
  geom_text(data = fig2_data,
            aes(x = x_change_nonp, y = y_pos, label = change_nonpart_text),
            hjust = 0, size = sz_all, fontface = "plain") +
  geom_text(data = fig2_data,
            aes(x = x_did, y = y_pos, label = did_text),
            hjust = 0, size = sz_all, fontface = "plain") +
  geom_text(data = fig2_data,
            aes(x = x_pval, y = y_pos, label = p_text),
            hjust = 0, size = sz_all, fontface = "plain") +

  theme_void() +
  theme(plot.margin = margin(t = 2, r = 0, b = 0, l = 5))

# ----- Right panel: forest plot -----

whisker_height <- row_spacing * 0.3

forest_panel <- ggplot(fig2_data) +
  coord_cartesian(ylim = c(y_min, y_max), xlim = c(-9, 9)) +

  # Zero reference line (dotted, clipped)
  annotate("segment", x = 0, xend = 0,
           y = y_min, yend = y_top_data,
           linetype = "dotted", color = "grey40", linewidth = 0.5) +

  # CI lines with whisker caps
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci, y = y_pos),
                 height = whisker_height, linewidth = 0.5) +

  # Point estimates
  geom_point(aes(x = estimate, y = y_pos), size = 2.2, shape = 15) +

  scale_x_continuous(
    name = "Differential change after trial start,\npercentage points (95% CI)",
    breaks = seq(-8, 8, by = 2),
    limits = c(-9, 9),
    expand = c(0, 0),
    guide = guide_axis(check.overlap = TRUE)
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y          = element_blank(),
    axis.ticks.y         = element_blank(),
    axis.title.y         = element_blank(),
    panel.grid.major.y   = element_blank(),
    panel.grid.minor     = element_blank(),
    panel.grid.major.x   = element_blank(),
    axis.text.x          = element_text(size = 8, margin = margin(t = 1)),
    axis.title.x         = element_text(size = 8, margin = margin(t = 3), lineheight = 0.85),
    axis.ticks.x         = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length.x  = unit(0.15, "cm"),
    plot.margin          = margin(t = 2, r = 10, b = 0, l = 0)
  ) +
  # X-axis line from -8 to 8 only
  annotate("segment", x = -8, xend = 8,
           y = -Inf, yend = -Inf,
           color = "black", linewidth = 0.4)

# ----- Combine panels (even tighter) -----
fig2_combined <- text_panel + forest_panel +
  plot_layout(widths = c(1.75, 1))

ggsave(paste0(resultsDirPath, "Figure2_ForestPlot_delayRate.png"),
       fig2_combined, width = 14, height = 7, units = "in", dpi = 300)
ggsave(paste0(resultsDirPath, "Figure2_ForestPlot_delayRate.eps"),
       fig2_combined, width = 14, height = 7, units = "in", device = "eps")

cat("Figure 2 saved to:", paste0(resultsDirPath, "Figure2_ForestPlot_delayRate.png"), "\n")

# ============================================================
# Forest plot for referral rates (similar to Figure 2 for delay rates)
# ============================================================

results_csv_ref <- read.csv(paste0(resultsDirPath, "SuppTable_overallResults_6m_refRate.csv"),
                            check.names = FALSE, stringsAsFactors = FALSE)

group_rows_ref <- results_csv_ref %>% filter(grepl("p[=<]", `Months0-5`))

fig_ref_data <- map_dfr(seq_len(nrow(group_rows_ref)), function(i) {
  map_dfr(seq_along(period_col_names), function(j) {
    parsed <- parse_did_string(group_rows_ref[[period_col_names[j]]][i])
    tibble(
      group_num    = i,
      group_label  = group_display_labels[i],
      period_num   = j,
      period_label = period_display_labels[j],
      estimate     = parsed$estimate,
      lower_ci     = parsed$lower_ci,
      upper_ci     = parsed$upper_ci,
      p_op         = parsed$p_op,
      p_value      = parsed$p_value
    )
  })
})

# --- Read unadjusted referral rates ---
fig_ref_data$pre_part      <- NA_real_
fig_ref_data$post_part     <- NA_real_
fig_ref_data$pre_nonpart   <- NA_real_
fig_ref_data$post_nonpart  <- NA_real_

for (i in seq_along(unadj_file_keys)) {
  unadj_ref <- read.csv(
    paste0(resultsDirPath, "unadjustedRates_6m_refRate_", unadj_file_keys[i], ".csv"),
    stringsAsFactors = FALSE
  )
  unadj_ref$period <- as.numeric(unadj_ref$period)

  pre_part <- unadj_ref %>%
    filter(period == 1, mced_treated == 1) %>%
    pull(ref_rate)
  pre_nonpart <- unadj_ref %>%
    filter(period == 1, mced_treated == 0) %>%
    pull(ref_rate)

  fig_ref_data$pre_part[fig_ref_data$group_num == i]    <- pre_part
  fig_ref_data$pre_nonpart[fig_ref_data$group_num == i] <- pre_nonpart

  for (j in 1:6) {
    post_part_val <- unadj_ref %>%
      filter(period == j + 1, mced_treated == 1) %>%
      pull(ref_rate)
    post_nonpart_val <- unadj_ref %>%
      filter(period == j + 1, mced_treated == 0) %>%
      pull(ref_rate)

    fig_ref_data$post_part[fig_ref_data$group_num == i & fig_ref_data$period_num == j]    <- post_part_val
    fig_ref_data$post_nonpart[fig_ref_data$group_num == i & fig_ref_data$period_num == j] <- post_nonpart_val
  }
}

# Assign y-positions (reuse layout constants from delay rate forest plot)
fig_ref_data <- fig_ref_data %>%
  arrange(group_num, period_num) %>%
  mutate(
    y_pos = case_when(
      group_num == 1 ~ g1_data_top - (period_num - 1) * row_spacing,
      group_num == 2 ~ g2_data_top - (period_num - 1) * row_spacing,
      group_num == 3 ~ g3_data_top - (period_num - 1) * row_spacing
    )
  )

# Format display text
fig_ref_data <- fig_ref_data %>%
  mutate(
    change_part    = post_part - pre_part,
    change_nonpart = post_nonpart - pre_nonpart,
    pre_part_text       = sprintf("%.1f", pre_part),
    post_part_text      = sprintf("%.1f", post_part),
    change_part_text    = sprintf("%.1f", change_part),
    pre_nonpart_text    = sprintf("%.1f", pre_nonpart),
    post_nonpart_text   = sprintf("%.1f", post_nonpart),
    change_nonpart_text = sprintf("%.1f", change_nonpart),
    did_text            = sprintf("%.2f (%.2f to %.2f)", estimate, lower_ci, upper_ci),
    p_text = case_when(
      p_op == "<" ~ "<.001",
      p_value < 0.01 ~ sub("^0", "", sprintf("%.3f", p_value)),
      TRUE ~ sub("^0", "", sprintf("%.2f", p_value))
    )
  )

fig_ref_data <- fig_ref_data %>%
  mutate(
    pre_part_text    = ifelse(period_num == 1, pre_part_text, ""),
    pre_nonpart_text = ifelse(period_num == 1, pre_nonpart_text, "")
  )

# Auto-scale x-axis to fit all CIs with padding, with evenly-spaced breaks (~7-9 labels)
ref_ci_max <- max(abs(c(fig_ref_data$lower_ci, fig_ref_data$upper_ci)), na.rm = TRUE)
ref_x_step <- ceiling(ref_ci_max / 4 / 10) * 10   # ~4 breaks per side, rounded to nearest 10
ref_x_max <- ref_x_step * 5                         # room for 4 labeled + padding
ref_x_breaks <- seq(-ref_x_step * 4, ref_x_step * 4, by = ref_x_step)

# Reuse x positions from delay rate forest plot
yr_header_line <- g1_header + sep_to_header
yr_col_header2 <- yr_header_line + row_spacing * 0.55
yr_mid_header_line <- yr_col_header2 + row_spacing * 0.55
yr_col_header1 <- yr_mid_header_line + row_spacing * 0.15
yr_max <- yr_col_header1 + row_spacing * 0.9
yr_min <- min(fig_ref_data$y_pos) - row_spacing * 0.25
yr_top_data <- max(fig_ref_data$y_pos) + row_spacing * 0.4

# ----- Left panel: text table -----
text_panel_ref <- ggplot() +
  coord_cartesian(xlim = c(0.3, x_did_right + 0.1), ylim = c(yr_min, yr_max), clip = "off") +

  # --- Top-level column headers ---
  annotate("text", x = x_pre_part, y = yr_col_header1,
           label = "Participating\nregions (n=8)",
           hjust = 0, vjust = 0, size = sz_all, fontface = "bold", lineheight = 0.85) +
  annotate("text", x = x_pre_nonp, y = yr_col_header1,
           label = "Non-participating\nregions (n=13)",
           hjust = 0, vjust = 0, size = sz_all, fontface = "bold", lineheight = 0.85) +

  # --- Far-left column header ---
  annotate("text", x = x_period, y = yr_header_line + row_spacing * 0.15,
           label = "Six-month period\nfollowing trial start",
           hjust = 0, vjust = 0, size = sz_all, fontface = "bold", lineheight = 0.85) +

  # --- DiD header ---
  annotate("text", x = x_did, y = yr_header_line + row_spacing * 0.15,
           label = "Adjusted difference-\nin-differences (95% CI),\nreferrals per 100,000\u1d47",
           hjust = 0, vjust = 0, size = sz_all, fontface = "bold", lineheight = 0.85) +

  # --- P-value header ---
  annotate("text", x = x_pval, y = yr_header_line + row_spacing * 0.15,
           label = "P\nvalue",
           hjust = 0, vjust = 0, size = sz_all, fontface = "bold", lineheight = 0.85) +

  # --- Line between top-level and sub-level headers ---
  annotate("segment",
           x = x_pre_part - 0.2, xend = x_change_nonp + 0.7,
           y = yr_mid_header_line, yend = yr_mid_header_line,
           color = col_header_sub, linewidth = 0.3) +

  # --- Sub-headers ---
  annotate("text", x = x_pre_part, y = yr_header_line + row_spacing * 0.15,
           label = "Before\ntrial\u1d43",
           hjust = 0, vjust = 0, size = sz_all, fontface = "bold", lineheight = 0.85) +
  annotate("text", x = x_post_part, y = yr_header_line + row_spacing * 0.15,
           label = "During\ntrial",
           hjust = 0, vjust = 0, size = sz_all, fontface = "bold", lineheight = 0.85) +
  annotate("text", x = x_change_part, y = yr_header_line + row_spacing * 0.15,
           label = "Change",
           hjust = 0, vjust = 0, size = sz_all, fontface = "bold", lineheight = 0.85) +
  annotate("text", x = x_pre_nonp, y = yr_header_line + row_spacing * 0.15,
           label = "Before\ntrial",
           hjust = 0, vjust = 0, size = sz_all, fontface = "bold", lineheight = 0.85) +
  annotate("text", x = x_post_nonp, y = yr_header_line + row_spacing * 0.15,
           label = "During\ntrial",
           hjust = 0, vjust = 0, size = sz_all, fontface = "bold", lineheight = 0.85) +
  annotate("text", x = x_change_nonp, y = yr_header_line + row_spacing * 0.15,
           label = "Change",
           hjust = 0, vjust = 0, size = sz_all, fontface = "bold", lineheight = 0.85) +

  # --- Header separator line ---
  annotate("segment", x = 0.5, xend = x_did_right,
           y = yr_header_line, yend = yr_header_line,
           color = col_group_sep, linewidth = 0.4) +

  # --- Group separator lines ---
  {
    annotations <- list()
    for (i in seq_len(nrow(sep_lines))) {
      annotations <- c(annotations, list(
        annotate("segment", x = 0.5, xend = x_did_right,
                 y = sep_lines$y_pos[i], yend = sep_lines$y_pos[i],
                 color = col_group_sep, linewidth = 0.4)
      ))
    }
    annotations
  } +

  # --- Lines below group headers ---
  {
    annotations <- list()
    for (i in seq_len(nrow(group_header_sep_lines))) {
      annotations <- c(annotations, list(
        annotate("segment", x = 0.5, xend = x_did_right,
                 y = group_header_sep_lines$y_sep[i], yend = group_header_sep_lines$y_sep[i],
                 color = col_header_sub, linewidth = 0.3)
      ))
    }
    annotations
  } +

  # --- Light row separators ---
  {
    annotations <- list()
    for (i in seq_len(nrow(row_sep_lines))) {
      annotations <- c(annotations, list(
        annotate("segment", x = 0.5, xend = x_did_right,
                 y = row_sep_lines$y_sep[i], yend = row_sep_lines$y_sep[i],
                 color = col_row_sep, linewidth = 0.25)
      ))
    }
    annotations
  } +

  # --- Group headers (bold) ---
  geom_text(data = header_data,
            aes(x = x_period, y = y_pos, label = group_label),
            hjust = 0, size = sz_all, fontface = "bold") +

  # --- Data rows ---
  geom_text(data = fig_ref_data,
            aes(x = x_period + 0.3, y = y_pos, label = period_label),
            hjust = 0, size = sz_all, fontface = "plain") +
  geom_text(data = fig_ref_data,
            aes(x = x_pre_part, y = y_pos, label = pre_part_text),
            hjust = 0, size = sz_all, fontface = "plain") +
  geom_text(data = fig_ref_data,
            aes(x = x_post_part, y = y_pos, label = post_part_text),
            hjust = 0, size = sz_all, fontface = "plain") +
  geom_text(data = fig_ref_data,
            aes(x = x_change_part, y = y_pos, label = change_part_text),
            hjust = 0, size = sz_all, fontface = "plain") +
  geom_text(data = fig_ref_data,
            aes(x = x_pre_nonp, y = y_pos, label = pre_nonpart_text),
            hjust = 0, size = sz_all, fontface = "plain") +
  geom_text(data = fig_ref_data,
            aes(x = x_post_nonp, y = y_pos, label = post_nonpart_text),
            hjust = 0, size = sz_all, fontface = "plain") +
  geom_text(data = fig_ref_data,
            aes(x = x_change_nonp, y = y_pos, label = change_nonpart_text),
            hjust = 0, size = sz_all, fontface = "plain") +
  geom_text(data = fig_ref_data,
            aes(x = x_did, y = y_pos, label = did_text),
            hjust = 0, size = sz_all, fontface = "plain") +
  geom_text(data = fig_ref_data,
            aes(x = x_pval, y = y_pos, label = p_text),
            hjust = 0, size = sz_all, fontface = "plain") +

  theme_void() +
  theme(plot.margin = margin(t = 2, r = 0, b = 0, l = 5))

# ----- Right panel: forest plot -----
forest_panel_ref <- ggplot(fig_ref_data) +
  coord_cartesian(ylim = c(yr_min, yr_max), xlim = c(-ref_x_max, ref_x_max)) +

  # Zero reference line (dotted, clipped)
  annotate("segment", x = 0, xend = 0,
           y = yr_min, yend = yr_top_data,
           linetype = "dotted", color = "grey40", linewidth = 0.5) +

  # CI lines with whisker caps
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci, y = y_pos),
                 height = whisker_height, linewidth = 0.5) +

  # Point estimates
  geom_point(aes(x = estimate, y = y_pos), size = 2.2, shape = 15) +

  scale_x_continuous(
    name = "Differential change after trial start,\nreferrals per 100,000 (95% CI)",
    breaks = ref_x_breaks,
    limits = c(-ref_x_max, ref_x_max),
    expand = c(0, 0)
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y          = element_blank(),
    axis.ticks.y         = element_blank(),
    axis.title.y         = element_blank(),
    panel.grid.major.y   = element_blank(),
    panel.grid.minor     = element_blank(),
    panel.grid.major.x   = element_blank(),
    axis.text.x          = element_text(size = 8, margin = margin(t = 1)),
    axis.title.x         = element_text(size = 8, margin = margin(t = 3), lineheight = 0.85),
    axis.ticks.x         = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length.x  = unit(0.15, "cm"),
    plot.margin          = margin(t = 2, r = 10, b = 0, l = 0)
  ) +
  annotate("segment", x = min(ref_x_breaks), xend = max(ref_x_breaks),
           y = -Inf, yend = -Inf,
           color = "black", linewidth = 0.4)

# ----- Combine panels -----
fig_ref_combined <- text_panel_ref + forest_panel_ref +
  plot_layout(widths = c(1.75, 1))

ggsave(paste0(resultsDirPath, "Figure4_ForestPlot_refRate.png"),
       fig_ref_combined, width = 14, height = 7, units = "in", dpi = 300)
ggsave(paste0(resultsDirPath, "Figure4_ForestPlot_refRate.eps"),
       fig_ref_combined, width = 14, height = 7, units = "in", device = "eps")

cat("Referral rate forest plot saved to:", paste0(resultsDirPath, "Figure4_ForestPlot_refRate.png"), "\n")

# ============================================================
# eTable: Pre-Trend Slope Test (Miller 2023)
# Formatted supplemental table for JAMA
# ============================================================

pretrend_delay <- read.csv(paste0(resultsDirPath, "pretrendSlopeTest_1m_delayRate.csv"),
                           stringsAsFactors = FALSE)
pretrend_ref   <- read.csv(paste0(resultsDirPath, "pretrendSlopeTest_1m_refRate.csv"),
                           stringsAsFactors = FALSE)

# Combine both outcomes
pretrend_all <- rbind(pretrend_delay, pretrend_ref)

# Compute 95% CIs using t-distribution
pretrend_all$t_crit <- qt(0.975, df = pretrend_all$df)
pretrend_all$lower  <- pretrend_all$slope_per_month - pretrend_all$t_crit * pretrend_all$slope_se
pretrend_all$upper  <- pretrend_all$slope_per_month + pretrend_all$t_crit * pretrend_all$slope_se

# Format p-values: no leading zero, 2 decimals (3 if <.01), <.001 for very small
pretrend_all$p_text <- ifelse(pretrend_all$p_value < 0.001,
                              "p<.001",
                              ifelse(pretrend_all$p_value < 0.01,
                                     paste0("p=", sub("^0", "", sprintf("%.3f", pretrend_all$p_value))),
                                     paste0("p=", sub("^0", "", sprintf("%.2f", pretrend_all$p_value)))))

# Format as "estimate (lower-upper, p=.XX)"
pretrend_all$Formatted <- paste0(
  sprintf("%.2f", pretrend_all$slope_per_month),
  " (",
  sprintf("%.2f", pretrend_all$lower),
  " to ",
  sprintf("%.2f", pretrend_all$upper),
  ", ",
  pretrend_all$p_text,
  ")"
)

# Cancer group display labels (matching existing supplemental table labels)
group_labels_pretrend <- c(
  "primary_high_detection" = "Primary high-detection group",
  "sec_exp_high_detection" = "Expanded high-detection group",
  "sec_low_detection"      = "Low-detection group"
)
pretrend_all$CancerGroup <- group_labels_pretrend[pretrend_all$cancer_group]

# Pivot to wide format: one column per outcome
etable_pretrend <- pretrend_all %>%
  select(CancerGroup, dep_var, Formatted) %>%
  pivot_wider(names_from = dep_var, values_from = Formatted) %>%
  rename(
    `Cancer Group` = CancerGroup,
    `Diagnostic delay rate, pp per month (95% CI)` = delayRate,
    `Referral rate per 100 000, per month (95% CI)` = refRate
  )

# Order rows to match existing table ordering
etable_pretrend <- etable_pretrend[match(group_labels_pretrend, etable_pretrend$`Cancer Group`), ]

write.csv(etable_pretrend,
          paste0(resultsDirPath, "SuppTable_pretrendSlopeTest.csv"),
          row.names = FALSE)
cat("Pre-trend slope test eTable saved to:", paste0(resultsDirPath, "SuppTable_pretrendSlopeTest.csv"), "\n")

} # end FIGURE1_ONLY skip block for post-Figure1 code
