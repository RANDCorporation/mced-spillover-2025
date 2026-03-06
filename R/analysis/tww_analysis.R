# title: "TWW Panel Analysis"
# author: "Sean Mann"
# date: "5 March 2026"
# output: github_document

# Description
# This program analyzes Two Week Wait (TWW) breach rates and estimated wait times using a difference-in-differences
# panel analysis framework. Follows the same methodology as mced_panel_analysis.R but with a shorter study period
# (Apr 2021 - Sep 2023) and no sensitivity analyses beyond the main model.

# ### Read in Analytic Data

library(tidyverse)
library(fixest)
library(scales)
library(here)
library(patchwork)

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

# ---- Shared formatting constants ----
sz_uniform <- 16
base_date  <- as.Date("2021-10-01")  # month 0 = Oct 2021

# X-axis labels for spaghetti figures (months -6 to 23)
breaks_vec_tww   <- seq(-6, 23, by = 1)
month_dates_tww  <- base_date %m+% months(breaks_vec_tww)
month_abbrs_tww  <- format(month_dates_tww, "%b")
x_labels_2line_tww <- ifelse(breaks_vec_tww %% 2 == 0,
                              paste0(breaks_vec_tww, "\n", month_abbrs_tww), "")

# X-axis labels for DiffInDiff figures (months -6 to 22)
breaks_vec_did_tww   <- seq(-6, 22, by = 1)
month_dates_did_tww  <- base_date %m+% months(breaks_vec_did_tww)
month_abbrs_did_tww  <- format(month_dates_did_tww, "%b")
x_labels_2line_did_tww <- ifelse(breaks_vec_did_tww %% 2 == 0,
                                  paste0(breaks_vec_did_tww, "\n", month_abbrs_did_tww), "")

# Year brackets for spaghetti figures (3 years for TWW)
year_brackets_tww <- data.frame(
  year_label = c("2021", "2022", "2023"),
  x_start = c(-6, 3, 15),
  x_end   = c(2, 14, 23),
  stringsAsFactors = FALSE
)

# Year brackets for DiD figures
year_brackets_did_tww <- data.frame(
  year_label = c("2021", "2022", "2023"),
  x_start = c(-6, 3, 15),
  x_end   = c(2, 14, 22),
  stringsAsFactors = FALSE
)

# Compute annotation positions proportional to y-axis range
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

# Matching theme for facet_wrap figures
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

# Common theme for manual panels (coord_cartesian + clip="off" for annotations)
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

# Import dataset file that was output from tww_datacollection.R
baseData <- read.csv(paste0(dataDirPath, "tww_base_data.csv"),
                     row.names = 1,
                     stringsAsFactors = FALSE)

# Remove rows where providers could not be linked to a Cancer Alliance (7 rows, <0.0001% of referrals)
baseData <- baseData %>% filter(!is.na(Cancer.Alliance))

# Set up function for TWW analyses
TWWAnalysis <- function(data, periodLength, depVar) {

  # Create suffix to append at end of resulting filenames
  suffix <- paste0("_", periodLength, "_", depVar)

  # Set conversion factor: scale by 100 (%) for breach rate, no scaling for wait time
  if (depVar == "breachRate") {
    conversionFactor <- 100
  } else if (depVar == "estWaitTime") {
    conversionFactor <- 1
  }

  overall_results <- data.frame()
  pretrend_slope_results <- data.frame()

  # For monthly analysis, adjust period labels so that period 0 is the first month following start of the trial (October 2021)
  if (periodLength == "1m") {
    period_adjustment <- 7
  } else {
    period_adjustment <- 0
  }

  # Create period number variable
  data$periodNum = data$monthNum

  # For 6-month analysis, convert monthly numbers into six-month time periods
  if (periodLength == "6m") {
    data$periodNum = as.numeric(ceiling(data$monthNum / 6))
  }

  # Create column that holds period number as a string (for use in grouping operation)
  data$period = as.character(data$periodNum)

  startingData <- data

  # Accumulator for combining DiD plot data across cancer groups into a single figure
  did_plot_accumulator <- data.frame()

  for (cancers_included in c(cancer_types_to_loop_through)) {
    # Only run 1m analyses to produce monthly figures for the main cancer groups
    if (periodLength == "1m" && (!(cancers_included %in% c(names(cancer_groups))))) {
      next
    }

    # Print cancer group or cancer name that is focus of this analysis iteration
    print(cancers_included)

    suffix2 <- paste0("_", periodLength, "_", depVar, "_", gsub("/", "", cancers_included))

    data <- startingData

    if (cancers_included %in% names(cancer_groups)) {
      data <- subset(data, (suspected_cancer_or_breast_symptomatic %in% cancer_groups[[cancers_included]]))
    }
    else {
      data <- subset(data, (suspected_cancer_or_breast_symptomatic == cancers_included))
    }

    # First group rows across cancer sites at the Provider.Code and month level
    # Done in this sequence in order to only count staff and absence numbers once per provider-month (and not multiple times for each cancer site)
    dataGrouped <- data %>%
      group_by(Provider.Code, monthNum) %>%
      summarise(
        num_not_seen_within_14_days = sum(num_not_seen_within_14_days),
        tww_total = sum(tww_total),
        tww_within_14_days = sum(tww_within_14_days),
        tww_within_14_day_detail = sum(tww_within_14_day_detail),
        tww_in_15_to_16_days = sum(tww_in_15_to_16_days),
        tww_in_17_to_21_days = sum(tww_in_17_to_21_days),
        tww_in_22_to_28_days = sum(tww_in_22_to_28_days),
        tww_after_28_days = sum(tww_after_28_days),
        mced_treated = mean(mced_treated),
        periodNum = mean(periodNum),
        period = first(period),
        Cancer.Alliance = first(Cancer.Alliance),
        totalStaff = first(totalStaff),
        totalAbsent = first(totalAbsent),
        totalBedsOccupied = first(totalBedsOccupied),
        date = first(date)
      )

    # Group data up to the cancer alliance- and month-level
    dataGrouped <- dataGrouped %>%
      group_by(Cancer.Alliance, monthNum) %>%
      summarise(
        num_not_seen_within_14_days = sum(num_not_seen_within_14_days),
        tww_total = sum(tww_total),
        tww_within_14_days = sum(tww_within_14_days),
        tww_within_14_day_detail = sum(tww_within_14_day_detail),
        tww_in_15_to_16_days = sum(tww_in_15_to_16_days),
        tww_in_17_to_21_days = sum(tww_in_17_to_21_days),
        tww_in_22_to_28_days = sum(tww_in_22_to_28_days),
        tww_after_28_days = sum(tww_after_28_days),
        mced_treated = mean(mced_treated),
        periodNum = mean(periodNum),
        period = first(period),
        totalStaff = sum(totalStaff, na.rm = TRUE),
        totalAbsent = sum(totalAbsent, na.rm = TRUE),
        totalBedsOccupied = sum(totalBedsOccupied, na.rm = TRUE),
        date = first(date)
      )

    # Group to cancer alliance x period level (collapses months within a 6-month period)
    dataGrouped <- dataGrouped %>%
      group_by(Cancer.Alliance, period) %>%
      summarise(
        num_not_seen_within_14_days = sum(num_not_seen_within_14_days),
        tww_total = sum(tww_total),
        tww_within_14_days = sum(tww_within_14_days),
        tww_within_14_day_detail = sum(tww_within_14_day_detail),
        tww_in_15_to_16_days = sum(tww_in_15_to_16_days),
        tww_in_17_to_21_days = sum(tww_in_17_to_21_days),
        tww_in_22_to_28_days = sum(tww_in_22_to_28_days),
        tww_after_28_days = sum(tww_after_28_days),
        mced_treated = mean(mced_treated),
        periodNum = mean(periodNum),
        totalStaff = sum(totalStaff, na.rm = TRUE),
        totalAbsent = sum(totalAbsent, na.rm = TRUE),
        totalBedsOccupied = sum(totalBedsOccupied, na.rm = TRUE),
        date = first(date)
      ) %>%
      ungroup()

    # Calculate total referrals for this cancer type/group (for table display)
    total_referrals <- sum(dataGrouped$tww_total)

    # Calculate TWW breach rate (% not seen within 14 days)
    dataGrouped$breach_rate <- dataGrouped$num_not_seen_within_14_days / dataGrouped$tww_total

    # Estimate average wait times from counts within the 5 TWW wait time categories: 0-14; 15-16; 17-21; 22-28; 29+ days
    # Each observation in each category is assumed to have a wait time exactly in the middle of that category
    dataGrouped$est_wait_time <-
      (dataGrouped$tww_within_14_day_detail * 7) +     # midpoint of 0-14
      (dataGrouped$tww_in_15_to_16_days * 15.5) +      # midpoint of 15-16
      (dataGrouped$tww_in_17_to_21_days * 19) +         # midpoint of 17-21
      (dataGrouped$tww_in_22_to_28_days * 25) +          # midpoint of 22-28
      (dataGrouped$tww_after_28_days * 32)               # midpoint of 29-35 (7-day window matching prior band width)
    dataGrouped$est_wait_time <- (dataGrouped$est_wait_time / dataGrouped$tww_total)

    # Calculate percentage of staff absent (covariate)
    dataGrouped$percentageAbsent = (dataGrouped$totalAbsent / dataGrouped$totalStaff)

    # Compute raw unadjusted pre-trial baseline rate for participating regions
    if (depVar == "breachRate") {
      rateColName <- "breach_rate"
    } else if (depVar == "estWaitTime") {
      rateColName <- "est_wait_time"
    }

    pretrial_baseline <- dataGrouped %>%
      filter(mced_treated == 1, periodNum == 1) %>%
      summarise(rate = weighted.mean(.data[[rateColName]], tww_total, na.rm = TRUE)) %>%
      pull(rate) * conversionFactor

    # Create a dummy variable for each time period, set to 1 when a row is in that time period and is for a cancer alliance region participating in the trial
    dataGrouped <- dataGrouped %>%
      mutate(dummy = ifelse(mced_treated == 1, 1, 0)) %>%
      pivot_wider(names_from = periodNum, values_from = dummy,
                  names_prefix = "dummy_period_", values_fill = list(dummy = 0)) %>%
      rename_with(~ sub("dummy_period_([0-9])$", "dummy_period_0\\1", .x)) %>%
      # Drop reference period (period 01 = pre-trial baseline)
      select(-dummy_period_01)

    # Extracting dummy period columns and sort them by time period number
    dummy_cols <- grep("dummy_period_", names(dataGrouped), value = TRUE)

    # Ensure other columns are maintained in their original order
    non_dummy_cols <- setdiff(names(dataGrouped), dummy_cols)

    # Reorder the dummy columns
    dataGrouped <- dataGrouped %>%
      select(all_of(non_dummy_cols), all_of(sort(dummy_cols)))

    # Get all of the names of the dummy period variables
    dummy_vars <- names(dataGrouped)[grepl("dummy_period_", names(dataGrouped))]

    # Formula: outcome ~ percentageAbsent + dummies | Cancer.Alliance + period
    if (depVar == "breachRate") {
      formula_str <- paste("breach_rate ~ `percentageAbsent` + ", paste(dummy_vars, collapse = " + "), "| Cancer.Alliance + period")
    } else if (depVar == "estWaitTime") {
      formula_str <- paste("est_wait_time ~ `percentageAbsent` + ", paste(dummy_vars, collapse = " + "), "| Cancer.Alliance + period")
    }

    # Run model weighted by referral volume, with errors clustered at cancer alliance level
    model = feols(fml= as.formula(formula_str),
                  weights= ~tww_total,
                  data=dataGrouped,
                  cluster="Cancer.Alliance")

    # When periodLength is set to 6m we want to start populating table results
    if (periodLength == "6m") {
      # Extract coefficients (estimates)
      estimates <- coef(model)

      conf_intervals <- confint(model, level = 0.95)
      p_values <- coeftable(model)[, "Pr(>|t|)"]

      # Select specific variables to capture in table (4 post-trial periods for TWW)
      variables_of_interest <- c("dummy_period_02", "dummy_period_03", "dummy_period_04", "dummy_period_05")

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

      results_df$Formatted <- paste(results_df$Formatted, ")", sep = "")

      results_df <- data.frame(Variable = rownames(results_df), results_df, check.names = FALSE)
      rownames(results_df) <- NULL

      results_df <- results_df %>%
        mutate(CancerType = case_when(
          Variable == "dummy_period_02" ~ "Months0-5",
          Variable == "dummy_period_03" ~ "Months6-11",
          Variable == "dummy_period_04" ~ "Months12-17",
          Variable == "dummy_period_05" ~ "Months18-23"
        ))

      results_transposed <- results_df %>%
        select(CancerType, Formatted) %>%
        spread(key = CancerType, value = Formatted)

      # Add the new CancerType column and populate it
      results_transposed <- results_transposed %>%
        mutate(CancerType = cancers_included)

      # Add Total No. of Referrals and Pre-Trial Baseline columns
      results_transposed <- results_transposed %>%
        mutate(
          `Total No. of Referrals` = format(total_referrals, big.mark = ",", trim = TRUE),
          `Pre-Trial Baseline` = sprintf("%.1f", pretrial_baseline)
        ) %>%
        select(CancerType, `Total No. of Referrals`, `Pre-Trial Baseline`, "Months0-5", "Months6-11", "Months12-17", "Months18-23")

      overall_results <- rbind(overall_results, results_transposed)
    }

    if (periodLength == "1m" && cancers_included %in% c(names(cancer_groups))) {

      # --- Pre-trend slope test (Miller 2023, JEP, footnote 11) ---
      # Regress pre-period event-study gammas on event time j and test H0: slope = 0.
      # A significant slope indicates violation of the parallel trends assumption.

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

      # Accumulate result (scale to percentage points for reporting)
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

      # --- Standard normalization (single reference period pinned to 0) ---
      # Display model results in a figure where x axis is time periods and y axis is percentage point difference
      coefficients <- coef(model) * conversionFactor
      standard_errors <- se(model) * conversionFactor

      results <- data.frame(
        estimate = coefficients,
        se = standard_errors
      )

      results <- results[grep("dummy_period_", rownames(results)), ]
      results$period <- as.numeric(sub("dummy_period_", "", rownames(results)))

      # Add the omitted reference period as zero (it is the normalization point by construction)
      results <- rbind(results, data.frame(estimate = 0, se = 0, period = 1, row.names = "ref"))

      # Set axis label in figure so that start of trial (Oct 2021) is 0, meaning pre-trial time periods are negative.
      results$period = results$period - period_adjustment

      # Add confidence intervals
      results$lower <- results$estimate - 1.96 * results$se
      results$upper <- results$estimate + 1.96 * results$se

      # Accumulate results for combined DiD plot
      results$cancer_group <- cancers_included

      # Compute baseline (reference period) rate for treated group
      baseline_treated <- dataGrouped %>%
        filter(mced_treated == 1, as.numeric(period) == 1)
      results$baseline_rate <- weighted.mean(baseline_treated[[rateColName]],
                                             baseline_treated$tww_total) * conversionFactor

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

    if (depVar == "breachRate") {
      did_y_label <- "TWW breach rate, percentage points"
      did_y_fmt <- label_percent(scale = 1)
    } else if (depVar == "estWaitTime") {
      did_y_label <- "Estimated wait time (days)"
      did_y_fmt <- waiver()
    }

    group_colors_3panel <- c(
      "Primary high-detection" = "#009E73",
      "Expanded high-detection" = "#0072B2",
      "Low-detection" = "#D55E00"
    )

    # Build individual DiffInDiff panels (manual assembly)
    group_levels_did <- levels(did_plot_accumulator$group_label)

    # Compute shared y-range for all panels
    did_y_min <- min(did_plot_accumulator$lower, na.rm = TRUE)
    did_y_max <- max(did_plot_accumulator$upper, na.rm = TRUE)
    y_pad <- (did_y_max - did_y_min) * 0.1
    did_y_min <- did_y_min - y_pad
    did_y_max <- did_y_max + y_pad

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
        scale_x_continuous(limits = c(-6.7, 22.7), breaks = breaks_vec_did_tww,
                           labels = x_labels_2line_did_tww, expand = c(0, 0)) +
        coord_cartesian(ylim = c(did_y_min, did_y_max), clip = "off") +
        labs(title = NULL, x = NULL, y = did_y_label) +
        dots_panel_theme

      # Y-axis labels
      p <- p + scale_y_continuous(labels = did_y_fmt, expand = c(0, 0))

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
      for (b in seq_len(nrow(year_brackets_did_tww))) {
        xs <- year_brackets_did_tww$x_start[b]
        xe <- year_brackets_did_tww$x_end[b]
        xm <- (xs + xe) / 2
        p <- p + annotate("segment", x = xs, xend = xe, y = pos$bracket_y, yend = pos$bracket_y,
                           color = "black", linewidth = 0.4)
        p <- p + annotate("segment", x = xs, xend = xs, y = pos$bracket_y, yend = pos$tick_y_top,
                           color = "black", linewidth = 0.4)
        p <- p + annotate("segment", x = xe, xend = xe, y = pos$bracket_y, yend = pos$tick_y_top,
                           color = "black", linewidth = 0.4)
        p <- p + annotate("text", x = xm, y = pos$label_y, label = year_brackets_did_tww$year_label[b],
                           size = sz_annot, fontface = "plain")
      }

      p
    }

    did_panels <- lapply(seq_along(group_levels_did), build_did_panel)
    did_3panel_plot <- wrap_plots(did_panels, ncol = 1)

    file_path <- paste0(resultsDirPath, "TWW_DiffInDiff_Combined3Panel", suffix, ".png")
    ggsave(file_path, did_3panel_plot, width = 18, height = 20, units = "in", dpi = 300)
    ggsave(sub("\\.png$", ".eps", file_path), did_3panel_plot, width = 18, height = 20, units = "in", device = "eps")
  }

  if (periodLength == "6m") {
    # Read the linking file to get cancer type labels and order for tabular display
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
      select(-Order)
    write.csv(overall_results, paste0(resultsDirPath, "TWW_SuppTable_overallResults", suffix, ".csv"), row.names = FALSE)
  }

  # Export pre-trend slope test results (Miller 2023)
  if (nrow(pretrend_slope_results) > 0) {
    write.csv(pretrend_slope_results,
              paste0(resultsDirPath, "TWW_pretrendSlopeTest", suffix, ".csv"),
              row.names = FALSE)
  }

}

# ============================================================
# Run analyses
# ============================================================

# Monthly analysis for figures and pre-trend tests
TWWAnalysis(baseData, periodLength = "1m", depVar = "estWaitTime")

# 6-month analysis for tables
TWWAnalysis(baseData, periodLength = "6m", depVar = "estWaitTime")

# ============================================================
# 3-panel spaghetti plot: one panel per cancer group (estimated wait time)
# ============================================================

period_adj_tww <- 7  # for 1m analyses

group_display_labels <- c(
  "primary_high_detection" = "Primary high-detection",
  "sec_exp_high_detection" = "Expanded high-detection",
  "sec_low_detection" = "Low-detection"
)

spaghetti_colors <- c("Participating in trial" = "#E69F00", "Not participating in trial" = "#56B4E9")

spaghetti_ewt_3panel_data <- map_dfr(names(cancer_groups), function(gname) {
  gdata <- baseData %>%
    filter(suspected_cancer_or_breast_symptomatic %in% cancer_groups[[gname]])

  gdata %>%
    group_by(Cancer.Alliance, monthNum) %>%
    summarise(
      tww_within_14_day_detail = sum(tww_within_14_day_detail),
      tww_in_15_to_16_days = sum(tww_in_15_to_16_days),
      tww_in_17_to_21_days = sum(tww_in_17_to_21_days),
      tww_in_22_to_28_days = sum(tww_in_22_to_28_days),
      tww_after_28_days = sum(tww_after_28_days),
      tww_total = sum(tww_total),
      mced_treated = mean(mced_treated),
      .groups = "drop"
    ) %>%
    mutate(
      estWaitTime = ((tww_within_14_day_detail * 7) +
                       (tww_in_15_to_16_days * 15.5) +
                       (tww_in_17_to_21_days * 19) +
                       (tww_in_22_to_28_days * 25) +
                       (tww_after_28_days * 32)) / tww_total,
      periodNum = monthNum - period_adj_tww,
      group = factor(ifelse(mced_treated == 1, "Participating in trial", "Not participating in trial"),
                     levels = c("Participating in trial", "Not participating in trial")),
      cancer_group = factor(group_display_labels[gname], levels = group_display_labels)
    )
})

group_levels <- levels(spaghetti_ewt_3panel_data$cancer_group)

# Compute group means
means_ewt_3panel <- spaghetti_ewt_3panel_data %>%
  group_by(cancer_group, group, periodNum) %>%
  summarise(outcome = mean(estWaitTime), .groups = "drop")

# Compute y-axis range across all panels
ewt_y_min <- 0
ewt_y_max <- max(c(spaghetti_ewt_3panel_data$estWaitTime, means_ewt_3panel$outcome), na.rm = TRUE)
ewt_y_max <- ceiling(ewt_y_max)  # round up to nearest integer day

# Helper: build a single spaghetti estimated wait time panel
build_spaghetti_ewt_panel <- function(i, x_labels) {
  gdata <- spaghetti_ewt_3panel_data %>% filter(cancer_group == group_levels[i])
  gmeans <- means_ewt_3panel %>% filter(cancer_group == group_levels[i])
  is_first <- (i == 1)

  yr <- ewt_y_max - ewt_y_min
  bracket_y <- ewt_y_min - 0.16 * yr
  tick_y_top <- ewt_y_min - 0.13 * yr
  label_y <- ewt_y_min - 0.21 * yr
  sz_annot <- sz_uniform / ggplot2::.pt

  p <- ggplot() +
    geom_line(data = gdata,
              aes(x = periodNum, y = estWaitTime, group = Cancer.Alliance, color = group),
              alpha = 0.25, linewidth = 0.4) +
    geom_line(data = gmeans,
              aes(x = periodNum, y = outcome, color = group),
              linewidth = 1.2) +
    geom_point(data = gmeans,
               aes(x = periodNum, y = outcome, fill = group),
               shape = 21, size = 2.0, color = "black", stroke = 0.38, alpha = 0.7) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey40", linewidth = 0.5) +
    scale_color_manual(values = spaghetti_colors, name = NULL,
                       labels = c("Participating in trial" = "Participating regions",
                                  "Not participating in trial" = "Non-participating regions")) +
    scale_fill_manual(values = spaghetti_colors, guide = "none") +
    scale_x_continuous(limits = c(-6.7, 23.7), breaks = breaks_vec_tww, labels = x_labels,
                       expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(ylim = c(ewt_y_min, ewt_y_max), clip = "off") +
    labs(title = NULL, x = NULL, y = "Estimated wait time (days)") +
    dots_panel_theme

  # Panel label and title
  p <- p +
    annotate("label", x = -6.5, y = ewt_y_max + 0.05 * yr, label = panel_labels[i],
             fontface = "bold", size = sz_annot, hjust = 0, vjust = 0,
             linewidth = 0.4, label.r = unit(0, "lines"),
             fill = "white", color = "black",
             label.padding = unit(0.2, "lines")) +
    annotate("text", x = -5.3, y = ewt_y_max + 0.06 * yr, label = panel_titles[i],
             fontface = "plain", size = sz_annot, hjust = 0, vjust = 0)

  # Year brackets
  for (b in seq_len(nrow(year_brackets_tww))) {
    xs <- year_brackets_tww$x_start[b]
    xe <- year_brackets_tww$x_end[b]
    xm <- (xs + xe) / 2
    p <- p + annotate("segment", x = xs, xend = xe, y = bracket_y, yend = bracket_y,
                       color = "black", linewidth = 0.4)
    p <- p + annotate("segment", x = xs, xend = xs, y = bracket_y, yend = tick_y_top,
                       color = "black", linewidth = 0.4)
    p <- p + annotate("segment", x = xe, xend = xe, y = bracket_y, yend = tick_y_top,
                       color = "black", linewidth = 0.4)
    p <- p + annotate("text", x = xm, y = label_y, label = year_brackets_tww$year_label[b],
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

# Build 3-panel spaghetti figure for estimated wait time
spaghetti_ewt_panels <- lapply(seq_along(group_levels), function(i) {
  build_spaghetti_ewt_panel(i, x_labels_2line_tww)
})
spaghetti_3panel_ewt <- wrap_plots(spaghetti_ewt_panels, ncol = 1)
ggsave(paste0(resultsDirPath, "TWW_SpaghettiPlot_3Panel_estWaitTime_1m.png"),
       spaghetti_3panel_ewt, width = 18, height = 20, units = "in", dpi = 300)
ggsave(paste0(resultsDirPath, "TWW_SpaghettiPlot_3Panel_estWaitTime_1m.eps"),
       spaghetti_3panel_ewt, width = 18, height = 20, units = "in", device = "eps")
