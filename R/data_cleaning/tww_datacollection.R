# title: "Create TWW Panel Dataset"
# author: "Sean Mann"
# date: "5 March 2026"
# output: github_document

# Description
# This program grabs Two Week Wait (TWW) data from NHS England URLs and processes them to generate a panel dataset
# for analysis of TWW breach rates and estimated wait times. Follows the same structure as mced_panel_datacollection.R.

# ### Read in Analytic Data

library(here)
library(tidyverse)
library(readxl)

# # # Global parameters start # # #

# Set working directory path
dirPath <- paste0(here("data/"), "/")
covariateDirPath <- paste0(dirPath, "raw_covariates/")
linkingDirPath <- paste0(dirPath, "linking_files/")
outDirPath <- paste0(dirPath, "cleaned_data/")

cancer_groups <- list(
  exclusively_protocol_not_screened = c("Suspected head & neck cancer",
                                        "Suspected lung cancer",
                                        "Suspected upper gastrointestinal cancer"),
  contains_protocol = c("Suspected lower gastrointestinal cancer",
                        "Suspected head & neck cancer",
                        "Suspected lung cancer",
                        "Suspected upper gastrointestinal cancer",
                        "Suspected gynaecological cancer",
                        "Suspected haematological malignancies (excluding acute leukaemia)",
                        "Suspected urological malignancies (excluding testicular)"),
  does_not_contain_protocol = c("Suspected skin cancer",
                                "Suspected breast cancer",
                                "Suspected sarcoma",
                                "Suspected brain/central nervous system tumours",
                                "Suspected testicular cancer",
                                "Suspected acute leukaemia",
                                "Suspected other cancer")
)

# # # Global parameters end # # #

# Helper function to read TWW CSVs which have 6 rows (4 metadata + 1 empty + 1 sub-header) before the column header
read_tww_csv <- function(url) {
  read.csv(url, skip = 6)
}

data_summary_stats <- data.frame(label = character(), value = numeric())

data = list(
  # Apr 2023 - Sep 2023 (published at /2024/01/)
  data_2023_09 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/01/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data.csv"),
  data_2023_08 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/01/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-8.csv"),
  data_2023_07 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/01/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-2.csv"),
  data_2023_06 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/01/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-3.csv"),
  data_2023_05 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/01/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-4.csv"),
  data_2023_04 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/01/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-5.csv"),

  # Oct 2022 - Mar 2023 (published at /2023/07/)
  data_2023_03 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/07/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-1.csv"),
  data_2023_02 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/07/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-2.csv"),
  data_2023_01 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/07/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-3.csv"),
  data_2022_12 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/07/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-4.csv"),
  data_2022_11 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/07/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-5.csv"),
  data_2022_10 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/07/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-6.csv"),

  # Apr 2022 - Sep 2022 (published at /2023/04/)
  data_2022_09 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/04/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-1.csv"),
  data_2022_08 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/04/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-2.csv"),
  data_2022_07 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/04/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-3.csv"),
  data_2022_06 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/04/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-4.csv"),
  data_2022_05 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/04/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-5.csv"),
  data_2022_04 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/04/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-6.csv"),

  # Apr 2021 - Mar 2022 (published at /2022/10/)
  data_2022_03 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-1.csv"),
  data_2022_02 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-2.csv"),
  data_2022_01 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-3.csv"),
  data_2021_12 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-4.csv"),
  data_2021_11 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-5.csv"),
  data_2021_10 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-6.csv"),
  data_2021_09 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-7.csv"),
  data_2021_08 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-8.csv"),
  data_2021_07 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-9.csv"),
  data_2021_06 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-10.csv"),
  data_2021_05 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-11.csv"),
  data_2021_04 = read_tww_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/3.-Two-Week-Wait-By-Suspected-Cancer-Provider-Data-12.csv")
)

# Standardize column names and add date column
for(n in names(data)){
  data[[n]]$date = paste0(substr(n, 6, 12), "_01")
}

# appending
data = Reduce(rbind, data)

###############
#
# Basic cleaning
#
###############

# renaming variables
names(data) = gsub("\\.", "_", names(data))
names(data) = gsub("__", "_", names(data))
names(data) = tolower(names(data))

# Remove blank/artifact columns (columns named x, x_, or similar empty columns from CSV)
data <- data %>% select(-matches("^x_?$"))

# Rename columns to analysis-friendly names
# TWW uses "SUSPECTED TYPE OF CANCER" (vs FDS's "SUSPECTED CANCER OR BREAST SYMPTOMATIC")
# Renaming to suspected_cancer_or_breast_symptomatic to match cancer_groups definitions
data <- data %>%
  rename(Provider.Code = ods_code_1_,
         suspected_cancer_or_breast_symptomatic = suspected_type_of_cancer,
         tww_total = total,
         tww_within_14_days = within_14_days,
         tww_after_14_days = after_14_days,
         tww_within_14_day_detail = within_14_day,
         tww_in_15_to_16_days = in_15_to_16_days,
         tww_in_17_to_21_days = in_17_to_21_days,
         tww_in_22_to_28_days = in_22_to_28_days,
         tww_after_28_days = after_28_days)

# Remove all rows except for rows that contain organization-level data (this removes empty 'NA' rows and rows with regional totals)
rows_to_keep <- data$Provider.Code != ""
data <- data[rows_to_keep, ]

data_summary_stats <- data_summary_stats %>%
  add_row(
    label = "calculation_Prior to data cleaning and linking, the NHS England TWW dataset reported that XXXXXXXXX patient referrals for suspected cancer were seen during the study period",
    value = sum(data$tww_total, na.rm = TRUE)
  )

# Calculate and remove all rows for referrals for breast symptoms where cancer is not initially suspected, referrals missing values, and referrals for non-specific symptoms
beforeExcl = sum(data$tww_total, na.rm = TRUE)
data <- subset(data, !(suspected_cancer_or_breast_symptomatic %in% c("Exhibited (non-cancer) breast symptoms - cancer not initially suspected")))
afterExcl = sum(data$tww_total, na.rm = TRUE)
data_summary_stats <- data_summary_stats %>%
  add_row(
    label = "calculation_We excluded XXXXX referrals labeled 'Exhibited (non-cancer) breast symptoms - cancer not initially suspected' from our analysis",
    value = beforeExcl - afterExcl
  )

beforeExcl = sum(data$tww_total, na.rm = TRUE)
data <- subset(data, !(suspected_cancer_or_breast_symptomatic %in% c("Missing or Invalid")))
afterExcl = sum(data$tww_total, na.rm = TRUE)
data_summary_stats <- data_summary_stats %>%
  add_row(
    label = "calculation_We excluded XXXXX referrals labeled 'Missing or Invalid' from our analysis",
    value = beforeExcl - afterExcl
  )

beforeExcl = sum(data$tww_total, na.rm = TRUE)
data <- subset(data, !(suspected_cancer_or_breast_symptomatic %in% c("Suspected cancer - non-specific symptoms")))
afterExcl = sum(data$tww_total, na.rm = TRUE)
data_summary_stats <- data_summary_stats %>%
  add_row(
    label = "calculation_We excluded XXXXX referrals labeled 'Suspected cancer - non-specific symptoms' from our analysis",
    value = beforeExcl - afterExcl
  )

beforeExcl = sum(data$tww_total, na.rm = TRUE)
data <- subset(data, !(suspected_cancer_or_breast_symptomatic %in% c("Suspected children's cancer")))
afterExcl = sum(data$tww_total, na.rm = TRUE)
data_summary_stats <- data_summary_stats %>%
  add_row(
    label = "calculation_We excluded XXXXX referrals labeled 'Suspected children's cancer' from our analysis",
    value = beforeExcl - afterExcl
  )

data_summary_stats <- data_summary_stats %>%
  add_row(
    label = "calculation_After these exclusions, XXXX referrals remained in our dataset",
    value = afterExcl
  )

# # # Linking data to cancer alliances start # # #

# Function to combine and then remove post-merge duplicate columns with .x and .y suffixes.
CombineSuffixColumns <- function(inDf, checkCols = TRUE) {
  # Get the column names of the dataframe
  col_names <- colnames(inDf)

  # Iterate through the column names
  for (col in col_names) {
    # Check if the column name ends with .x or .y
    if (grepl("\\.x$", col)) {
      # Get the base name of the column
      baseCol <- sub("\\.[xy]$", "", col)
      xCol <- col
      yCol <- paste0(baseCol, ".y")

      # Check for inconsistencies in columns
      inDf <- inDf %>%
        mutate(
          code_mismatch = ifelse(!is.na(.data[[xCol]]) & !is.na(.data[[yCol]]) & tolower(.data[[xCol]]) != tolower(.data[[yCol]]), TRUE, FALSE)
        )

      if (checkCols) {
        # Throw an error if there are mismatched codes
        if (any(inDf$code_mismatch)) {
          subset_df <- inDf %>%
            filter(code_mismatch == TRUE)
          stop("Mismatched values found in data")
        }
      }

      # Combine the two merged ICB Code columns into one
      inDf <- inDf %>%
        mutate({{baseCol}} := coalesce(.data[[xCol]], .data[[yCol]]))

      # Remove the .x and .y and code_mismatch columns
      inDf <- inDf %>%
        select(-all_of(c(xCol, yCol, "code_mismatch")))
    }
  }
  return(inDf)
}

# # Linking provider-level data to ICBs and Cancer Alliances
LinkToCA <- function(df, dfName, check = TRUE) {
  # First input file links provider codes to ICB codes
  # Trust-ICB-Attribution-File.csv
  # Downloaded from https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/02/Trust-ICB-Attribution-File.xls
  # Downloaded on July 22, 2025
  # Pre-processing prior to using as input here: opened file in excel and saved as csv
  trustToICB = read.csv(paste0(linkingDirPath, "Trust-ICB-Attribution-File.csv"), skip=7)

  # Second input file links ICB (Integrated Care Board) codes to Cancer Alliances
  # Sub-ICB-to-Alliance-mapping-2022_v1.4.csv
  # Downloaded from https://www.ncpes.co.uk/wp-content/uploads/2023/07/Sub-ICB-to-Alliance-mapping-2022_v1.4.csv (accessed from https://www.ncpes.co.uk/survey-instructions/)
  # Downloaded on July 22, 2025
  # ICB to Cancer Alliance crosswalk file from National Cancer Patient Experience Survey official website
  icbToAlliance = read.csv(paste0(linkingDirPath, "Sub-ICB-to-Alliance-mapping-2022_v1.4.csv"))

  # Rename organizational UID column so it matches UID column (Provider.Code) in main dataset
  trustToICB <- trustToICB %>%
    rename(
      Provider.Code = Code
    )

  # Merge dataframe containing linked ICB codes into main dataset
  df <- merge(df, trustToICB, by = 'Provider.Code', all.x = TRUE)

  # Remove duplicate .x and .y post-merge columns
  df <- CombineSuffixColumns(df, check)

  # Remove unnecessary ICB-level columns from dataframe
  icbToAlliance <- icbToAlliance[, -which(names(icbToAlliance) %in% c("Sub.ICB.E.Code", "SICBL22CDH", "SICBL22NM", "ICB.E.code"))]
  # Then remove all duplicate rows so only one row per ICB.Code UID
  icbToAlliance <- unique(icbToAlliance)

  # Merge dataframe containing Cancer Alliance assignment linked to ICB codes into main dataset
  df <- merge(df, icbToAlliance, by = 'ICB.Code', all.x = TRUE)

  # Remove duplicate .x and .y post-merge columns
  df <- CombineSuffixColumns(df, check)

  # Third input file concerns providers that did not link to Cancer Alliances via above linking files.
  # These providers had to be linked to Cancer Alliances manually, based on publicly available information (see "Source" column in input file)
  # Manual linking was conducted on May 23, 2024.
  trustToAlliance = read.csv(paste0(linkingDirPath, "ProviderToCAMappingByHand.csv"))

  # Merge dataframe containing manually identified Cancer Alliances linked to provider codes into main dataset
  df <- merge(df, trustToAlliance, by = 'Provider.Code', all.x = TRUE)

  # Remove duplicate .x and .y post-merge columns
  df <- CombineSuffixColumns(df, check)

  # Remove rows for manually linked providers (private providers) that operate in multiple Cancer Alliance regions and so can't be directly linked to any single alliance
  df <- df %>%
    filter(Cancer.Alliance != "multiple" | is.na(Cancer.Alliance))

  # Load manually created crosswalk file that indicates which Cancer Alliances participated in NHS-Galleri trial (n=8) and which did not participate (n=13)
  # Identification of treated Cancer Alliances is from NHS Galleri published protocol, Figure 2
  # see https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9564213/ (accessed July 9, 2024)
  mced = read.csv(paste0(linkingDirPath, "McedToCAMappingByHand.csv"))

  # Merge in column identifying which providers were part of NHS Galleri/MCED trial and which were not
  df <- merge(df, mced, by = 'Cancer.Alliance', all.x = TRUE)

  # Remove duplicate .x and .y post-merge columns
  df <- CombineSuffixColumns(df, check)

  return(df)
}

before = sum(data$tww_total, na.rm = TRUE)
data <- LinkToCA(data, "tww_clean")
after = sum(data$tww_total, na.rm = TRUE)

data_summary_stats <- data_summary_stats %>%
  add_row(
    label = "calculation_This manual review resulted in the exclusion of XXXXXX referrals associated with providers that provide services across multiple regions",
    value = before - after
  )

data_summary_stats <- data_summary_stats %>%
  add_row(
    label = "calculation_Following data cleaning and linking, our TWW study sample included XXXXXX cancer referrals",
    value = after
  )

# # # Linking data to cancer alliances end # # #


# # # Staff and Absences and Bed Occupancy Covariate Code Start # # #

# # Code to process provider-level staff numbers and absences for use in covariate
# # First section processes total staff numbers
# # Downloaded input data file on July 22, 2025. Source URL is https://files.digital.nhs.uk/D2/C38F95/NHS%20Workforce%20Statistics%2C%20March%202025%20England%20and%20Organisation.xlsx
# # Pre-processing before using as input here: opened file in excel, navigated to Table 5 (Sheet: "5. All Staff, NHSE & Org - FTE") and then saved as .csv file

staff = read.csv(paste0(covariateDirPath, "NHS Workforce Statistics, March 2025 England and Organisation.csv"), skip=4)

# Remove all rows except for rows that contain organization-level data (this removes empty regional subheader rows and informational footer)
rows_to_keep <- staff$Organisation.code != ""
staff <- staff[rows_to_keep, ]

# Remove the first four columns that contain region-level info. This leaves only the organization-level data columns.
staff <- staff %>%
  select(-c(1:4))

# Remove empty right-most column, artifact of reading in as csv
staff <- staff %>%
  select(-X)

# Rename organizational UID column so it matches UID column (Provider.Code) in wait times file
staff <- staff %>%
  rename(Provider.Code = Organisation.code)

# Transform dataframe from wide provider level dataset to long provider-month level dataset
staff <- staff %>%
  pivot_longer(
    cols = -c(Organisation.name, Provider.Code),
    names_to = "date",
    values_to = "totalStaff"
  )

# Reformat monthly date column
staff <- staff %>%
  mutate(date = format(dmy(paste("01", date, sep=".")), "%Y_%m_%d"))

# Convert the date column to Date format
staff$dateFormat <- as.Date(staff$date, format = "%Y_%m_%d")
# Filter to keep only records during the TWW study period from April 2021 through September 2023
staff <- staff[staff$dateFormat >= as.Date("2021-04-01") &
                    staff$dateFormat <= as.Date("2023-09-30"), ]
# drop formatted date columm
staff <- staff[, !names(staff) %in% c("dateFormat")]

# Link the staff provider-level data to Cancer Alliances
staff <- LinkToCA(staff, "staff")

# Merge dataframe containing total staff numbers by provider and month into main dataframe
data <- merge(data, staff, by = c('Provider.Code', 'date'), all.x = TRUE)

# Remove duplicate .x and .y post-merge columns
data <- CombineSuffixColumns(data)

# # Code to process staff absence numbers
# # Downloaded these input data files on July 22, 2025
# # Source URLs available at https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/
# # Reading in data
absencesDfs = list(
  absences_2021_09_30 = read_excel(paste0(covariateDirPath, "Covid-Publication-30-09-2021-DQnotes.xlsx"), sheet = "All Absences", skip = 12),
  absences_2022_03_31 = read_excel(paste0(covariateDirPath, "Covid-Publication-31-03-2022-v2-1.xlsx"), sheet = "All Absences", skip = 12),
  absences_2022_09_30 = read_excel(paste0(covariateDirPath, "Covid-Publication-08-12-2022_220401to220930.xlsx"), sheet = "All Absences", skip = 12),
  absences_2023_05_31 = read_excel(paste0(covariateDirPath, "Covid-Publication-08-06-2023.xlsx"), sheet = "All Absences", skip = 12),
  absences_2024_03_31 = read_excel(paste0(covariateDirPath, "Covid-Publication-11-04-2024-v2.xlsx"), sheet = "All Absences", skip = 12),
  absences_2024_09_30 = read_excel(paste0(covariateDirPath, "Covid-Publication-10-10-2024-v2.0.xlsx"), sheet = "All Absences", skip = 12)
)

# # Code to process average number of hospital beds occupied by patients with COVID-19 numbers
# # Downloaded these input data files on July 22, 2025
# # Source URLs available at https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/
# # Reading in data
bedDfs = list(
  beds_2021_09_30 = read_excel(paste0(covariateDirPath, "Covid-Publication-30-09-2021-DQnotes.xlsx"), sheet = "Total Beds Occupied Covid", skip = 12),
  beds_2022_03_31 = read_excel(paste0(covariateDirPath, "Covid-Publication-31-03-2022-v2-1.xlsx"), sheet = "Total Beds Occupied Covid", skip = 12),
  beds_2022_09_30 = read_excel(paste0(covariateDirPath, "Covid-Publication-08-12-2022_220401to220930.xlsx"), sheet = "Total Beds Occupied Covid", skip = 12),
  beds_2023_05_31 = read_excel(paste0(covariateDirPath, "Covid-Publication-08-06-2023.xlsx"), sheet = "Total Beds Occupied Covid", skip = 12),
  beds_2024_03_31 = read_excel(paste0(covariateDirPath, "Covid-Publication-11-04-2024-v2.xlsx"), sheet = "Total Beds Occupied Covid", skip = 12),
  beds_2024_09_30 = read_excel(paste0(covariateDirPath, "Covid-Publication-10-10-2024-v2.0.xlsx"), sheet = "Total Beds Occupied Covid", skip = 12)
)

# Generalized function to process COVID-related data (absences or beds)
ProcessCovidData <- function(dataFramesList, valueColumnName) {
  # Process each dataframe in the list
  processedDfs <- lapply(seq_along(dataFramesList), function(i) {
    df <- dataFramesList[[i]]

    # Format date columns to be in same format as main dataset
    formattedDateCols <- sapply(colnames(df), function(x) {
      # Check if it's a numeric Excel date
      if (grepl("^[0-9]+$", x)) {
        numeric_date <- as.numeric(x)
        date_format <- format(as.Date(numeric_date, origin = "1899-12-30"), "%Y_%m_%d")
      }
      # Check if it's a text date in format like "31-Mar-2024"
      else if (grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{4}$", x)) {
        # Parse the text date and convert to desired format
        parsed_date <- as.Date(x, format = "%d-%b-%Y")
        date_format <- format(parsed_date, "%Y_%m_%d")
      }
      # Keep original column name if it doesn't match date patterns
      else {
        x
      }
    })
    colnames(df) <- formattedDateCols

    # Remove NHS England Region column if it exists
    if ("NHS England Region" %in% colnames(df)) {
      df <- df %>%
        select(-"NHS England Region")
    }

    # Rename organizational UID column
    df <- df %>%
      rename(Provider.Code = Code)

    # Remove rows with NA Provider.Code
    df <- df[!is.na(df$Provider.Code), ]

    # Transform dataframe from wide to long format
    df <- df %>%
      pivot_longer(
        cols = -c(Provider.Code, Name),
        names_to = "date",
        values_to = valueColumnName
      )

    return(df)
  })

  # Check that all dataframes have consistent column names
  lapply(processedDfs, names)

  # Combine all dataframes into single dataset
  combinedData <- Reduce(rbind, processedDfs)

  # Convert date column to proper Date format
  combinedData$date <- as.Date(combinedData$date, format = "%Y_%m_%d")

  # Group daily dataset into month-level dataset
  # For absences: averaging daily numbers to get average monthly absences
  # For beds: averaging daily numbers to get average monthly bed occupancy
  combinedData <- combinedData %>%
    group_by(Provider.Code, date = floor_date(date, "month")) %>%
    summarise(
      Name = first(Name),
      !!valueColumnName := mean(!!sym(valueColumnName), na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(date = format(date, "%Y_%m_%d"))

  # Link provider-level data to Cancer Alliances
  # Do not throw error for mismatched post-merge columns
  combinedData <- LinkToCA(combinedData, valueColumnName, check = FALSE)

  return(combinedData)
}

# Process absences data
absences <- ProcessCovidData(
  dataFramesList = absencesDfs,
  valueColumnName = "totalAbsent"
)

# Convert the date column to Date format for filtering purposes
absences$dateFormat <- as.Date(absences$date, format = "%Y_%m_%d")
# Filter to keep only records during the TWW study period from April 2021 through September 2023
absences <- absences[absences$dateFormat >= as.Date("2021-04-01") &
                       absences$dateFormat <= as.Date("2023-09-30"), ]
# drop formatted date columm
absences <- absences[, !names(absences) %in% c("dateFormat")]

# Process beds data
beds <- ProcessCovidData(
  dataFramesList = bedDfs,
  valueColumnName = "totalBedsOccupied"
)

# Convert the date column to Date format for filtering purposes
beds$dateFormat <- as.Date(beds$date, format = "%Y_%m_%d")
# Filter to keep only records during the TWW study period from April 2021 through September 2023
beds <- beds[beds$dateFormat >= as.Date("2021-04-01") &
               beds$dateFormat <= as.Date("2023-09-30"), ]
# drop formatted date columm
beds <- beds[, !names(beds) %in% c("dateFormat")]

# Merge dataframe containing total absent staff numbers by provider and month into main dataframe
data <- merge(data, absences, by = c('Provider.Code', 'date'), all.x = TRUE)

# Remove duplicate .x and .y post-merge columns
# Do not throw error for mismatched post-merge columns
data <- CombineSuffixColumns(data, check = FALSE)

# Merge dataframe containing total COVID-19 bed occupancy numbers by provider and month into main dataframe
data <- merge(data, beds, by = c('Provider.Code', 'date'), all.x = TRUE)

# Remove duplicate .x and .y post-merge columns
# Do not throw error for mismatched post-merge columns
data <- CombineSuffixColumns(data, check = FALSE)

# Convert string-format numbers into numeric data
data <- data %>%
  mutate(totalAbsent = as.numeric(gsub(",", "", totalAbsent))) %>%
  mutate(totalBedsOccupied = as.numeric(gsub(",", "", totalBedsOccupied)))

data <- data %>%
  mutate(totalStaff = as.numeric(gsub(",", "", totalStaff)))

# Exclude total staff numbers for providers for which we don't have absence numbers, and vice-versa
data <- data %>%
  mutate(totalStaff = if_else(is.na(totalAbsent), NA_real_, totalStaff)) %>%
  mutate(totalAbsent = if_else(is.na(totalStaff), NA_real_, totalAbsent))

# # # Staff and Absences and Bed Occupancy Code End # # #

# calculating number of patients not seen within 14 days (TWW breach)
data$num_not_seen_within_14_days = (data$tww_total - data$tww_within_14_days)

# convert dates in string format to date format
data$date <- gsub("_", "-", data$date)
data$date <- as.Date(data$date)

# calculate month number, starting with the first month in the dataset, which is Apr 2021
data$monthNum = as.numeric((year(data$date)-2021)*12 +(month(data$date)) - 3)

# Calculate numbers of all referrals that are for groupings of cancer types # # #

for (group_name in names(cancer_groups)) {
  # Filter and calculate the total sum for the current group
  group_sum <- data %>%
    filter(suspected_cancer_or_breast_symptomatic %in% cancer_groups[[group_name]]) %>%
    summarise(total = sum(tww_total, na.rm = TRUE)) %>%
    pull(total)

  # Add a row to data_summary_stats with the current group's total
  data_summary_stats <- data_summary_stats %>%
    add_row(
      label = paste0("calculation_Total TWW referrals for ", group_name, " cancers: ", paste(cancer_groups[[group_name]], collapse = ", ")),
      value = group_sum
    )
}

# # # Calculation end # # #

# Export processed file
write.csv(data, file = paste0(outDirPath, "tww_base_data.csv"))

write.csv(data_summary_stats, paste0(outDirPath, "twwDataSummaryStats.csv"), row.names = FALSE)
