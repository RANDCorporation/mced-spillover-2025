# title: "Create MCED Panel Dataset"
# author: "Joshua Eagan and Sean Mann"
# date: "23 July 2025"
# output: github_document

# Description
# This program grabs data from the provided URLs and processes them to generate a panel dataset for analysis of diagnostic delays in NHS England.

# ### Read in Analytic Data

library(tidyverse)
library(readxl)

# # # Global parameters start # # #

# Set working directory path
dirPath <- "data/"
covariateDirPath <- paste0(dirPath, "raw_covariates/")
linkingDirPath <- paste0(dirPath, "linking_files/")
outDirPath <- paste0(dirPath, "cleaned_data/")

# Uncomment this and all other lines that refer to workingDirPath if you want to output intermediate data processing files for step-by-step quality checking purposes
workingDirPath <- paste0(dirPath, "sean_working/")

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

data_summary_stats <- data.frame(label = character(), value = numeric())

data = list(
  # Apr 2024 - Sep 2024
  data_2024_09 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/02/3.-28-Day-Faster-Diagnosis-By-Route-and-Suspected-Cancer-Provider-Data-1.csv", skip =8),
  data_2024_08 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/02/3.-28-Day-Faster-Diagnosis-By-Route-and-Suspected-Cancer-Provider-Data-2.csv", skip =8),
  data_2024_07 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/02/3.-28-Day-Faster-Diagnosis-By-Route-and-Suspected-Cancer-Provider-Data-3.csv", skip =8),
  data_2024_06 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/02/3.-28-Day-Faster-Diagnosis-By-Route-and-Suspected-Cancer-Provider-Data-4.csv", skip =8),
  data_2024_05 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/02/3.-28-Day-Faster-Diagnosis-By-Route-and-Suspected-Cancer-Provider-Data-5.csv", skip =8),
  data_2024_04 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/02/3.-28-Day-Faster-Diagnosis-By-Route-and-Suspected-Cancer-Provider-Data-6.csv", skip =8),

  # Oct 2023 - Mar 2024
  data_2024_03 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/05/3.-28-Day-Faster-Diagnosis-By-Route-and-Suspected-Cancer-Provider-Data.csv", skip =8),
  data_2024_02 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/07/3.-28-Day-Faster-Diagnosis-By-Route-and-Suspected-Cancer-Provider-Data-2.csv", skip =8),
  data_2024_01 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/07/3.-28-Day-Faster-Diagnosis-By-Route-and-Suspected-Cancer-Provider-Data-3.csv", skip =8),
  data_2023_12 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/07/3.-28-Day-Faster-Diagnosis-By-Route-and-Suspected-Cancer-Provider-Data-4.csv", skip =8),
  data_2023_11 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/07/3.-28-Day-Faster-Diagnosis-By-Route-and-Suspected-Cancer-Provider-Data-6.csv", skip =8),
  data_2023_10 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/07/3.-28-Day-Faster-Diagnosis-By-Route-and-Suspected-Cancer-Provider-Data-5.csv", skip =8),

  # older
  data_2023_09 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/01/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data.csv", skip =6),
  data_2023_08 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/01/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-8.csv", skip =6),
  data_2023_07 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/01/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-2.csv", skip =6),
  data_2023_06 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/01/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-3.csv", skip =6),
  data_2023_05 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/01/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-4.csv", skip =6),
  data_2023_04 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/01/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-5.csv", skip =6),
  data_2023_03 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/07/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-1.csv", skip =6),
  data_2023_02 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/07/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-2.csv", skip =6),
  data_2023_01 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/07/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-3.csv", skip =6),
  data_2022_12 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/07/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-4.csv", skip =6),
  data_2022_11 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/07/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-5.csv", skip =6),
  data_2022_10 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/07/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-6.csv", skip =6),
  data_2022_09 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/04/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-1.csv", skip =6),
  data_2022_08 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/04/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-2.csv", skip =6),
  data_2022_07 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/04/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-3.csv", skip =6),
  data_2022_06 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/04/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-4.csv", skip =6),
  data_2022_05 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/04/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-5.csv", skip =6),
  data_2022_04 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/04/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-6.csv", skip =6),
  data_2022_03 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-1.csv", skip =6),
  data_2022_02 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-2.csv", skip =6),
  data_2022_01 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-3.csv", skip =6),
  data_2021_12 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-4.csv", skip =6),
  data_2021_11 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-5.csv", skip =6),
  data_2021_10 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-6.csv", skip =6),
  data_2021_09 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-7.csv", skip =6),
  data_2021_08 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-8.csv", skip =6),
  data_2021_07 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-9.csv", skip =6),
  data_2021_06 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-10.csv", skip =6),
  data_2021_05 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-11.csv", skip =6),
  data_2021_04 = read.csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/16.-28-Day-Faster-Diagnosis-By-route-and-suspected-cancer-or-breast-symptomatic-Provider-Data-12.csv", skip =6)
)

# Standardize column names across source datasets since "..2." was added to the suspected cancer site column name starting in Oct 2023
for(n in names(data)){
  # Fix the inconsistent column name
  names(data[[n]])[names(data[[n]]) == "SUSPECTED.CANCER.OR.BREAST.SYMPTOMATIC..2."] <- "SUSPECTED.CANCER.OR.BREAST.SYMPTOMATIC"

  data[[n]]$date = paste0(substr(n, 6, 12), "_01")
}

# making sure the datasets line up - remove if not needed
# lapply(data, dim)
# lapply(data, names)
# purrr::map(data, ~table(names(.x) %in% names(data[[1]])))

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
data = data %>% select(-c(x, x_)) %>%
  rename(Provider.Code = ods_code_1_,
         told_diagnosis_outcome_total = total,
         told_diagnosis_outcome_within_28_days = within_28_days,
         told_diagnosis_outcome_after_28_days = after_28_days,
         percentage_told_diagnosis_outcome_within_28_days = told_within_28_days,
         told_diagnosis_outcome_within_14_days = within_14_days,
         told_diagnosis_outcome_in_29_to_42_days = in_29_to_42_days,
         told_diagnosis_outcome_in_43_to_62_days = in_43_to_62_days,
         told_diagnosis_outcome_after_62_days = after_62_days
         ) %>%
  select(date, percentage_told_diagnosis_outcome_within_28_days, everything())

write.csv(data, file = "data/sean_working/28day_clean.csv")

# Remove all rows except for rows that contain organization-level data (this removes empty 'NA' rows and rows with regional totals)
rows_to_keep <- data$Provider.Code != ""
data <- data[rows_to_keep, ]

# Remove all rows for referrals for breast symptoms where cancer is not initially suspected, referrals missing values, and referrals for non-specific symptoms
data <- subset(data, !(suspected_cancer_or_breast_symptomatic %in% c("Exhibited (non-cancer) breast symptoms - cancer not initially suspected", "Missing or invalid", "Suspected cancer - non-specific symptoms")))

# Export processed file for quality checking purposes
# write.csv(data, file = paste0(workingDirPath, "28day_clean_test1.csv"))

# # # Calculation start # # #

# Calculate numbers of all referrals that are for groupings of cancer types # # #

for (group_name in names(cancer_groups)) {
  # Filter and calculate the total sum for the current group
  group_sum <- data %>%
    filter(suspected_cancer_or_breast_symptomatic %in% cancer_groups[[group_name]]) %>%
    summarise(total = sum(told_diagnosis_outcome_total, na.rm = TRUE)) %>%
    pull(total)

  # Add a row to data_summary_stats with the current group's total
  data_summary_stats <- data_summary_stats %>%
    add_row(
      label = paste0("calculation_Total referrals for ", group_name, " cancers: ", paste(cancer_groups[[group_name]], collapse = ", ")),
      value = group_sum
    )
  print("here")
  print(group_name)
  print(paste(cancer_groups[[group_name]], collapse = ", "))
}

write.csv(data_summary_stats, paste0(outDirPath, "dataSummaryStats.csv"), row.names = FALSE)

# # # Calculation end # # #

# # # Linking data to cancer alliances start # # #

# Function to combine and then remove post-merge duplicate columns with .x and .y suffixes.
CombineSuffixColumns <- function(inDf, checkCols = TRUE) {
  # Get the column names of the dataframe
  col_names <- colnames(inDf)

  # Iterate through the column names
  for (col in col_names) {
    # Check if the column name ends with .x or .y
    if (grepl("\\.x$", col)) {
      print(col)
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
          # write.csv(subset_df, file = paste0(workingDirPath, "mismatched.csv"))
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

  # Export processed file for quality checking purposes
  # write.csv(df, file = paste0(workingDirPath, dfName, "link1.csv"))

  # Remove duplicate .x and .y post-merge columns
  df <- CombineSuffixColumns(df, check)

  # Export processed file for quality checking purposes
  # write.csv(df, file = paste0(workingDirPath, dfName, "link1a.csv"))

  # Remove unnecessary ICB-level columns from dataframe
  icbToAlliance <- icbToAlliance[, -which(names(icbToAlliance) %in% c("Sub.ICB.E.Code", "SICBL22CDH", "SICBL22NM", "ICB.E.code"))]
  # Then remove all duplicate rows so only one row per ICB.Code UID
  icbToAlliance <- unique(icbToAlliance)

  # Merge dataframe containing Cancer Alliance assignment linked to ICB codes into main dataset
  df <- merge(df, icbToAlliance, by = 'ICB.Code', all.x = TRUE)

  # Remove duplicate .x and .y post-merge columns
  df <- CombineSuffixColumns(df, check)

  # Export processed file for quality checking purposes
  # write.csv(df, file = paste0(workingDirPath, dfName, "link2.csv"))

  # Third input file concerns providers that did not link to Cancer Alliances via above linking files.
  # These providers had to be linked to Cancer Alliances manually, based on publicly available information (see "Source" column in input file)
  # Manual linking was conducted on May 23, 2024.
  trustToAlliance = read.csv(paste0(linkingDirPath, "ProviderToCAMappingByHand.csv"))

  # Merge dataframe containing manually identified Cancer Alliances linked to provider codes into main dataset
  df <- merge(df, trustToAlliance, by = 'Provider.Code', all.x = TRUE)

  # Remove duplicate .x and .y post-merge columns
  df <- CombineSuffixColumns(df, check)

  # Export processed file for quality checking purposes
  # write.csv(df, file = paste0(workingDirPath, dfName, "link4.csv"))

  # Remove rows for manually linked providers (private providers) that operate in multiple Cancer Alliance regions and so can't be directly linked to any single alliance
  df <- df %>%
    filter(Cancer.Alliance != "multiple" | is.na(Cancer.Alliance))

  # Export processed file for quality checking purposes
  # write.csv(df, file = paste0(workingDirPath, dfName, "link5.csv"))

  # Export processed file for quality checking purposes
  # write.csv(df, file = paste0(workingDirPath, dfName, "link6.csv"))

  # Load manually created crosswalk file that indicates which Cancer Alliances participated in NHS-Galleri trial (n=8) and which did not participate (n=13)
  # Identification of treated Cancer Alliances is from NHS Galleri published protocol, Figure 2
  # see https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9564213/ (accessed July 9, 2024)
  mced = read.csv(paste0(linkingDirPath, "McedToCAMappingByHand.csv"))

  # Merge in column identifying which providers were part of NHS Galleri/MCED trial and which were not
  df <- merge(df, mced, by = 'Cancer.Alliance', all.x = TRUE)

  # Remove duplicate .x and .y post-merge columns
  df <- CombineSuffixColumns(df, check)

  # Export processed file for quality checking purposes
  # write.csv(df, file = paste0(workingDirPath, dfName, "link7.csv"))

  return(df)
}

data <- LinkToCA(data, "28day_clean")

# Export processed file for quality checking purposes
# write.csv(data, file = paste0(workingDirPath, "28day_clean_test2.csv"))

# # # Linking data to cancer alliances end # # #


# # # Staff and Absences and Bed Occupancy Covariate Code Start # # #

# # Code to process provider-level staff numbers and absences for use in covariate
# # First section processes total staff numbers
# # Downloaded input data file on July 22, 2025. Source URL is https://files.digital.nhs.uk/D2/C38F95/NHS%20Workforce%20Statistics%2C%20March%202025%20England%20and%20Organisation.xlsx
# # Pre-processing before using as input here: opened file in excel, navigated to Table 5 (Sheet: "5. All Staff, NHSE & Org - FTE") and then saved as .csv file

staff = read.csv(paste0(covariateDirPath, "NHS Workforce Statistics, March 2025 England and Organisation.csv"), skip=4)

# Export processed file for quality checking purposes
# write.csv(staff, file = paste0(workingDirPath, "staff0.csv"))

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

# Export processed file for quality checking purposes
# write.csv(staff, file = paste0(workingDirPath, "staff.csv"))

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

# Export processed file for quality checking purposes
# write.csv(staff, file = paste0(workingDirPath, "staff1.csv"))

# Link the staff provider-level data to Cancer Alliances
staff <- LinkToCA(staff, "staff")

# Export processed file for quality checking purposes
# write.csv(staff, file = paste0(workingDirPath, "staff2.csv"))

# Merge dataframe containing total staff numbers by provider and month into main dataframe
data <- merge(data, staff, by = c('Provider.Code', 'date'), all.x = TRUE)

# Export processed file for quality checking purposes
# write.csv(data, file = paste0(workingDirPath, "28day_clean_test2a.csv"))

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
ProcessCovidData <- function(dataFramesList, valueColumnName, workingDirPath = NULL) {
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

    # Export with unique filename using index (if workingDirPath provided)
    if (!is.null(workingDirPath)) {
      write.csv(df, file = paste0(workingDirPath, valueColumnName, "1_", i, ".csv"))
    }

    # Rename organizational UID column
    df <- df %>%
      rename(Provider.Code = Code)

    if (!is.null(workingDirPath)) {
      write.csv(df, file = paste0(workingDirPath, valueColumnName, "1a_", i, ".csv"))
    }

    # Remove rows with NA Provider.Code
    df <- df[!is.na(df$Provider.Code), ]

    if (!is.null(workingDirPath)) {
      write.csv(df, file = paste0(workingDirPath, valueColumnName, "1b_", i, ".csv"))
    }

    # Transform dataframe from wide to long format
    df <- df %>%
      pivot_longer(
        cols = -c(Provider.Code, Name),
        names_to = "date",
        values_to = valueColumnName
      )

    if (!is.null(workingDirPath)) {
      write.csv(df, file = paste0(workingDirPath, valueColumnName, "1c_", i, ".csv"))
    }

    return(df)
  })

  # Check that all dataframes have consistent column names
  lapply(processedDfs, names)

  # Combine all dataframes into single dataset
  combinedData <- Reduce(rbind, processedDfs)

  # Export processed file for quality checking purposes
  if (!is.null(workingDirPath)) {
    write.csv(combinedData, file = paste0(workingDirPath, valueColumnName, "2.csv"))
  }

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

  # Export processed file for quality checking purposes
  if (!is.null(workingDirPath)) {
    write.csv(combinedData, file = paste0(workingDirPath, valueColumnName, "3.csv"))
  }

  # Link provider-level data to Cancer Alliances
  # Note: This assumes LinkToCA function is available in the environment
  # Do not throw error for mismatched post-merge columns
  combinedData <- LinkToCA(combinedData, valueColumnName, check = FALSE)

  # Export processed file for quality checking purposes
  if (!is.null(workingDirPath)) {
    write.csv(combinedData, file = paste0(workingDirPath, valueColumnName, "4.csv"))
  }

  return(combinedData)
}

# Usage examples:

# Process absences data
absences <- ProcessCovidData(
  dataFramesList = absencesDfs,
  valueColumnName = "totalAbsent",
)

# Process beds data
beds <- ProcessCovidData(
  dataFramesList = bedDfs,
  valueColumnName = "totalBedsOccupied",
)

# Merge dataframe containing total absent staff numbers by provider and month into main dataframe
data <- merge(data, absences, by = c('Provider.Code', 'date'), all.x = TRUE)

# Remove duplicate .x and .y post-merge columns
# Do not throw error for mismatched post-merge columns; Sean checked these by hand 28 July 2024 and all are fine
data <- CombineSuffixColumns(data, check = FALSE)

# Merge dataframe containing total COVID-19 bed occupancy numbers by provider and month into main dataframe
data <- merge(data, beds, by = c('Provider.Code', 'date'), all.x = TRUE)

# Remove duplicate .x and .y post-merge columns
# Do not throw error for mismatched post-merge columns; Sean checked these by hand 28 July 2024 and all are fine
data <- CombineSuffixColumns(data, check = FALSE)

# Export processed file for quality checking purposes
# write.csv(data, file = paste0(workingDirPath, "28day_clean_test2b.csv"))

# Convert string-format numbers into numeric data
data <- data %>%
  mutate(totalAbsent = as.numeric(gsub(",", "", totalAbsent))) %>%
  mutate(totalBedsOccupied = as.numeric(gsub(",", "", totalBedsOccupied)))

data <- data %>%
  mutate(totalStaff = as.numeric(gsub(",", "", totalStaff)))

# Exclude total staff numbers for providers for which we don't have absence numbers, and vice-versa
# This only excludes a very small number of staff and absences (<1%) from a couple providers
data <- data %>%
  mutate(totalStaff = if_else(is.na(totalAbsent), NA_real_, totalStaff)) %>%
  mutate(totalAbsent = if_else(is.na(totalStaff), NA_real_, totalAbsent))

# # # Staff and Absences and Bed Occupancy Code End # # #

# calculating number of patients facing diagnostic delay
data$num_not_told_diagnosis_outcome_within_28_days = (data$told_diagnosis_outcome_total - data$told_diagnosis_outcome_within_28_days)

# convert dates in string format to date format
data$date <- gsub("_", "-", data$date)
data$date <- as.Date(data$date)

# calculate month number, starting with the first month in the dataset, which is Apr 2021
data$monthNum = as.numeric((year(data$date)-2021)*12 +(month(data$date)) - 3)

# Export processed file
write.csv(data, file = paste0(outDirPath, "base_data.csv"))
