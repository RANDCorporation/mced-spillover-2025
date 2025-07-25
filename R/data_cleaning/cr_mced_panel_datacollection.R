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
# workingDirPath <- paste0(dirPath, "sean_working/")

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

cancer_types_to_loop_through <- c(
  "Suspected breast cancer",
  "Suspected lower gastrointestinal cancer",
  "Suspected skin cancer",
  "Suspected acute leukaemia",
  "Suspected brain/central nervous system tumours",
  "Suspected children's cancer",
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

cancer_types_to_loop_through <- c(cancer_types_to_loop_through, names(cancer_groups))

# # # Global parameters end # # #

summary_stats <- data.frame(label = character(), value = numeric())

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

# making sure the datasets line up
lapply(data, dim)
lapply(data, names)
purrr::map(data, ~table(names(.x) %in% names(data[[1]])))
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

# Remove all rows for referrals for breast symptoms where cancer is not initially suspected
data <- subset(data, !(suspected_cancer_or_breast_symptomatic %in% c("Exhibited (non-cancer) breast symptoms - cancer not initially suspected")))

# Export processed file for quality checking purposes
# write.csv(data, file = paste0(workingDirPath, "28day_clean_test1.csv"))

# # # Calculation start # # #

# Calculate proportion of all referrals that are for most commonly referred cancer types # # #

for (group_name in names(cancer_groups)) {
  # Filter and calculate the total sum for the current group
  group_sum <- data %>%
    filter(suspected_cancer_or_breast_symptomatic %in% cancer_groups[[group_name]]) %>%
    summarise(total = sum(told_diagnosis_outcome_total, na.rm = TRUE)) %>%
    pull(total)

  # Add a row to summary_stats with the current group's total
  summary_stats <- summary_stats %>%
    add_row(
      label = paste0("calculation_Total referrals for ", group_name, " cancers: ", paste(cancer_groups[[group_name]], collapse = ", ")),
      value = group_sum
    )
  print("here")
  print(group_name)
  print(paste(cancer_groups[[group_name]], collapse = ", "))
}

# write.csv(summary_stats, paste0(workingDirPath, "summaryStats.csv"), row.names = FALSE)

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


# # # Staff and Absences Covariate Code Start # # #

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

# Format date columns to be in same format as main dataset
absencesDfs <- lapply(seq_along(absencesDfs), function(i) {
  absences <- absencesDfs[[i]]

  formattedDateCols <- sapply(colnames(absences), function(x) {
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
  colnames(absences) <- formattedDateCols
  absences <- absences %>%
    select(-"NHS England Region")

  # Export with unique filename using index
  # write.csv(absences, file = paste0(workingDirPath, "absences1_", i, ".csv"))

  absences <- absences %>%
    rename(Provider.Code = Code)
  # write.csv(absences, file = paste0(workingDirPath, "absences1a_", i, ".csv"))

  absences <- absences[!is.na(absences$Provider.Code), ]
  # write.csv(absences, file = paste0(workingDirPath, "absences1b_", i, ".csv"))

  # Transform dataframe
  absences <- absences %>%
    pivot_longer(
      cols = -c(Provider.Code, Name),
      names_to = "date",
      values_to = "totalAbsent"
    )

  write.csv(absences, file = paste0(workingDirPath, "absences1c.csv"))

  return(absences)
})

# make sure the absence dataset column names all line up
lapply(absencesDfs, names)

# appending all absence datasets into single overall dataset
absences = Reduce(rbind, absencesDfs)

# Export processed file for quality checking purposes
# write.csv(absences, file = paste0(workingDirPath, "absences2.csv"))

# Reformat monthly date column
absences$date <- as.Date(absences$date, format = "%Y_%m_%d")

# Group daily dataset into month-level dataset, averaging daily numbers of absent staff to get average monthly absences
absences <- absences %>%
  group_by(Provider.Code, date = floor_date(date, "month")) %>%
  summarise(
    Name = first(Name),
    totalAbsent = mean(totalAbsent, na.rm = TRUE),
    .groups = 'drop') %>%
  mutate(date = format(date, "%Y_%m_%d"))

# Export processed file for quality checking purposes
# write.csv(absences, file = paste0(workingDirPath, "absences3.csv"))

# Link absences provider-level data to Cancer Alliances
# Do not throw error for mismatched post-merge columns; Sean checked these by hand 28 July 2024 and all are fine
absences <- LinkToCA(absences, "absences", check = FALSE)

# Export processed file for quality checking purposes
# write.csv(absences, file = paste0(workingDirPath, "absences4.csv"))

# Merge dataframe containing total absent staff numbers by provider and month into main dataframe
data <- merge(data, absences, by = c('Provider.Code', 'date'), all.x = TRUE)

# Remove duplicate .x and .y post-merge columns
# Do not throw error for mismatched post-merge columns; Sean checked these by hand 28 July 2024 and all are fine
data <- CombineSuffixColumns(data, check = FALSE)

# Export processed file for quality checking purposes
# write.csv(data, file = paste0(workingDirPath, "28day_clean_test2b.csv"))

# Convert string-format numbers into numeric data
data <- data %>%
  mutate(totalAbsent = as.numeric(gsub(",", "", totalAbsent)))

data <- data %>%
  mutate(totalStaff = as.numeric(gsub(",", "", totalStaff)))

# Exclude total staff numbers for providers for which we don't have absence numbers, and vice-versa
# This only excludes a very small number of staff and absences (<1%) from a couple providers
data <- data %>%
  mutate(totalStaff = if_else(is.na(totalAbsent), NA_real_, totalStaff)) %>%
  mutate(totalAbsent = if_else(is.na(totalStaff), NA_real_, totalAbsent))

# # # Staff and Absences Code End # # #

# calculating number of patients facing diagnostic delay
data$num_not_told_diagnosis_outcome_within_28_days = (data$told_diagnosis_outcome_total - data$told_diagnosis_outcome_within_28_days)

# convert dates in string format to date format
data$date <- gsub("_", "-", data$date)
data$date <- as.Date(data$date)

# calculate month number, starting with the first month in 28 day FDS dataset, which is Apr 2021
data$monthNum = as.numeric((year(data$date)-2021)*12 +(month(data$date)) - 3)

baseData <- data

# After all data has been processed and linked, set up function for separate main and sensitivity analyses
AggregateData <- function(data, periodLength, sensitivity_analysis, depVar) {
  print("MADE IT")
  # Create suffix with analysis run parameters to append at end of result filenames
  suffix <- paste0("_", periodLength, "_", sensitivity_analysis, depVar)

  # Set conversion factor for result, no scaling at all for referral numbers, scale by 100 (%) for delay rate
  if (depVar %in% c("totalRefVol", "estWaitTime")) {
    conversionFactor <- 1
  }
  if (depVar %in% c("delayRate", "totalRefVolRelative")) {
    conversionFactor <- 100
  }

  overall_results <- data.frame()

  unadjusted_rates_treated <- data.frame()
  unadjusted_rates_untreated <- data.frame()

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

  # Export processed file for quality checking purposes
  # write.csv(data, file = paste0(workingDirPath, "28day_clean_test_", suffix, ".csv"))

  # # # Generate summary stats for Table 1 # # #
  # Use data aggregated up to 6 month periods
  if (periodLength == "6m" && sensitivity_analysis == "") {
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
      add_row(label = paste0("Table 1, diagnostic delay rate, participating in trial", suffix),
              value = sum(participating$told_diagnosis_outcome_after_28_days) / sum(participating$told_diagnosis_outcome_total))

    summary_stats <- summary_stats %>%
      add_row(label = paste0("Table 1, diagnostic delay rate, not participating in trial", suffix),
              value = sum(notParticipating$told_diagnosis_outcome_after_28_days) / sum(notParticipating$told_diagnosis_outcome_total))

    summary_stats <- summary_stats %>%
      add_row(label = paste0("for calculation_Table 1, total referrals, participating in trial", suffix),
              value = sum(participating$told_diagnosis_outcome_total))

    summary_stats <- summary_stats %>%
      add_row(label = paste0("for calculation_Table 1, total referrals, not participating in trial", suffix),
              value = sum(notParticipating$told_diagnosis_outcome_total))
  }

  startingData <- data

  for (cancers_included in c(cancer_types_to_loop_through)) {
    suffix2 <- paste0("_", periodLength, "_", sensitivity_analysis, depVar, "_", gsub("/", "", cancers_included))

    print(cancers_included)

    data <- startingData

    if (cancers_included %in% names(cancer_groups)) {
      data <- subset(data, (suspected_cancer_or_breast_symptomatic %in% cancer_groups[[cancers_included]]))
    }
    else {
      data <- subset(data, (suspected_cancer_or_breast_symptomatic == cancers_included))
    }

    # Export processed file for quality checking purposes
    # write.csv(data, file = paste0(workingDirPath, "28day_clean_test6", suffix2, ".csv"))

    # PLACEHOLDER FROM OLD CODE FILE: For monthly analyses and primary cancer groups, produce trend figures using unadjusted values

    # First group rows across cancer sites at the Provider.Code and month level
    # Done in this sequence in order to only count staff and absence numbers once per provider-month (and not multiple times for each cancer site)
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
        date = first(date)
      )

    # Export processed file for quality checking purposes
    # write.csv(dataGrouped, file = paste0(workingDirPath, "28day_clean_grouped0", suffix2, ".csv"))

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
        date = first(date)
      )

    # Export processed file for quality checking purposes
    # write.csv(dataGrouped, file = paste0(workingDirPath, "28day_clean_groupedA", suffix2, ".csv"))

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
        date = first(date)
      )

    # # # Generate more summary stats for Table 1 # # #
    # Use data aggregated up to 6 month periods, for all cancer referrals
    if (periodLength == "6m" && cancers_included == "allCancers" && sensitivity_analysis == "") {
      # Just use data from the first 6m (pre-trial) period in the dataset
      preTrial <- subset(dataGrouped, (periodNum == 1))

      # Separate data into participating and non-participating cancer alliances
      participating <- subset(preTrial, (mced_treated == 1))
      notParticipating <- subset(preTrial, (mced_treated == 0))

      summary_stats <- summary_stats %>%
        add_row(label = paste0("Table 1, number of staff, participating in trial", suffix),
                value = sum(participating$totalStaff))

      summary_stats <- summary_stats %>%
        add_row(label = paste0("Table 1, number of staff, not participating in trial", suffix),
                value = sum(notParticipating$totalStaff))
    }

    # calculating percentage of patients facing diagnostic delay
    dataGrouped$percentage_not_told_diagnosis_outcome_within_28_days = (dataGrouped$num_not_told_diagnosis_outcome_within_28_days / dataGrouped$told_diagnosis_outcome_total)

    if (depVar %in% c('totalRefVol', 'totalRefVolRelative')) {
      file_path <- paste0(dirPath, "Cancer+Prevalence+Statistics+England+2020_download_popByCA.csv")
      pop_df <- read.csv(file_path, skip = 1)

      # write.csv(dataGrouped, file = paste0(workingDirPath, "28day_clean_groupedC", suffix2, ".csv"))

      dataGrouped <- merge(dataGrouped, pop_df[, c("Cancer.Alliance", "population")],
                   by = "Cancer.Alliance", all.x = TRUE)

      dataGrouped$ref_rate <- (dataGrouped$told_diagnosis_outcome_total / dataGrouped$population) * 100000

      # write.csv(dataGrouped, file = paste0(workingDirPath, "28day_clean_groupedD", suffix2, ".csv"))

      print(nrow(dataGrouped))

      if (depVar == 'totalRefVolRelative') {
        dataGrouped <- dataGrouped %>%
          group_by(Cancer.Alliance) %>%
          mutate(ref_rate_base = ifelse(any(periodNum == 1), ref_rate[periodNum == 1], NA)) %>%
          ungroup()

        if (any(is.na(dataGrouped$ref_rate_base))) {next}

        dataGrouped$ref_rate_relative = dataGrouped$ref_rate / dataGrouped$ref_rate_base
      }
      print(nrow(dataGrouped))
    }

    # estimate average wait times from counts within the 5 wait times categories: 0-14; 15-28; 28-42; 43-62; 62+ days
    # each observation in each category is assumed to have a wait time exactly in the middle of that category
    # (e.g. all referrals in the 0-14 days category are assumed to have a wait time of (0+14)/2 = 7 days
    dataGrouped$est_wait_time <-
      (dataGrouped$told_diagnosis_outcome_within_14_days * 7) +
      (dataGrouped$in_15_to_28_days * 21.5) +
      (dataGrouped$told_diagnosis_outcome_in_29_to_42_days * 35.5) +
      (dataGrouped$told_diagnosis_outcome_in_43_to_62_days * 52.5) +
      (dataGrouped$told_diagnosis_outcome_after_62_days * 81.5) # Upper bound of category set to 100; (63+100)/2 = 81.5 days
    dataGrouped$est_wait_time <- (dataGrouped$est_wait_time / dataGrouped$told_diagnosis_outcome_total)

    # Export processed file for quality checking purposes
    # write.csv(dataGrouped, file = paste0(workingDirPath, "28day_clean_groupedB", suffix2, ".csv"))

    # calculating percentage of staff absent
    dataGrouped$percentageAbsent = (dataGrouped$totalAbsent / dataGrouped$totalStaff)

    # create a dummy variable for each time period, set to 1 when a row is in that time period and is for a cancer alliance participating in the trial
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

    # Export processed file for quality checking purposes
    write.csv(dataGrouped, file = paste0(outDirPath, "28day_clean_test_grouped", suffix2, ".csv"))

    # PLACEHOLDER FROM OLD CODE FILE: Analysis # Formula uses % patients facing diagnostic delay as outcome, percentageAbsent as covariate,
  }
}

dep_var = "delayRate"
# dep_var = "estWaitTime"
# dep_var = "totalRefVol"
# dep_var = "totalRefVolRelative"

print("about to start aggregating data")

AggregateData(baseData, periodLength = "1m", sensitivity_analysis = "", depVar = dep_var)
# AggregateData(baseData, periodLength = "6m", sensitivity_analysis = "", depVar = dep_var)

# AggregateData(baseData, periodLength = "1m", sensitivity_analysis = "washout", depVar = dep_var)
# AggregateData(baseData, periodLength = "6m", sensitivity_analysis = "washout", depVar = dep_var)
#
# AggregateData(baseData, periodLength = "1m", sensitivity_analysis = "no covariates", depVar = dep_var)
# AggregateData(baseData, periodLength = "6m", sensitivity_analysis = "no covariates", depVar = dep_var)

write.csv(summary_stats, paste0(outDirPath, "summaryStats.csv"), row.names = FALSE)
