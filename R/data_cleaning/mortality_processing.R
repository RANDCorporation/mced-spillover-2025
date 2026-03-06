# title: "Process Cancer Mortality Data by Cancer Alliance"
# author: "Sean Mann"
# date: "4 March 2026"

# Description
# This script computes population-weighted age-standardised cancer mortality rates
# (excluding non-melanoma skin cancer) at the cancer alliance level. It reads
# Sub-ICB level data from the NDRS cancer registration statistics file and maps
# Sub-ICBs to cancer alliances using an existing mapping file.

library(here)
library(tidyverse)
library(readxl)

# # # Paths # # #

dirPath <- paste0(here("data/"), "/")
linkingDirPath <- paste0(dirPath, "linking_files/")
mortalityDirPath <- paste0(linkingDirPath, "Cancer Mortality/")
outDirPath <- paste0(dirPath, "cleaned_data/")

# # # Read input files # # #

# 1. Cancer mortality data (Table_4: Sub-ICB level)
# This file was downloaded on March 4, 2026 from https://digital.nhs.uk/data-and-information/publications/statistical/cancer-registration-statistics/england-2020
mortality_raw <- read_excel(paste0(mortalityDirPath, "Cancer_Mortality_final.xls"),
  sheet = "Table_4",
  skip = 2)

# 2. Sub-ICB to Cancer Alliance mapping
sub_icb_to_ca <- read.csv(paste0(linkingDirPath, "Sub-ICB-to-Alliance-mapping-2022_v1.4.csv"))

# # # Filter to all cancers excl NMSC, All ages # # #

mortality_filtered <- mortality_raw %>%
  filter(
    `Site Description` == "All malignant cancers excluding non-melanoma skin cancer (NMSC)",
    Age == "All ages"
  )

# # # Extract age-standardised rates and derive populations # # #
# The data has Male and Female rows separately, with no combined "Persons" row.
# For each Sub-ICB, we derive gender-specific populations from the non-standardised
# (crude) rate, then compute a population-weighted combined ASR.

# Get age-standardised rates
asr_data <- mortality_filtered %>%
  filter(`Type of rate` == "Age-standardised") %>%
  select(
    sub_icb_code = `Sub ICB Code`,
    gender = Gender,
    cases = Cases,
    asr = `Rate (per 100,000 population)`
  ) %>%
  mutate(asr = as.numeric(asr))

# Get non-standardised (crude) rates to derive population
crude_data <- mortality_filtered %>%
  filter(`Type of rate` == "Non-standardised") %>%
  select(
    sub_icb_code = `Sub ICB Code`,
    gender = Gender,
    cases = Cases,
    crude_rate = `Rate (per 100,000 population)`
  ) %>%
  mutate(crude_rate = as.numeric(crude_rate))

# Join ASR and crude rate data
sub_icb_data <- asr_data %>%
  inner_join(crude_data %>% select(sub_icb_code, gender, crude_rate),
             by = c("sub_icb_code", "gender")) %>%
  mutate(
    # Derive population from count and crude rate: pop = cases / (crude_rate / 100000)
    population = cases / (crude_rate / 100000)
  )

# Compute combined ASR per Sub-ICB (population-weighted average of male and female ASRs)
sub_icb_combined <- sub_icb_data %>%
  group_by(sub_icb_code) %>%
  summarise(
    total_deaths = sum(cases),
    total_population = sum(population),
    combined_asr = sum(asr * population) / sum(population),
    .groups = "drop"
  )

cat("Sub-ICBs with combined ASR:", nrow(sub_icb_combined), "\n")

# # # Map Sub-ICBs to Cancer Alliances # # #

sub_icb_combined <- sub_icb_combined %>%
  inner_join(
    sub_icb_to_ca %>% select(sub_icb_code = Sub.ICB.E.Code, Cancer.Alliance = Cancer.Alliance),
    by = "sub_icb_code"
  )

# Verify all Sub-ICBs mapped
stopifnot(all(!is.na(sub_icb_combined$Cancer.Alliance)))

# # # Aggregate to Cancer Alliance level # # #

ca_mortality <- sub_icb_combined %>%
  group_by(Cancer.Alliance) %>%
  summarise(
    mortality_deaths = sum(total_deaths),
    mortality_population = sum(total_population),
    mortality_asr = sum(combined_asr * total_population) / sum(total_population),
    .groups = "drop"
  )

# # # Verification # # #

cat("Number of cancer alliances:", nrow(ca_mortality), "\n")
cat("ASR range:", round(min(ca_mortality$mortality_asr), 1), "to",
    round(max(ca_mortality$mortality_asr), 1), "per 100,000\n")

# Verify all 21 CAs from McedToCAMappingByHand.csv are present
mced <- read.csv(paste0(linkingDirPath, "McedToCAMappingByHand.csv"))
missing_cas <- setdiff(mced$Cancer.Alliance, ca_mortality$Cancer.Alliance)
if (length(missing_cas) > 0) {
  stop("Missing cancer alliances: ", paste(missing_cas, collapse = ", "))
}
cat("All 21 cancer alliances matched successfully.\n")

# Sanity check: compare total deaths against England total from Table_1
england_t1 <- read_excel(paste0(mortalityDirPath, "Cancer_Mortality_final.xls"),
  sheet = "Table_1",
  skip = 2)
england_deaths <- england_t1 %>%
  filter(
    `Site description` == "All malignant cancers excluding non-melanoma skin cancer (NMSC)",
    `Geography type` == "Country",
    `Age Group` == "All ages",
    `Type of rate` == "Non-standardised"
  ) %>%
  summarise(total = sum(Count))
cat("England total deaths (Table_1):", england_deaths$total, "\n")
cat("Our CA-level total deaths:", sum(ca_mortality$mortality_deaths), "\n")
cat("Difference:", sum(ca_mortality$mortality_deaths) - england_deaths$total, "\n")

# Verify ASRs are plausible
stopifnot(all(ca_mortality$mortality_asr > 100 & ca_mortality$mortality_asr < 500))

# # # Save output # # #

write.csv(ca_mortality, paste0(outDirPath, "mortality_by_cancer_alliance.csv"), row.names = FALSE)
cat("Saved:", paste0(outDirPath, "mortality_by_cancer_alliance.csv"), "\n")
