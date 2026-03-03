# title: "Process IMD Data by Cancer Alliance"
# author: "Sean Mann"
# date: "24 February 2026"

# Description
# This script computes population-weighted IMD (Index of Multiple Deprivation) measures
# at the cancer alliance level. It reads LSOA-level IMD scores, maps LSOAs to cancer
# alliances, and aggregates using mid-2020 population weights.

library(here)
library(tidyverse)
library(readxl)

# # # Paths # # #

dirPath <- paste0(here("data/"), "/")
linkingDirPath <- paste0(dirPath, "linking_files/")
imdDirPath <- paste0(linkingDirPath, "IMD linking files/")
outDirPath <- paste0(dirPath, "cleaned_data/")

# # # Read input files # # #

# 1. IMD 2019 scores, ranks, and deciles per LSOA
# This file was downloaded on February 23, 2026 from https://assets.publishing.service.gov.uk/media/5dc407b440f0b6379a7acc8d/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv
imd <- read.csv(paste0(imdDirPath,
  "File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv"))

imd <- imd %>%
  select(
    lsoa_code = LSOA.code..2011.,
    imd_score = Index.of.Multiple.Deprivation..IMD..Score,
    imd_decile = Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.
  )

# 2. LSOA 2011 to Cancer Alliance geographic lookup
# This file was downloaded on March 2, 2026 from https://stg-arcgisazurecdataprod1.az.arcgis.com/exportfiles-1559-9398/LSOA11_CCG21_STP21_CAL21_LAD21_EN_LU_f3b57157421d4eaa81e77d3ecc4b5bda_2315360445991796596.xlsx?sv=2025-05-05&st=2026-03-02T20%3A06%3A52Z&se=2026-03-02T21%3A11%3A52Z&sr=c&sp=r&sig=7B9NMPR318ELfPaFd%2Bn0CiSLjnQ2jY4%2F%2BnAGrlsqTuA%3D
# Download link was accessed from https://www.data.gov.uk/dataset/2c416d06-5929-46b6-a83e-b2467381e095/lsoa-2011-to-clinical-commissioning-group-to-stp-to-cal-april-2021-lookup-in-en
lsoa_to_ca <- read_excel(paste0(imdDirPath,
  "LSOA11_CCG21_STP21_CAL21_LAD21_EN_LU_f3b57157421d4eaa81e77d3ecc4b5bda_2315360445991796596.xlsx"))

lsoa_to_ca <- lsoa_to_ca %>%
  select(
    lsoa_code = LSOA11CD,
    cal21_name = CAL21NM
  )

# 3. Mid-2020 population per LSOA
# This file was downloaded on February 23, 2026 from https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimatesnationalstatistics/mid2020sape23dt13/sape23dt13mid2020lsoabroadagesestimatesunformatted.xlsx
# Note that population numbers for each group of regions calculated using this data file are exactly the same as numbers we present in Table 1 which were derived from Cancer Prevalence dataset
pop <- read_excel(paste0(imdDirPath,
  "sape23dt13mid2020lsoabroadagesestimatesunformatted.xlsx"),
  sheet = "Mid-2020 Persons",
  skip = 3)

pop <- pop %>%
  select(
    lsoa_code = `LSOA Code`,
    population = `All Ages`
  )

# Filter to England-only LSOAs (codes starting with "E")
pop <- pop %>%
  filter(grepl("^E", lsoa_code))

# 4. Mid-2020 single-year-of-age population per LSOA (for 50-77 age weighting)
# This file was downloaded on February 24, 2026 from https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2020sape23dt2/sape23dt2mid2020lsoasyoaestimatesunformatted.xlsx
pop_sya <- read_excel(paste0(imdDirPath,
  "sape23dt2mid2020lsoasyoaestimatesunformatted.xlsx"),
  sheet = "Mid-2020 Persons",
  skip = 3)

pop_sya <- pop_sya %>%
  mutate(pop_50_77 = rowSums(across(`50`:`77`))) %>%
  select(lsoa_code = `LSOA Code`, pop_50_77) %>%
  filter(grepl("^E", lsoa_code))

# # # Join datasets # # #

lsoa_df <- imd %>%
  inner_join(lsoa_to_ca, by = "lsoa_code") %>%
  inner_join(pop, by = "lsoa_code") %>%
  inner_join(pop_sya, by = "lsoa_code")

cat("Joined LSOA rows:", nrow(lsoa_df), "\n")

# # # Compute national IMD quintiles from deciles # # #
# Decile convention: 1 = most deprived 10% of LSOAs
# Quintile convention: 1 = most deprived 20% (deciles 1-2), 5 = least deprived (deciles 9-10)

lsoa_df <- lsoa_df %>%
  mutate(imd_quintile = ceiling(imd_decile / 2))

# # # Map cancer alliance names # # #
# CAL21 lookup uses short names; the main dataset uses full names with " Cancer Alliance" suffix.
# Most map by appending the suffix, but 4 require special handling.

ca_name_map <- c(
  "Cheshire and Merseyside" = "Cheshire and Merseyside Cancer Alliance",
  "East Midlands" = "East Midlands Cancer Alliance",
  "East of England - North" = "East of England - North Cancer Alliance",
  "East of England - South" = "East of England - South Cancer Alliance",
  "Greater Manchester" = "Greater Manchester Cancer Alliance",
  "Humber, Coast and Vale" = "Humber and North Yorkshire Cancer Alliance",
  "Kent and Medway" = "Kent and Medway Cancer Alliance",
  "Lancashire and South Cumbria" = "Lancashire & South Cumbria Cancer Alliance",
  "North Central London" = "North Central London Cancer Alliance",
  "North East London" = "North East London Cancer Alliance",
  "North West and South West London" = "West London Cancer Alliance",
  "Northern" = "Northern Cancer Alliance",
  "Peninsula" = "Peninsula Cancer Alliance",
  "Somerset, Wiltshire, Avon and Gloucestershire" = "Somerset, Wiltshire, Avon and Gloucestershire Cancer Alliance",
  "South East London" = "South East London Cancer Alliance",
  "South Yorkshire and Bassetlaw" = "South Yorkshire Cancer Alliance",
  "Surrey and Sussex" = "Surrey and Sussex Cancer Alliance",
  "Thames Valley" = "Thames Valley Cancer Alliance",
  "Wessex" = "Wessex Cancer Alliance",
  "West Midlands" = "West Midlands Cancer Alliance",
  "West Yorkshire and Harrogate" = "West Yorkshire and Harrogate Cancer Alliance"
)

lsoa_df$Cancer.Alliance <- ca_name_map[lsoa_df$cal21_name]

# Verify all LSOAs mapped successfully
stopifnot(all(!is.na(lsoa_df$Cancer.Alliance)))

# # # Aggregate to cancer alliance level # # #

# Population-weighted average IMD score per cancer alliance
ca_imd_avg <- lsoa_df %>%
  group_by(Cancer.Alliance) %>%
  summarise(
    imd_pop_weighted_avg = sum(imd_score * population) / sum(population),
    imd_population = sum(population),
    .groups = "drop"
  )

# % of population in each IMD quintile per cancer alliance
ca_quintile_pct <- lsoa_df %>%
  group_by(Cancer.Alliance, imd_quintile) %>%
  summarise(
    quintile_pop = sum(population),
    .groups = "drop"
  ) %>%
  left_join(
    lsoa_df %>%
      group_by(Cancer.Alliance) %>%
      summarise(total_pop = sum(population), .groups = "drop"),
    by = "Cancer.Alliance"
  ) %>%
  mutate(quintile_pct = quintile_pop / total_pop) %>%
  select(Cancer.Alliance, imd_quintile, quintile_pct) %>%
  pivot_wider(
    names_from = imd_quintile,
    values_from = quintile_pct,
    names_prefix = "imd_q",
    names_sort = TRUE,
    values_fill = 0
  ) %>%
  rename(
    imd_q1_pct = imd_q1,
    imd_q2_pct = imd_q2,
    imd_q3_pct = imd_q3,
    imd_q4_pct = imd_q4,
    imd_q5_pct = imd_q5
  )

# # # Aggregate 50-77 age-weighted measures # # #
# This is done to provide a check of the IMD data against an independent source
# Independent source: https://journals.sagepub.com/doi/10.1177/17407745241302477
#    IMD quintile % for ages 50-77 for participating vs non-participating cancer alliance regions are given in the two right-most columns of Table 1
# When compared against the output from our IMD data collection, linking, and aggregation up to the group of participating vs. non-participating regions
# Our figures are very close (difference between 0.0 and 0.2 percentage points for all quintiles) to the figures given in that independent source
# The small discrepancy could be explained by use of different area population data; we use official 2020 population figures; they do not specify which year or population dataset they use.

# Population-weighted average IMD score per cancer alliance (50-77)
ca_imd_avg_5077 <- lsoa_df %>%
  group_by(Cancer.Alliance) %>%
  summarise(
    imd_pop_weighted_avg_50_77 = sum(imd_score * pop_50_77) / sum(pop_50_77),
    imd_population_50_77 = sum(pop_50_77),
    .groups = "drop"
  )

# % of 50-77 population in each IMD quintile per cancer alliance
ca_quintile_pct_5077 <- lsoa_df %>%
  group_by(Cancer.Alliance, imd_quintile) %>%
  summarise(
    quintile_pop = sum(pop_50_77),
    .groups = "drop"
  ) %>%
  left_join(
    lsoa_df %>%
      group_by(Cancer.Alliance) %>%
      summarise(total_pop = sum(pop_50_77), .groups = "drop"),
    by = "Cancer.Alliance"
  ) %>%
  mutate(quintile_pct = quintile_pop / total_pop) %>%
  select(Cancer.Alliance, imd_quintile, quintile_pct) %>%
  pivot_wider(
    names_from = imd_quintile,
    values_from = quintile_pct,
    names_prefix = "imd_q",
    names_sort = TRUE,
    values_fill = 0
  ) %>%
  rename(
    imd_q1_pct_50_77 = imd_q1,
    imd_q2_pct_50_77 = imd_q2,
    imd_q3_pct_50_77 = imd_q3,
    imd_q4_pct_50_77 = imd_q4,
    imd_q5_pct_50_77 = imd_q5
  )

# Combine all-ages and 50-77 measures
ca_imd <- ca_imd_avg %>%
  left_join(ca_quintile_pct, by = "Cancer.Alliance") %>%
  left_join(ca_imd_avg_5077, by = "Cancer.Alliance") %>%
  left_join(ca_quintile_pct_5077, by = "Cancer.Alliance")

# # # Verification # # #

cat("Number of cancer alliances:", nrow(ca_imd), "\n")
cat("Pop-weighted avg IMD scores range (all ages):",
    round(min(ca_imd$imd_pop_weighted_avg), 2), "to",
    round(max(ca_imd$imd_pop_weighted_avg), 2), "\n")
cat("Pop-weighted avg IMD scores range (50-77):",
    round(min(ca_imd$imd_pop_weighted_avg_50_77), 2), "to",
    round(max(ca_imd$imd_pop_weighted_avg_50_77), 2), "\n")

# Check quintile percentages sum to ~1.0 for each CA (all ages)
quintile_sums <- ca_imd %>%
  mutate(q_sum = imd_q1_pct + imd_q2_pct + imd_q3_pct + imd_q4_pct + imd_q5_pct)
stopifnot(all(abs(quintile_sums$q_sum - 1.0) < 0.001))

# Check quintile percentages sum to ~1.0 for each CA (50-77)
quintile_sums_5077 <- ca_imd %>%
  mutate(q_sum = imd_q1_pct_50_77 + imd_q2_pct_50_77 + imd_q3_pct_50_77 +
           imd_q4_pct_50_77 + imd_q5_pct_50_77)
stopifnot(all(abs(quintile_sums_5077$q_sum - 1.0) < 0.001))

# Verify 50-77 population is smaller than all-ages population for every CA
stopifnot(all(ca_imd$imd_population_50_77 < ca_imd$imd_population))

# Verify all 21 CAs from McedToCAMappingByHand.csv are present
mced <- read.csv(paste0(linkingDirPath, "McedToCAMappingByHand.csv"))
missing_cas <- setdiff(mced$Cancer.Alliance, ca_imd$Cancer.Alliance)
if (length(missing_cas) > 0) {
  stop("Missing cancer alliances: ", paste(missing_cas, collapse = ", "))
}
cat("All 21 cancer alliances matched successfully.\n")

# # # Save output # # #

write.csv(ca_imd, paste0(outDirPath, "imd_by_cancer_alliance.csv"), row.names = FALSE)
cat("Saved:", paste0(outDirPath, "imd_by_cancer_alliance.csv"), "\n")
