# title: "Process Stage at Diagnosis Data by Cancer Alliance"
# author: "Sean Mann"
# date: "4 March 2026"

# Description
# This script computes the proportion of cancers with known stage diagnosed at
# stage III or IV in 2020 at the cancer alliance level. It reads Cancer Alliance-level
# data from the NDRS stage metrics file, except for East of England which is split
# into North and South sub-regions using ICB-level data from the same file.

library(here)
library(tidyverse)

# # # Paths # # #

dirPath <- paste0(here("data/"), "/")
linkingDirPath <- paste0(dirPath, "linking_files/")
stageDirPath <- paste0(linkingDirPath, "Cancer Stage files/")
outDirPath <- paste0(dirPath, "cleaned_data/")

# # # Read input file # # #

# This file was downloaded on March 4, 2026 from https://digital.nhs.uk/ndrs/data/data-outputs/cancer-data-hub/cancer-stage-at-diagnosis
stage_raw <- read.csv(paste0(stageDirPath,
  "NDRS Stage Metrics- Stage at diagnosis by cancer site in England, 2013-2022.csv"))

# Filter to 2020 only
stage_2020 <- stage_raw %>%
  filter(Year == 2020)

# # # Process Cancer Alliance rows (excluding East of England) # # #

ca_data <- stage_2020 %>%
  filter(Geography.Type == "Cancer Alliance",
         Geography.Name != "East of England") %>%
  group_by(Geography.Name) %>%
  summarise(
    stage_1 = sum(Stage.1),
    stage_2 = sum(Stage.2),
    stage_3 = sum(Stage.3),
    stage_4 = sum(Stage.4),
    .groups = "drop"
  ) %>%
  mutate(
    total_known_stage = stage_1 + stage_2 + stage_3 + stage_4,
    pct_stage_3_4 = (stage_3 + stage_4) / total_known_stage
  )

# Map NDRS cancer alliance names to project cancer alliance names
# Most map by appending " Cancer Alliance", but some require special handling
ca_name_map <- c(
  "Cheshire and Merseyside" = "Cheshire and Merseyside Cancer Alliance",
  "East Midlands" = "East Midlands Cancer Alliance",
  "Greater Manchester" = "Greater Manchester Cancer Alliance",
  "Humber and North Yorkshire" = "Humber and North Yorkshire Cancer Alliance",
  "Kent and Medway" = "Kent and Medway Cancer Alliance",
  "Lancashire and South Cumbria" = "Lancashire & South Cumbria Cancer Alliance",
  "North Central London" = "North Central London Cancer Alliance",
  "North East London" = "North East London Cancer Alliance",
  "Northern" = "Northern Cancer Alliance",
  "Peninsula" = "Peninsula Cancer Alliance",
  "Somerset, Wiltshire, Avon and Gloucestershire" = "Somerset, Wiltshire, Avon and Gloucestershire Cancer Alliance",
  "South East London" = "South East London Cancer Alliance",
  "South Yorkshire and Bassetlaw" = "South Yorkshire Cancer Alliance",
  "Surrey and Sussex" = "Surrey and Sussex Cancer Alliance",
  "Thames Valley" = "Thames Valley Cancer Alliance",
  "Wessex" = "Wessex Cancer Alliance",
  "West London" = "West London Cancer Alliance",
  "West Midlands" = "West Midlands Cancer Alliance",
  "West Yorkshire and Harrogate" = "West Yorkshire and Harrogate Cancer Alliance"
)

ca_data$Cancer.Alliance <- ca_name_map[ca_data$Geography.Name]

# Verify all 19 CAs mapped successfully
stopifnot(all(!is.na(ca_data$Cancer.Alliance)))

# # # Process East of England via ICB-level data # # #
# The NDRS file has "East of England" as a single Cancer Alliance, but the project
# uses a North/South split. We reconstruct this using ICB-level data.

eoe_north_icbs <- c(
  "NHS Cambridgeshire and Peterborough Integrated Care Board",
  "NHS Norfolk and Waveney Integrated Care Board",
  "NHS Suffolk and North East Essex Integrated Care Board"
)

eoe_south_icbs <- c(
  "NHS Bedfordshire, Luton and Milton Keynes Integrated Care Board",
  "NHS Hertfordshire and West Essex Integrated Care Board",
  "NHS Mid and South Essex Integrated Care Board"
)

icb_data <- stage_2020 %>%
  filter(Geography.Type == "Integrated Care Board",
         Geography.Name %in% c(eoe_north_icbs, eoe_south_icbs)) %>%
  mutate(
    eoe_region = case_when(
      Geography.Name %in% eoe_north_icbs ~ "East of England - North Cancer Alliance",
      Geography.Name %in% eoe_south_icbs ~ "East of England - South Cancer Alliance"
    )
  ) %>%
  group_by(eoe_region) %>%
  summarise(
    stage_1 = sum(Stage.1),
    stage_2 = sum(Stage.2),
    stage_3 = sum(Stage.3),
    stage_4 = sum(Stage.4),
    .groups = "drop"
  ) %>%
  mutate(
    total_known_stage = stage_1 + stage_2 + stage_3 + stage_4,
    pct_stage_3_4 = (stage_3 + stage_4) / total_known_stage
  ) %>%
  rename(Cancer.Alliance = eoe_region)

# Verify both EoE sub-regions are present
stopifnot(nrow(icb_data) == 2)

# # # Combine all 21 cancer alliances # # #

ca_stage <- bind_rows(
  ca_data %>% select(Cancer.Alliance, stage_1, stage_2, stage_3, stage_4, total_known_stage, pct_stage_3_4),
  icb_data
)

# # # Verification # # #

cat("Number of cancer alliances:", nrow(ca_stage), "\n")
cat("Pct stage III/IV range:",
    round(min(ca_stage$pct_stage_3_4) * 100, 1), "% to",
    round(max(ca_stage$pct_stage_3_4) * 100, 1), "%\n")

# Verify pct_stage_3_4 is between 0 and 1 for all CAs
stopifnot(all(ca_stage$pct_stage_3_4 >= 0 & ca_stage$pct_stage_3_4 <= 1))

# Verify all 21 CAs from McedToCAMappingByHand.csv are present
mced <- read.csv(paste0(linkingDirPath, "McedToCAMappingByHand.csv"))
missing_cas <- setdiff(mced$Cancer.Alliance, ca_stage$Cancer.Alliance)
if (length(missing_cas) > 0) {
  stop("Missing cancer alliances: ", paste(missing_cas, collapse = ", "))
}
cat("All 21 cancer alliances matched successfully.\n")

# Sanity check: compare sum of our CA-level counts against England national total
england_total <- stage_2020 %>%
  filter(Geography.Type == "Country", Geography.Name == "England") %>%
  summarise(
    stage_1 = sum(Stage.1),
    stage_2 = sum(Stage.2),
    stage_3 = sum(Stage.3),
    stage_4 = sum(Stage.4)
  )

our_total <- ca_stage %>%
  summarise(
    stage_1 = sum(stage_1),
    stage_2 = sum(stage_2),
    stage_3 = sum(stage_3),
    stage_4 = sum(stage_4)
  )

cat("England national total known stage:", sum(england_total), "\n")
cat("Our CA-level total known stage:", sum(our_total), "\n")
cat("Difference:", sum(our_total) - sum(england_total), "\n")

# # # Save output # # #

write.csv(ca_stage, paste0(outDirPath, "stage_by_cancer_alliance.csv"), row.names = FALSE)
cat("Saved:", paste0(outDirPath, "stage_by_cancer_alliance.csv"), "\n")
