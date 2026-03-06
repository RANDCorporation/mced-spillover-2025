# title: "Process Ethnicity Data by Cancer Alliance"
# author: "Sean Mann"
# date: "4 March 2026"

# Description
# This script computes ethnic composition (% White, Asian, Black, Mixed, Other)
# at the cancer alliance level using 2021 Census LSOA-level data. It maps LSOAs
# to cancer alliances using an LSOA 2021 geographic lookup. For the East of England
# Cancer Alliance (which the project splits into North and South), Sub-ICB codes
# from the lookup are used with the Sub-ICB-to-Alliance mapping.

library(here)
library(tidyverse)

# # # Paths # # #

dirPath <- paste0(here("data/"), "/")
linkingDirPath <- paste0(dirPath, "linking_files/")
ethnicityDirPath <- paste0(linkingDirPath, "Ethnicity/")
outDirPath <- paste0(dirPath, "cleaned_data/")

# # # Read input files # # #

# 1. Census 2021 ethnicity data at LSOA level
# This file was downloaded on March 4, 2026 from https://www.nomisweb.co.uk/output/census/2021/census2021-ts021.zip
ethnicity_raw <- read.csv(paste0(ethnicityDirPath, "census2021-ts021-lsoa.csv"))

# 2. LSOA 2021 to Cancer Alliance geographic lookup
# This file was downloaded on March 4, 2026 from https://geoportal.statistics.gov.uk/datasets/lsoa-2021-to-sicbl-to-icb-to-cancer-alliances-to-lad-april-2024-lookup-in-en/about
lsoa_to_ca <- read.csv(paste0(ethnicityDirPath,
  "LSOA_(2021)_to_SICBL_to_ICB_to_Cancer_Alliances_to_LAD_(April_2024)_Lookup_in_EN.csv"))

lsoa_to_ca <- lsoa_to_ca %>%
  select(
    lsoa_code = LSOA21CD,
    cal24_name = CAL24NM,
    sicbl_code = SICBL24CD
  )

# 3. Sub-ICB to Cancer Alliance mapping (for East of England North/South split)
sub_icb_to_ca <- read.csv(paste0(linkingDirPath, "Sub-ICB-to-Alliance-mapping-2022_v1.4.csv"))

# # # Process ethnicity data # # #

# Filter to England LSOAs only and select the 5 top-level ethnic group columns
ethnicity <- ethnicity_raw %>%
  filter(grepl("^E", geography.code)) %>%
  select(
    lsoa_code = geography.code,
    total = Ethnic.group..Total..All.usual.residents,
    white = Ethnic.group..White,
    asian = Ethnic.group..Asian..Asian.British.or.Asian.Welsh,
    black = Ethnic.group..Black..Black.British..Black.Welsh..Caribbean.or.African,
    mixed = Ethnic.group..Mixed.or.Multiple.ethnic.groups,
    other = Ethnic.group..Other.ethnic.group
  )

cat("England LSOAs in census data:", nrow(ethnicity), "\n")

# # # Map LSOAs to Cancer Alliances # # #

ethnicity_mapped <- ethnicity %>%
  inner_join(lsoa_to_ca, by = "lsoa_code")

cat("LSOAs matched to cancer alliances:", nrow(ethnicity_mapped), "of", nrow(ethnicity), "\n")

# Map cancer alliance names to project names
# Most map by appending " Cancer Alliance", with special cases noted below
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

# For non-East of England LSOAs, map directly using the CA name
ethnicity_non_eoe <- ethnicity_mapped %>%
  filter(cal24_name != "East of England") %>%
  mutate(Cancer.Alliance = ca_name_map[cal24_name])

# Verify all non-EoE LSOAs mapped successfully
stopifnot(all(!is.na(ethnicity_non_eoe$Cancer.Alliance)))

# For East of England LSOAs, use Sub-ICB codes to determine North vs South
eoe_sub_icb_map <- sub_icb_to_ca %>%
  filter(grepl("East of England", Cancer.Alliance)) %>%
  select(sicbl_code = Sub.ICB.E.Code, Cancer.Alliance)

ethnicity_eoe <- ethnicity_mapped %>%
  filter(cal24_name == "East of England") %>%
  inner_join(eoe_sub_icb_map, by = "sicbl_code")

# Verify all EoE LSOAs mapped
eoe_total <- sum(ethnicity_mapped$cal24_name == "East of England")
cat("East of England LSOAs mapped via Sub-ICB:", nrow(ethnicity_eoe), "of", eoe_total, "\n")
stopifnot(nrow(ethnicity_eoe) == eoe_total)

# Combine
ethnicity_with_ca <- bind_rows(
  ethnicity_non_eoe %>% select(lsoa_code, total, white, asian, black, mixed, other, Cancer.Alliance),
  ethnicity_eoe %>% select(lsoa_code, total, white, asian, black, mixed, other, Cancer.Alliance)
)

cat("Total LSOAs with CA assignment:", nrow(ethnicity_with_ca), "\n")

# # # Aggregate to Cancer Alliance level # # #

ca_ethnicity <- ethnicity_with_ca %>%
  group_by(Cancer.Alliance) %>%
  summarise(
    ethnicity_total = sum(total),
    ethnicity_white = sum(white),
    ethnicity_asian = sum(asian),
    ethnicity_black = sum(black),
    ethnicity_mixed = sum(mixed),
    ethnicity_other = sum(other),
    .groups = "drop"
  ) %>%
  mutate(
    pct_white = ethnicity_white / ethnicity_total * 100,
    pct_asian = ethnicity_asian / ethnicity_total * 100,
    pct_black = ethnicity_black / ethnicity_total * 100,
    pct_mixed = ethnicity_mixed / ethnicity_total * 100,
    pct_other = ethnicity_other / ethnicity_total * 100
  )

# # # Verification # # #

cat("Number of cancer alliances:", nrow(ca_ethnicity), "\n")

# Verify percentages sum to ~100% for each CA
pct_sums <- ca_ethnicity %>%
  mutate(pct_sum = pct_white + pct_asian + pct_black + pct_mixed + pct_other)
stopifnot(all(abs(pct_sums$pct_sum - 100) < 0.1))

# Verify all 21 CAs from McedToCAMappingByHand.csv are present
mced <- read.csv(paste0(linkingDirPath, "McedToCAMappingByHand.csv"))
missing_cas <- setdiff(mced$Cancer.Alliance, ca_ethnicity$Cancer.Alliance)
if (length(missing_cas) > 0) {
  stop("Missing cancer alliances: ", paste(missing_cas, collapse = ", "))
}
cat("All 21 cancer alliances matched successfully.\n")

# Print summary
cat("Pct White range:", round(min(ca_ethnicity$pct_white), 1), "% to",
    round(max(ca_ethnicity$pct_white), 1), "%\n")
cat("Pct Asian range:", round(min(ca_ethnicity$pct_asian), 1), "% to",
    round(max(ca_ethnicity$pct_asian), 1), "%\n")
cat("Pct Black range:", round(min(ca_ethnicity$pct_black), 1), "% to",
    round(max(ca_ethnicity$pct_black), 1), "%\n")

# # # Save output # # #

write.csv(ca_ethnicity, paste0(outDirPath, "ethnicity_by_cancer_alliance.csv"), row.names = FALSE)
cat("Saved:", paste0(outDirPath, "ethnicity_by_cancer_alliance.csv"), "\n")
