

# PSW sensitivity analysis using IMD (Index of Multiple Deprivation)
# Accounts for regional imbalances in area deprivation using
# population-weighted average IMD score as the PS model covariate.

# load libraries ----------------------------------------------------------

library(twang)
library(here)

# set paths ---------------------------------------------------------------

dataDirPath <- here("data", "cleaned_data")

# load the data -----------------------------------------------------------

# Existing PS weights file (for CancerAlliance, mcedTreated, and covariates)
data <- read.csv(file.path(dataDirPath, "PS_wts_for_sensruns.csv"))

# IMD data at cancer alliance level
imd_data <- read.csv(file.path(dataDirPath, "imd_by_cancer_alliance.csv"))

# merge -------------------------------------------------------------------

# Handle column name difference: PS file uses "CancerAlliance", IMD file uses "Cancer.Alliance"
data <- merge(data, imd_data, by.x = "CancerAlliance", by.y = "Cancer.Alliance")

cat("Merged data dimensions:", dim(data), "\n")
cat("Treatment indicator:\n")
print(table(data$mcedTreated))

# fit logistic PS model ---------------------------------------------------

ps.log <- glm(mcedTreated ~ imd_pop_weighted_avg,
              family = binomial(link = "logit"), data = data)

summary(ps.log)

# compute ATT weights -----------------------------------------------------
# Treated units get weight = 1; control units get weight = exp(linear predictor)

data$w.logit.imd <- rep(1, nrow(data))
data$w.logit.imd[data$mcedTreated == 0] <- exp(predict(ps.log, subset(data, mcedTreated == 0)))

# construct final PSW column
data$psw_imd <- data$w.logit.imd

cat("\nWeight summary for treated units:\n")
print(summary(data$psw_imd[data$mcedTreated == 1]))
cat("\nWeight summary for control units:\n")
print(summary(data$psw_imd[data$mcedTreated == 0]))

# check balance -----------------------------------------------------------

# Primary balance check: IMD pop-weighted avg + quintile percentages
bal.logit.imd <- dx.wts(x = data$w.logit.imd, data = data,
	vars = c("imd_pop_weighted_avg",
		"imd_q1_pct", "imd_q2_pct", "imd_q3_pct", "imd_q4_pct", "imd_q5_pct"),
	treat.var = "mcedTreated",
	perm.test.iters = 0, estimand = "ATT")

cat("\nBalance check (IMD variables only):\n")
print(bal.logit.imd)
print(bal.table(bal.logit.imd))

# Expanded balance check: IMD + existing covariates
bal.logit.imd.full <- dx.wts(x = data$w.logit.imd, data = data,
	vars = c("imd_pop_weighted_avg",
		"imd_q1_pct", "imd_q2_pct", "imd_q3_pct", "imd_q4_pct", "imd_q5_pct",
		"NumProviderOrgs", "NumStaff", "Pop",
		"Pop0to49perc", "Pop50to64perc", "Pop65to74perc",
		"Pop75to84perc", "Pop85plusperc",
		"PopFemalePerc", "PopMalePerc",
		"CancerPrevPer100k", "CancerReferralsPrevPer100k",
		"PercentageAbsent_Month_TMinus1", "PercentageAbsent_Month_TMinus2",
		"PercentageAbsent_Month_TMinus3", "PercentageAbsent_Month_TMinus4",
		"PercentageAbsent_Month_TMinus5", "PercentageAbsent_Month_TMinus6",
		"DiagnosticDelayRate_Month_TMinus1", "DiagnosticDelayRate_Month_TMinus2",
		"DiagnosticDelayRate_Month_TMinus3", "DiagnosticDelayRate_Month_TMinus4",
		"DiagnosticDelayRate_Month_TMinus5", "DiagnosticDelayRate_Month_TMinus6"),
	treat.var = "mcedTreated",
	perm.test.iters = 0, estimand = "ATT")

cat("\nBalance check (all covariates):\n")
print(bal.logit.imd.full)

bal.info <- bal.table(bal.logit.imd.full)
print(bal.info)

# save balance tables -----------------------------------------------------

write.table(bal.info[[1]], file.path(dataDirPath, "unweighted_balance_IMD.csv"), sep = ",")
write.table(bal.info[[2]], file.path(dataDirPath, "weighted_balance_IMD.csv"), sep = ",")

cat("\nBalance tables saved to:\n")
cat("  ", file.path(dataDirPath, "unweighted_balance_IMD.csv"), "\n")
cat("  ", file.path(dataDirPath, "weighted_balance_IMD.csv"), "\n")

# save weights for sensitivity analysis runs ------------------------------

output <- data[, c("CancerAlliance", "mcedTreated", "imd_pop_weighted_avg", "psw_imd")]

write.csv(output, file.path(dataDirPath, "PS_wts_IMD_for_sensruns.csv"), row.names = FALSE)

cat("\nWeights saved to:", file.path(dataDirPath, "PS_wts_IMD_for_sensruns.csv"), "\n")
cat("Dimensions:", dim(output), "\n")
