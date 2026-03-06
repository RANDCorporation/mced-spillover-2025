

# PSW sensitivity analysis using cancer mortality
# Accounts for regional imbalances in cancer mortality rates using
# age-standardised mortality rate as the PS model covariate.

# load libraries ----------------------------------------------------------

library(twang)
library(here)

# set paths ---------------------------------------------------------------

dataDirPath <- here("data", "cleaned_data")

# load the data -----------------------------------------------------------

# Existing PS weights file (for CancerAlliance, mcedTreated, and covariates)
data <- read.csv(file.path(dataDirPath, "PS_wts_for_sensruns.csv"))

# Mortality data at cancer alliance level
mortality_data <- read.csv(file.path(dataDirPath, "mortality_by_cancer_alliance.csv"))

# merge -------------------------------------------------------------------

# Handle column name difference: PS file uses "CancerAlliance", mortality file uses "Cancer.Alliance"
data <- merge(data, mortality_data, by.x = "CancerAlliance", by.y = "Cancer.Alliance")

cat("Merged data dimensions:", dim(data), "\n")
cat("Treatment indicator:\n")
print(table(data$mcedTreated))

# fit logistic PS model ---------------------------------------------------

ps.log <- glm(mcedTreated ~ mortality_asr,
              family = binomial(link = "logit"), data = data)

summary(ps.log)

# compute ATT weights -----------------------------------------------------
# Treated units get weight = 1; control units get weight = exp(linear predictor)

data$w.logit.mortality <- rep(1, nrow(data))
data$w.logit.mortality[data$mcedTreated == 0] <- exp(predict(ps.log, subset(data, mcedTreated == 0)))

# construct final PSW column
data$psw_mortality <- data$w.logit.mortality

cat("\nWeight summary for treated units:\n")
print(summary(data$psw_mortality[data$mcedTreated == 1]))
cat("\nWeight summary for control units:\n")
print(summary(data$psw_mortality[data$mcedTreated == 0]))

# check balance -----------------------------------------------------------

# Primary balance check: mortality ASR
bal.logit.mortality <- dx.wts(x = data$w.logit.mortality, data = data,
	vars = c("mortality_asr"),
	treat.var = "mcedTreated",
	perm.test.iters = 0, estimand = "ATT")

cat("\nBalance check (mortality variables only):\n")
print(bal.logit.mortality)
print(bal.table(bal.logit.mortality))

# Expanded balance check: mortality + existing covariates
bal.logit.mortality.full <- dx.wts(x = data$w.logit.mortality, data = data,
	vars = c("mortality_asr",
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
print(bal.logit.mortality.full)

bal.info <- bal.table(bal.logit.mortality.full)
print(bal.info)

# save balance tables -----------------------------------------------------

write.table(bal.info[[1]], file.path(dataDirPath, "unweighted_balance_mortality.csv"), sep = ",")
write.table(bal.info[[2]], file.path(dataDirPath, "weighted_balance_mortality.csv"), sep = ",")

cat("\nBalance tables saved to:\n")
cat("  ", file.path(dataDirPath, "unweighted_balance_mortality.csv"), "\n")
cat("  ", file.path(dataDirPath, "weighted_balance_mortality.csv"), "\n")

# save weights for sensitivity analysis runs ------------------------------

output <- data[, c("CancerAlliance", "mcedTreated", "mortality_asr", "psw_mortality")]

write.csv(output, file.path(dataDirPath, "PS_wts_mortality_for_sensruns.csv"), row.names = FALSE)

cat("\nWeights saved to:", file.path(dataDirPath, "PS_wts_mortality_for_sensruns.csv"), "\n")
cat("Dimensions:", dim(output), "\n")
