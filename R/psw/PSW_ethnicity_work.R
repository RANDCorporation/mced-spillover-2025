

# PSW sensitivity analysis using ethnicity
# Accounts for regional imbalances in ethnic composition using
# percent non-white population as the PS model covariate.

# load libraries ----------------------------------------------------------

library(twang)
library(here)

# set paths ---------------------------------------------------------------

dataDirPath <- here("data", "cleaned_data")

# load the data -----------------------------------------------------------

# Existing PS weights file (for CancerAlliance, mcedTreated, and covariates)
data <- read.csv(file.path(dataDirPath, "PS_wts_for_sensruns.csv"))

# Ethnicity data at cancer alliance level
ethnicity_data <- read.csv(file.path(dataDirPath, "ethnicity_by_cancer_alliance.csv"))

# merge -------------------------------------------------------------------

# Handle column name difference: PS file uses "CancerAlliance", ethnicity file uses "Cancer.Alliance"
data <- merge(data, ethnicity_data, by.x = "CancerAlliance", by.y = "Cancer.Alliance")

cat("Merged data dimensions:", dim(data), "\n")
cat("Treatment indicator:\n")
print(table(data$mcedTreated))

# derive percent non-white ------------------------------------------------

data$pct_nonwhite <- 1 - data$pct_white

# fit logistic PS model ---------------------------------------------------

ps.log <- glm(mcedTreated ~ pct_nonwhite,
              family = binomial(link = "logit"), data = data)

summary(ps.log)

# compute ATT weights -----------------------------------------------------
# Treated units get weight = 1; control units get weight = exp(linear predictor)

data$w.logit.ethnicity <- rep(1, nrow(data))
data$w.logit.ethnicity[data$mcedTreated == 0] <- exp(predict(ps.log, subset(data, mcedTreated == 0)))

# construct final PSW column
data$psw_ethnicity <- data$w.logit.ethnicity

cat("\nWeight summary for treated units:\n")
print(summary(data$psw_ethnicity[data$mcedTreated == 1]))
cat("\nWeight summary for control units:\n")
print(summary(data$psw_ethnicity[data$mcedTreated == 0]))

# check balance -----------------------------------------------------------

# Primary balance check: pct_nonwhite + individual ethnicity percentages
bal.logit.ethnicity <- dx.wts(x = data$w.logit.ethnicity, data = data,
	vars = c("pct_nonwhite",
		"pct_white", "pct_asian", "pct_black", "pct_mixed", "pct_other"),
	treat.var = "mcedTreated",
	perm.test.iters = 0, estimand = "ATT")

cat("\nBalance check (ethnicity variables only):\n")
print(bal.logit.ethnicity)
print(bal.table(bal.logit.ethnicity))

# Expanded balance check: ethnicity + existing covariates
bal.logit.ethnicity.full <- dx.wts(x = data$w.logit.ethnicity, data = data,
	vars = c("pct_nonwhite",
		"pct_white", "pct_asian", "pct_black", "pct_mixed", "pct_other",
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
print(bal.logit.ethnicity.full)

bal.info <- bal.table(bal.logit.ethnicity.full)
print(bal.info)

# save balance tables -----------------------------------------------------

write.table(bal.info[[1]], file.path(dataDirPath, "unweighted_balance_ethnicity.csv"), sep = ",")
write.table(bal.info[[2]], file.path(dataDirPath, "weighted_balance_ethnicity.csv"), sep = ",")

cat("\nBalance tables saved to:\n")
cat("  ", file.path(dataDirPath, "unweighted_balance_ethnicity.csv"), "\n")
cat("  ", file.path(dataDirPath, "weighted_balance_ethnicity.csv"), "\n")

# save weights for sensitivity analysis runs ------------------------------

output <- data[, c("CancerAlliance", "mcedTreated", "pct_nonwhite", "psw_ethnicity")]

write.csv(output, file.path(dataDirPath, "PS_wts_ethnicity_for_sensruns.csv"), row.names = FALSE)

cat("\nWeights saved to:", file.path(dataDirPath, "PS_wts_ethnicity_for_sensruns.csv"), "\n")
cat("Dimensions:", dim(output), "\n")
