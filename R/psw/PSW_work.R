
# load libraries ----------------------------------------------------------

#Use the TWANG package to help us get balance tables
library(twang)

# setting working directory
library(here)
setwd(here())

# load the data -----------------------------------------------------------

data <- read.table(file.path("data", "data_for_psw.csv"), header=T, sep=",")


###################################################################################
#PS weights can only include 2 variables due to constrained N
#So, we opt to balance on # of staff & pop size as part of the PSW
#for sensitivity analyses given the lack of balance we saw on these two variables
#when comparing treatment and control groups
##################################################################################

#Fit a logistic model for PS weights
ps.log=glm(mcedTreated ~ NumStaff+Pop, family = binomial(link = "logit"), data=data)

data$psw <- rep(1,nrow(data))
data$psw[data$mcedTreated==0] <- exp(predict(ps.log,subset(data,mcedTreated==0)))

#check balance - going to use dx.wts so can assess balance using multiple metrics

bal.logit <- dx.wts(x = data$psw,data=data,
	vars=c("NumStaff","Pop"),
	treat.var="mcedTreated",
	perm.test.iters=0, estimand = "ATT")

bal.logit

bal.table(bal.logit) #balance is notably better; but it is not perfect

#check balance on everything

bal.logit <- dx.wts(x = data$psw,data=data,
	vars=c("PercentageAbsent_Month_TMinus1","PercentageAbsent_Month_TMinus2","PercentageAbsent_Month_TMinus3",
		"PercentageAbsent_Month_TMinus4","PercentageAbsent_Month_TMinus5","PercentageAbsent_Month_TMinus6",
	        "DiagnosticDelayRate_Month_TMinus1","DiagnosticDelayRate_Month_TMinus2",
		"DiagnosticDelayRate_Month_TMinus3","DiagnosticDelayRate_Month_TMinus4",
		"DiagnosticDelayRate_Month_TMinus5","DiagnosticDelayRate_Month_TMinus6",
		"NumProviderOrgs","NumStaff","Pop","Pop0to49perc","Pop50to64perc","Pop65to74perc",                                             		"Pop75to84perc","Pop85plusperc","PopFemalePerc","PopMalePerc","CancerPrevPer100k","CancerReferralsPrevPer100k"),
	treat.var="mcedTreated",
	perm.test.iters=0, estimand = "ATT")

bal.logit

bal.info=bal.table(bal.logit)

#save balance tables
write.table(bal.info[[1]], file.path("data", "results", "unweighted_balance_10142025.csv"),sep=",")
write.table(bal.info[[2]],file.path("data", "results", "weighted_balance_10142025.csv"),sep=",")

#save psw weights to data for use in sensitivity analyses

write.table(data, file.path("data", "PS_wts_for_sensruns.csv"),sep=",",row.names=F)