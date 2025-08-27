

# load libraries ----------------------------------------------------------

library(twang)

setwd("C:/Users/bethg/Documents/MCED spillover")


# load the data -----------------------------------------------------------

data <- read.table("data_for_psw.csv",header=T,sep=",")

#learning about data

#binary treatment indicator
table(data$mcedTreated)



#All the variables corresponding to Table 1 in proposal, which we use to show that control and intervention group regions appear #balanced.
#Number of providers, staff, population, pop by age band, pop by sex, cancer prevalence, cancer referrals
#Many variables are provided in two forms: raw count and as either a percentage of the population or as a rate per 100k population

#extra covariates - we don't want to lose balance on these - apparently we have
#3] "NumProviderOrgs"                   "NumStaff"                         
 #5] "Pop"                               "Pop0to49"                         
 #7] "Pop0to49perc"                      "Pop50to64"                        
 #9] "Pop50to64perc"                     "Pop65to74"                        
#11] "Pop65to74perc"                     "Pop75to84"                        
#13] "Pop75to84perc"                     "Pop85plus"                        
#15] "Pop85plusperc"                     "PopFemale"                        
#17] "PopFemalePerc"                     "PopMale"                          
#19] "PopMalePerc"                       "CancerPrevCount"                  
#21] "CancerPrevPer100k"                 "CancerReferrals"

#these are most important
#Covariate and Outcome variables for the first six months in our dataset which are prior to intervention exposure
#April 2021 is labeled Month_TMinus5; May 2021 is Month_TMinus4, up through Sep 2021 which is Month_TMinus1
#Note - If you pick a subset of time periods to balance on, best to exclude Sep 2021, since there may have been partial intervention #exposure during this period immediately before the NHS-Galleri trial start. For more info, see the timeline discussion in the #‘Intervention’ section of the proposal

#Covariate variable is Percentage of staff absent
#Outcome variable is diagnostic delay rate (percentage of referrals with wait times > 28 days for diagnostic resolution)

#try just using two variables:

#best - change between or average 2-6 or 6....

#PercentageAbsent_Month_TMinus1
#DiagnosticDelayRate_Month_TMinus1

###################################################################################
#do # of staff & pop size as part of the PSW for sens checks given lack of balance 
##################################################################################

#if twang is too much; go ahead and use logistic....but geez even that feels bad....

#twang fit

ps.twang <- ps(mcedTreated ~ PercentageAbsent_Month_TMinus1+DiagnosticDelayRate_Month_TMinus1,
 data = data,
 n.trees=1000,
 interaction.depth=2,
 shrinkage=0.01,
 perm.test.iters=0,
 stop.method=c("ks.max"),
 estimand = "ATT",
 verbose=FALSE)

#errors out

#logistic
#1 of each is clearly enough
ps.log=glm(mcedTreated ~ PercentageAbsent_Month_TMinus1+DiagnosticDelayRate_Month_TMinus1, 
	family = binomial(link = "logit"), data=data)

data$w.logit <- rep(1,nrow(data))
data$w.logit[data$mcedTreated==0] <- exp(predict(ps.log,subset(data,mcedTreated==0)))

#check balance - going to use dx.wts so can assess balance on more than just one thing

bal.logit <- dx.wts(x = data$w.logit,data=data,
	vars=c("PercentageAbsent_Month_TMinus1","DiagnosticDelayRate_Month_TMinus1"),
	treat.var="mcedTreated",
	perm.test.iters=0, estimand = "ATT")

bal.logit #fixes ES but not KS; maybe because small sample

bal.table(bal.logit)

#expand so include all 6 waves prior

bal.logit <- dx.wts(x = data$w.logit,data=data,
	vars=c("PercentageAbsent_Month_TMinus1","PercentageAbsent_Month_TMinus2","PercentageAbsent_Month_TMinus3",
		"PercentageAbsent_Month_TMinus4","PercentageAbsent_Month_TMinus5","PercentageAbsent_Month_TMinus6",
	        "DiagnosticDelayRate_Month_TMinus1","DiagnosticDelayRate_Month_TMinus2",
		"DiagnosticDelayRate_Month_TMinus3","DiagnosticDelayRate_Month_TMinus4",
		"DiagnosticDelayRate_Month_TMinus5","DiagnosticDelayRate_Month_TMinus6"),
	treat.var="mcedTreated",
	perm.test.iters=0, estimand = "ATT")

bal.logit #now doesn't fix es or ks since added more in

bal.table(bal.logit)




#not really different on one year prior so let's try and see where really different and only use that
#biggest differences at 4,5  so let's focus on those.

#######################
#try 2
#################
#logistic
ps.log=glm(mcedTreated ~ PercentageAbsent_Month_TMinus4+DiagnosticDelayRate_Month_TMinus4, 
	family = binomial(link = "logit"), data=data)

data$w.logit <- rep(1,nrow(data))
data$w.logit[data$mcedTreated==0] <- exp(predict(ps.log,subset(data,mcedTreated==0)))

#check balance - going to use dx.wts so can assess balance on more than just one thing

bal.logit <- dx.wts(x = data$w.logit,data=data,
	vars=c("PercentageAbsent_Month_TMinus4","DiagnosticDelayRate_Month_TMinus4"),
	treat.var="mcedTreated",
	perm.test.iters=0, estimand = "ATT")

bal.logit #fixes ES but not KS; maybe because small sample

bal.table(bal.logit)

#check balance on everything

bal.logit <- dx.wts(x = data$w.logit,data=data,
	vars=c("PercentageAbsent_Month_TMinus1","PercentageAbsent_Month_TMinus2","PercentageAbsent_Month_TMinus3",
		"PercentageAbsent_Month_TMinus4","PercentageAbsent_Month_TMinus5","PercentageAbsent_Month_TMinus6",
	        "DiagnosticDelayRate_Month_TMinus1","DiagnosticDelayRate_Month_TMinus2",
		"DiagnosticDelayRate_Month_TMinus3","DiagnosticDelayRate_Month_TMinus4",
		"DiagnosticDelayRate_Month_TMinus5","DiagnosticDelayRate_Month_TMinus6",
		"NumProviderOrgs","NumStaff","Pop","Pop0to49perc","Pop50to64perc","Pop65to74perc",                                             		"Pop75to84perc","Pop85plusperc","PopFemalePerc","PopMalePerc","CancerPrevPer100k","CancerReferralsPrevPer100k"),
	treat.var="mcedTreated",
	perm.test.iters=0, estimand = "ATT")

bal.logit #now doesn't fix es or ks since added more in

bal.info=bal.table(bal.logit)

write.table(bal.info[[1]],"unweighted_balance.csv",sep=",")

write.table(bal.info[[2]],"weighted_balance.csv",sep=",")


#DIFF are that big; it's a sample size issue making these look bigger than they are

#expand so include all 6 waves prior

bal.logit <- dx.wts(x = data$w.logit,data=data,
	vars=c("PercentageAbsent_Month_TMinus1","PercentageAbsent_Month_TMinus2","PercentageAbsent_Month_TMinus3",
		"PercentageAbsent_Month_TMinus4","PercentageAbsent_Month_TMinus5","PercentageAbsent_Month_TMinus6",
	        "DiagnosticDelayRate_Month_TMinus1","DiagnosticDelayRate_Month_TMinus2",
		"DiagnosticDelayRate_Month_TMinus3","DiagnosticDelayRate_Month_TMinus4",
		"DiagnosticDelayRate_Month_TMinus5","DiagnosticDelayRate_Month_TMinus6"),
	treat.var="mcedTreated",
	perm.test.iters=0, estimand = "ATT")

bal.logit #now doesn't fix es or ks since added more in

bal.table(bal.logit)

#how far can I push the logit

###############
#try 3
#################

#then let's see if I can also add in month 6

ps.log=glm(mcedTreated ~ PercentageAbsent_Month_TMinus4+DiagnosticDelayRate_Month_TMinus4+PercentageAbsent_Month_TMinus5+DiagnosticDelayRate_Month_TMinus5, 
	family = binomial(link = "logit"), data=data)


ps.log=glm(mcedTreated ~ PercentageAbsent_Month_TMinus4+DiagnosticDelayRate_Month_TMinus4+PercentageAbsent_Month_TMinus5+DiagnosticDelayRate_Month_TMinus5+PercentageAbsent_Month_TMinus6+DiagnosticDelayRate_Month_TMinus6, 
	family = binomial(link = "logit"), data=data)

#looks SO BAD