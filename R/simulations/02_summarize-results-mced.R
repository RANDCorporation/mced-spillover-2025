# summarize_results.R -----------------------------------------------------
# 
# Summarize results of the selection bias simulations
# This version relies on output saved in parquet format using arrow.
# 
# Adam Scherling, 2/3/2022



# load libraries ----------------------------------------------------------

library(arrow)
library(dplyr)
library(ggplot2)
library(glue)
library(tidyr)

setwd("C:/Users/bethg/Documents/MCED spillover")

# setup -------------------------------------------------------------------

# load summarize functions
# in the future, these should be exported, or better yet, incorporated into a summary method
source('summary-functions.R')

# define output path
outpath <- 'C:/Users/bethg/Documents/MCED spillover'

# set a datestamp
datestamp <- Sys.Date()


# load the data -----------------------------------------------------------

#sim_results <- open_dataset(glue('{outpath}/optic_raw_results_1000'))

# if the output is in several folders, they can be combined as follows:
# sim_results1 <- open_dataset(glue('{outpath}/2022-07-07'))
# sim_results2 <- open_dataset(glue('{outpath}/2022-07-08'))
# 
# sim_results <- c(sim_results1, sim_results2)

sim_results <- read.table("opptic_raw_results_1000.csv",header=T,sep=",")

# check the data ----------------------------------------------------------

# these parameters should uniquely identify each simulations
sim_results %>% 
  select(effect_magnitude, model_name, se_adjustment, n_units) %>%
  collect() %>%
  count(effect_magnitude, model_name, se_adjustment, n_units) %>% View()


# summarize the data ------------------------------------------------------

sim_summaries <- sim_results %>%
  collect() %>% 
  nest_by(effect_magnitude, model_name, se_adjustment, n_units, .keep=TRUE) %>%
  mutate(simulation_summary = summarize_results_noconf(data)) %>%
  summarize(simulation_summary, .groups='drop')

#needed this version to get it to run without errors
#diving into the summarize results piece in the summary file
summarize_results_noconf <- function(x) {

#x=sim_results

#right now doing this on entire file without subsetting

  biasVec <- Vectorize(bias, vectorize.args = c("x", "te"))
  mseVec <- Vectorize(mse, vectorize.args = c("x", "te"))
  cf <- correction_factor(test_stats = na.omit(x$t_stat), type = "t")

  r <- x %>%
    summarize(
      estimate=mean(na.omit(estimate)),
      se=mean(na.omit(se)),
      variance=mean(na.omit(variance)),
      t_stat=mean(na.omit(t_stat)),
      p_value=mean(na.omit(p_value)),
      #mse=mean(na.omit(mse)),
      mse=mean(mseVec(estimate, as.numeric(effect_magnitude), unique(effect_direction))),
      #max_n_unique_enact_years=max(n_unique_enact_years),
      bias=mean(biasVec(estimate, as.numeric(effect_magnitude), unique(effect_direction)))
    ) %>%
    mutate(
      outcome=unique(x$outcome),
      se_adjustment=unique(x$se_adjustment),
      effect_magnitude=as.numeric(unique(x$effect_magnitude)),
      effect_direction=unique(x$effect_direction),
      policy_speed=unique(x$policy_speed),
      n_units = unique(x$n_units),
      n_implementation_periods=unique(x$n_implementation_periods),
      prior_control=unique(x$prior_control),
      model_name=unique(x$model_name),
      model_call=unique(x$model_call),
      model_formula=unique(x$model_formula),
    ) %>%
    select(outcome, se_adjustment, model_name, model_call, model_formula,
           policy_speed, n_implementation_periods, prior_control, n_units, 
           effect_magnitude, effect_direction, estimate, se, variance,t_stat,
           p_value, mse, bias)
  
  if(unique(x$effect_direction) == "neg"){
    r$type_1_error <- mean(ifelse(na.omit(x$p_value) < 0.05, 1, 0))
    r$type_s_error <- type_s_error(x$estimate, x$p_value, unique(x$effect_direction))
    
    r$coverage <- coverage(betas = x$estimate, ses = x$se, cf = cf, 
                           te = -1*unique(x$effect_magnitude))
    
    ed = ifelse(unique(x$effect_magnitude)==0, "null", "neg")
    r$crr <- mean(correct_rejection_rate_flag(coeffs = na.omit(x$estimate), 
                                              ses = na.omit(x$se), 
                                              cf = cf, 
                                              effect_direction = ed))
  }
  return(r)
}



# export the data ---------------------------------------------------------

saveRDS(sim_summaries, "summarized_results_{datestamp}.Rds")

write.table(sim_summaries,"sim_summaries_mced_07292025.csv",sep=",",row.names=F)
