Policy Simulations
================
Joshua Eagan
2025-07-23

``` r
# read in data
data = read.csv(file.path(here::here(), "data/processed/28day_clean_test_grouped_1m_delayRate_exclusively_protocol_not_screened.csv"))

# dropping exposure data
data = data %>%
  dplyr::select(-mced_treated) %>%
  
  # time var must be called "year"
  rename(year = period)

# Calculate 5% and 10% changes in mean diagnosis rate, across states and years. 
five_percent_effect <- 0.05*mean(data$percentage_not_told_diagnosis_outcome_within_28_days, na.rm = T)
ten_percent_effect  <- 0.10*mean(data$percentage_not_told_diagnosis_outcome_within_28_days, na.rm = T)

# Calculate a co-occuring policy effect
cooccur_effect <- -0.02*mean(data$percentage_not_told_diagnosis_outcome_within_28_days, na.rm = T)

# Scenario object for "no confounding" evaluation scenario:
scenarios_no_confounding <- list(five_percent_effect, ten_percent_effect)

# Scenario object for "co-occuring policy" evaluation scenario:
scenarios_co_occur <- list(c(five_percent_effect, cooccur_effect),
                           c(ten_percent_effect, cooccur_effect))

# Specify 2 models to simulate treatment effects: Linear fixed effect model,
#  and a linear model using ar-terms with no fixed-effects
lm_fe_unadj <- optic_model(
    name = "fixed_effect_linear",
    type = "reg",
    call = "lm",
    formula = percentage_not_told_diagnosis_outcome_within_28_days ~ as.factor(year) + as.factor(Cancer.Alliance) + treatment_level,
    se_adjust = "cluster-unit"
)
  
lm_ar <- optic_model(
    name = "auto_regressive_linear",
    type = "autoreg",
    call = "lm",
    formula = percentage_not_told_diagnosis_outcome_within_28_days ~ as.factor(year) + treatment_change,
    se_adjust = "none"
)

m_csa <- optic_model(
  name = "csa_did",
  type = "did",
  call = "att_gt",
  formula = percentage_not_told_diagnosis_outcome_within_28_days ~ treatment_level,
  model_args = list(yname = "percentage_not_told_diagnosis_outcome_within_28_days", tname = 'year', idname = 'Cancer.Alliance',
                    gname = 'treatment_date'),
  se_adjust = "none"
  
)

m_aug <- optic_model(
  name = "augsynth",
  type = "multisynth",
  call = "multisynth",
  formula = percentage_not_told_diagnosis_outcome_within_28_days ~ treatment_level,
  model_args = list(unit = as.name("Cancer.Alliance"),
                    time = as.name("year"),
                    lambda = 0.1),
  se_adjust = "none"
  
)

# just do TWFE and AR for now
sim_models <- list(lm_fe_unadj, lm_ar)

sim_config <- optic_simulation(
  
  x                        = data,
  models                   = sim_models,
  iters                    = 1000, 
  method                   = "no_confounding",
  unit_var                 = "Cancer.Alliance",
  treat_var                = "Cancer.Alliance",
  time_var                 = "year",
  effect_magnitude         = scenarios_no_confounding,
  n_units                  = c(5, 8, 11),
  effect_direction         = c("neg"),
  policy_speed             = c("instant"),
  n_implementation_periods = c(1)
)
```

    ## Number of Simulations: 6
    ## Number of Models: 2
    ## Iteration per Simulation : 1000
    ## Total number of Iterations to Run: 12000

``` r
results <- dispatch_simulations(
  
  sim_config,
  use_future = T,
  seed = 9782,
  verbose = 2,
  future.globals=c("cluster_adjust_se"),
  future.packages=c("MASS", "dplyr", "optic", "augsynth")
  
)
```

    ## JOB 1 OF 6 DISPATCHED:
    ##         DISPATCH METHOD: parallel (future)
    ##         PARAMS:
    ##             unit_var: Cancer.Alliance
    ##             time_var: year
    ##             effect_magnitude: 0.0140107673316667
    ##             n_units: 5
    ##             effect_direction: neg
    ##             policy_speed: instant
    ##             n_implementation_periods: 1
    ##             prior_control: level
    ##             treat_var: Cancer.Alliance
    ## JOB 2 OF 6 DISPATCHED:
    ##         DISPATCH METHOD: parallel (future)
    ##         PARAMS:
    ##             unit_var: Cancer.Alliance
    ##             time_var: year
    ##             effect_magnitude: 0.0140107673316667
    ##             n_units: 8
    ##             effect_direction: neg
    ##             policy_speed: instant
    ##             n_implementation_periods: 1
    ##             prior_control: level
    ##             treat_var: Cancer.Alliance
    ## JOB 3 OF 6 DISPATCHED:
    ##         DISPATCH METHOD: parallel (future)
    ##         PARAMS:
    ##             unit_var: Cancer.Alliance
    ##             time_var: year
    ##             effect_magnitude: 0.0140107673316667
    ##             n_units: 11
    ##             effect_direction: neg
    ##             policy_speed: instant
    ##             n_implementation_periods: 1
    ##             prior_control: level
    ##             treat_var: Cancer.Alliance
    ## JOB 4 OF 6 DISPATCHED:
    ##         DISPATCH METHOD: parallel (future)
    ##         PARAMS:
    ##             unit_var: Cancer.Alliance
    ##             time_var: year
    ##             effect_magnitude: 0.0280215346633333
    ##             n_units: 5
    ##             effect_direction: neg
    ##             policy_speed: instant
    ##             n_implementation_periods: 1
    ##             prior_control: level
    ##             treat_var: Cancer.Alliance
    ## JOB 5 OF 6 DISPATCHED:
    ##         DISPATCH METHOD: parallel (future)
    ##         PARAMS:
    ##             unit_var: Cancer.Alliance
    ##             time_var: year
    ##             effect_magnitude: 0.0280215346633333
    ##             n_units: 8
    ##             effect_direction: neg
    ##             policy_speed: instant
    ##             n_implementation_periods: 1
    ##             prior_control: level
    ##             treat_var: Cancer.Alliance
    ## JOB 6 OF 6 DISPATCHED:
    ##         DISPATCH METHOD: parallel (future)
    ##         PARAMS:
    ##             unit_var: Cancer.Alliance
    ##             time_var: year
    ##             effect_magnitude: 0.0280215346633333
    ##             n_units: 11
    ##             effect_direction: neg
    ##             policy_speed: instant
    ##             n_implementation_periods: 1
    ##             prior_control: level
    ##             treat_var: Cancer.Alliance

``` r
# Compare point estimates across models for the 5% change scenario, with instantaneous policy adoption:
df_compare <- results[[1]][(results[[1]]$se_adjustment == "cluster-unit")|(results[[1]]$model_name == "auto_regressive_linear"),]

true_est <- -round(five_percent_effect, 3)

grab_mean_and_se <- function(model_name){
  
  sim_mean <- round(mean(df_compare[df_compare$model_name == model_name,]$estimate), 3)
  sim_se   <- round(sd(df_compare[df_compare$model_name == model_name,]$estimate), 3)
  
  res <- paste0(sim_mean, " (", sim_se, ")")
  return(res)
  
}

print(paste0("True effect size: ", true_est))
```

    ## [1] "True effect size: -0.014"

``` r
print(paste0("FE LM effect: ", grab_mean_and_se('fixed_effect_linear')))
```

    ## [1] "FE LM effect: -0.014 (0.014)"

``` r
print(paste0("AR LM effect: ", grab_mean_and_se('auto_regressive_linear')))
```

    ## [1] "AR LM effect: -0.015 (0.012)"
