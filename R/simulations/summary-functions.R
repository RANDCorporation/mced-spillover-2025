# summarize-results.R
# Functions for summarizing simulation results


# helper functions --------------------------------------------------------

#' calculate the correction factor
#'
#' @param test_stats test statistics
#' @param type what type of test statistic is being supplied; one of "t", "wald"; default is "t"
correction_factor <- function(test_stats, type="t") {
  if (type == "t") {
    f_stats <- (test_stats)^2
    f_stats <- sort(f_stats)
    high_cut <- 0.95 * length(test_stats)
    femp95 <- f_stats[high_cut]
    freal <- qf(0.95, 1, Inf)
    corr_factor <- sqrt(femp95 / freal)
  }
  
  return(corr_factor)
}

coverage <- function(betas, ses, cf, te=0) {
  adj_ses <- ses*cf
  ind <- rep(0, length(betas))
  low95 <- betas - 1.96 * adj_ses
  high95 <- betas + 1.96 * adj_ses
  ind[te > low95 & te < high95] <- 1
  return(sum(ind) / length(betas))
}

#' formula for correcting p-values using correction factor
#' 
#' @param coeffs vector of regression coefficients
#' @param ses vector of standard errors related to provided coefficients
#' @param cf correction factor to use for adjustment
#' @param effect_direction direction of true effect, one of "null", "neg", "pos"
correct_rejection_rate_flag <- function(coeffs, ses, cf, effect_direction="null") {
  adj_ses <- ses * cf
  low95 <- coeffs - 1.96 * adj_ses
  high95 <- coeffs + 1.96 * adj_ses
  
  if (effect_direction == "null") {
    # 1 if confidence interval contains 0
    sig_dummy <- as.integer((low95 < 0 & high95 > 0))
  } else if (effect_direction == "pos") {
    # 1 if confidence interval does not contain 0
    sig_dummy <- as.integer((low95 < 0 & high95 > 0) == FALSE)
    # if significant but in the wrong direction, set to 0
    sig_dummy[sig_dummy == 1 & coeffs < 0] <- 0
  } else if (effect_direction == "neg") {
    # 1 if confidence interval does not contain 0
    sig_dummy <- as.integer((low95 < 0 & high95 > 0) == FALSE)
    # if significant but in the wrong direction, set to 0
    sig_dummy[sig_dummy == 1 & coeffs > 0] <- 0
  }
  
  return(sig_dummy)
}


#' helper for mean squared error
mse <- function(x, te, effect_direction) {
  if (effect_direction == "null") {
    (x - 0) ^ 2
  } else if(effect_direction == "neg") {
    (x - (-te))^2
  } else{
    (x - te)^2
  }
}

#' helper for bias
bias <- function(x, te, effect_direction) {
  if (effect_direction == "null") {
    x - 0
  } else if(effect_direction == "neg"){
    (x - (-te))
  } else{
    (x - te)
  }
}

#' create binary indicator for significance of p-value based on given level
#' 
#' @param p vector of p-values
#' @param level threshold for significance; default is 0.05
pval_flag <- function(p, level=0.05) {
  p[p < level] <- 1
  p[p != 1] <- 0
  return(p)
}



#' Calculate type S error - how often model gets direction of effect wrong
#' 
#' number of significant betas of certain direction divided by all significant betas
#' 
#' @param betas vector of regression coefficients
#' @param pvals vector of p values for betas
#' @param effect_direction direction of true effect
type_s_error <- function(betas, pvals, effect_direction) {
  if (effect_direction == "null") {
    return(NA)
  }
  if (length(betas[pvals < 0.05]) != 0) {
    if (effect_direction == "neg") {
      s_error <- length(betas[betas > 0 & pvals < 0.05]) / length(betas[pvals < 0.05])
    } else {
      s_error <- length(betas[betas < 0 & pvals < 0.05]) / length(betas[pvals < 0.05])
    }
  }else{
    s_error <- 0
  }
  return(s_error)
}


# concurrent --------------------------------------------------------------


#' summary method for concurrent runs
summarize_concurrent <- function(x) {
  # need to vectorize some functions
  biasVec <- Vectorize(bias, vectorize.args = c("x", "te"))
  mseVec <- Vectorize(mse, vectorize.args = c("x", "te"))
  
  # get the null and neg runs separately
  null <- x %>%
    filter(effect_direction == "null")
  neg <- x %>%
    filter(effect_direction == "neg")
  
  # calculate correction factors from null run
  cf1 <- correction_factor(na.omit(null$test_stat1))
  cf2 <- correction_factor(na.omit(null$test_stat2))
  cfj <- correction_factor(na.omit(null$test_statj))
  
  results <- NULL
  for (tmp in list(neg)) {#null, 
    r <- tmp %>%
      summarize(
        estimate1=mean(na.omit(estimate1)),
        se1=mean(na.omit(se1)),
        variance1=mean(na.omit(variance1)),
        test_stat1=mean(na.omit(test_stat1)),
        mse1=mean(mseVec(estimate1, as.numeric(effect_magnitude1), unique(effect_direction))),
        bias1=mean(biasVec(estimate1, as.numeric(effect_magnitude1), unique(effect_direction))),
        estimate2=mean(na.omit(estimate2)),
        se2=mean(na.omit(se2)),
        variance2=mean(na.omit(variance2)),
        test_stat2=mean(na.omit(test_stat2)),
        mse2=mean(mseVec(estimate2, as.numeric(effect_magnitude2), unique(effect_direction))),
        bias2=mean(biasVec(estimate2, as.numeric(effect_magnitude2), unique(effect_direction))),
        joint.eff=mean(na.omit(joint.eff)),
        joint.eff.se=mean(na.omit(joint.eff.se)),
        joint.eff.var=mean(na.omit(variancej)),
        joint.test.stat=mean(na.omit(test_statj)),
        joint.mse=mean(mseVec(joint.eff, (as.numeric(effect_magnitude1)+as.numeric(effect_magnitude2)), unique(effect_direction))),
        joint.bias=mean(biasVec(joint.eff, (as.numeric(effect_magnitude1)+as.numeric(effect_magnitude2)), unique(effect_direction))),
        avg_min_distance=mean(min_distance),
        avg_max_distance=mean(max_distance),
        avg_mean_distance=mean(mean_distance)
      ) %>%
      mutate(
        se_adjustment=unique(tmp$se_adjustment),
        effect_magnitude1=as.numeric(unique(tmp$effect_magnitude1)),
        effect_magnitude2=as.numeric(unique(tmp$effect_magnitude2)),
        effect_direction=unique(tmp$effect_direction),
        policy_speed=unique(tmp$policy_speed),
        rho=unique(tmp$rho),
        n_units = unique(tmp$n_units),
        ordered = unique(tmp$ordered),
        n_implementation_periods=unique(tmp$n_units),
        model_name=unique(tmp$model_name),
        model_call=unique(tmp$model_call),
        model_formula=unique(tmp$model_formula),
        specification=unique(tmp$specification),
        years_apart=unique(tmp$years_apart)
      )
    
    if (unique(tmp$effect_direction) == "null") {
      r$type1_error1 <- mean(pval_flag(tmp$p_value1))
      r$type1_error2 <- mean(pval_flag(tmp$p_value2))
      r$type1_error_joint <- mean(pval_flag(tmp$joint.eff.pvalue))
      
      r$type_s_error1 <- NA
      r$type_s_error2 <- NA
      r$type_s_error_joint <- NA
      
      r$coverage1 <- coverage(betas = tmp$estimate1, ses = tmp$se1, cf = 1, 
                              te = 0)
      r$coverage2 <- coverage(betas = tmp$estimate2, ses = tmp$se2, cf = 1,
                              te = 0)
      r$coverage_joint <- coverage(betas = tmp$joint.eff, ses = tmp$joint.eff.se, cf = 1,
                                   te=0)
      
      r$power1 <- mean(correct_rejection_rate_flag(coeffs = tmp$estimate1, 
                                                   ses = tmp$se1, 
                                                   cf = 1, 
                                                   effect_direction = unique(tmp$effect_direction)))
      r$power2 <- mean(correct_rejection_rate_flag(coeffs = tmp$estimate2, 
                                                   ses = tmp$se2, 
                                                   cf = 1, 
                                                   effect_direction = unique(tmp$effect_direction)))
      r$power_joint <- mean(correct_rejection_rate_flag(coeffs = tmp$joint.eff, 
                                                        ses = tmp$joint.eff.se, 
                                                        cf = 1, 
                                                        effect_direction = unique(tmp$effect_direction)))
      
      
    } else if(unique(tmp$effect_direction) == "neg"){
      r$type1_error1 <- NA
      r$type1_error2 <- NA
      r$type1_error_joint <- NA
      
      r$type_s_error1 <- type_s_error(tmp$estimate1, tmp$p_value1, unique(tmp$effect_direction))
      r$type_s_error2 <- type_s_error(tmp$estimate2, tmp$p_value2, unique(tmp$effect_direction))
      r$type_s_error_joint <- type_s_error(tmp$joint.eff, tmp$joint.eff.pvalue, unique(tmp$effect_direction))
      
      r$coverage1 <- coverage(betas = tmp$estimate1, ses = tmp$se1, cf = 1, 
                              te = -1*unique(tmp$effect_magnitude1))
      r$coverage2 <- coverage(betas = tmp$estimate2, ses = tmp$se2, cf = 1,
                              te = -1*unique(tmp$effect_magnitude2))
      r$coverage_joint <- coverage(betas = tmp$joint.eff, ses = tmp$joint.eff.se, cf = 1,
                                   te=-1*(unique(tmp$effect_magnitude1)+unique(tmp$effect_magnitude2)))
      
      r$power1 <- mean(correct_rejection_rate_flag(coeffs = tmp$estimate1, 
                                                   ses = tmp$se1, 
                                                   cf = 1, 
                                                   effect_direction = unique(tmp$effect_direction)))
      r$power2 <- mean(correct_rejection_rate_flag(coeffs = tmp$estimate2, 
                                                   ses = tmp$se2, 
                                                   cf = 1, 
                                                   effect_direction = unique(tmp$effect_direction)))
      r$power_joint <- mean(correct_rejection_rate_flag(coeffs = tmp$joint.eff, 
                                                        ses = tmp$joint.eff.se, 
                                                        cf = 1, 
                                                        effect_direction = unique(tmp$effect_direction)))
      
    } else{
      r$type1_error1 <- NA
      r$type1_error2 <- NA
      r$type1_error_joint <- NA
      
      r$type_s_error1 <- type_s_error(tmp$estimate1, tmp$p_value1, unique(tmp$effect_direction))
      r$type_s_error2 <- type_s_error(tmp$estimate2, tmp$p_value2, unique(tmp$effect_direction))
      r$type_s_error_joint <- type_s_error(tmp$joint.eff, tmp$joint.eff.pvalue, unique(tmp$effect_direction))
      
      r$coverage1 <- coverage(betas = tmp$estimate1, ses = tmp$se1, cf = 1, 
                              te = unique(tmp$effect_magnitude1))
      r$coverage2 <- coverage(betas = tmp$estimate2, ses = tmp$se2, cf = 1,
                              te = unique(tmp$effect_magnitude2))
      r$coverage_joint <- coverage(betas = tmp$joint.eff, ses = tmp$joint.eff.se, cf = 1,
                                   te=(unique(tmp$effect_magnitude1) + unique(tmp$effect_magnitude2)))
      
      r$power1 <- mean(correct_rejection_rate_flag(coeffs = tmp$estimate1, 
                                                   ses = tmp$se1, 
                                                   cf = 1, 
                                                   effect_direction = unique(tmp$effect_direction)))
      r$power2 <- mean(correct_rejection_rate_flag(coeffs = tmp$estimate2, 
                                                   ses = tmp$se2, 
                                                   cf = 1, 
                                                   effect_direction = unique(tmp$effect_direction)))
      r$power_joint <- mean(correct_rejection_rate_flag(coeffs = tmp$joint.eff, 
                                                        ses = tmp$joint.eff.se, 
                                                        cf = 1, 
                                                        effect_direction = unique(tmp$effect_direction)))
    }
    
    results <- rbind(results, r)
  }
  
  return(results)
}


# no confounding ----------------------------------------------------------




summarize_results_noconf <- function(x) {
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
      mean_es_prior=mean(na.omit(mean_es_prior)), 
      mean_es_unempl=mean(na.omit(mean_es_unempl)), 
      mean_es_outcome=mean(na.omit(mean_es_outcome)), 
      sd_prior=mean(na.omit(sd_prior)), 
      sd_prior_old=mean(na.omit(sd_prior_old)), 
      sd=mean(na.omit(sd)),
      max_es_prior=max(max_es_prior),
      max_es_unempl=max(max_es_unempl),
      max_es_outcome=max(max_es_outcome),
      min_n_unique_enact_years=min(n_unique_enact_years),
      mean_n_unique_enact_years=mean(n_unique_enact_years),
      max_n_unique_enact_years=max(n_unique_enact_years),
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
           p_value, mse, bias, mean_es_prior, mean_es_unempl, mean_es_outcome, sd_prior,
           sd_prior_old, sd, max_es_prior, max_es_unempl, max_es_outcome,
           min_n_unique_enact_years, mean_n_unique_enact_years, max_n_unique_enact_years)
  
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


# selection bias ----------------------------------------------------------


summarize_results_selbias <- function(x) {
  # get means and range values
  summary_results <- x %>%
    summarize(across(c(estimate, se, variance, t_stat, p_value, mean_es_prior, mean_es_unempl, mean_es_outcome, 
                      sd_prior, sd_prior_old, sd), mean))
  
  summary_results <- cbind(
    summary_results,
    x %>% summarize(across(c(max_es_prior, max_es_unempl, max_es_outcome), max))
  )
  
  summary_results <- cbind(
    summary_results,
    x %>% summarize(min_n_unique_enact_years=min(n_unique_enact_years),
                    max_n_unique_enact_years=max(n_unique_enact_years),
                    min_n_treated=min(n),
                    max_n_treated=max(n))
  )
  
  # bias
  summary_results <- cbind(
    summary_results,
    data.frame(bias=mean(x$estimate - (-1)^(x$effect_direction=='neg') * x$effect_magnitude),
               mse=mean((x$estimate - (-1)^(x$effect_direction=='neg') * x$effect_magnitude)^2),
               var=mean( (x$estimate - mean(x$estimate))^2 )
    )
  )
  
  # type 1 error
  summary_results <- cbind(
    summary_results,
    data.frame(type_1_error=
                 case_when(
                   unique(x$effect_magnitude==0) ~ mean(ifelse(x$p_value < 0.05, 1, 0)),
                   TRUE ~ as.numeric(NA)
                )
    )
  )
  
  # power
  summary_results <- cbind(
    summary_results,
    data.frame(
      power=mean(
        correct_rejection_rate_flag(
          x$estimate,
          x$se,
          correction_factor(x$t_stat),
          effect_direction = "null"
        )
      )
    )
  )
  
  # coverage
  summary_results <- cbind(
    summary_results,
    data.frame(
      coverage=coverage(
        x$estimate,
        ses=x$se,
        cf=correction_factor(x$t_stat),
        te=(-1)^(x$effect_direction=='neg') * x$effect_magnitude
      )
    )
  )
  
  summary_results <- cbind(
    data.frame(
      outcome=unique(x$outcome),
      # se_adjustment=unique(x$se_adjustment),
      model_name=unique(x$model_name),
      model_call=unique(x$model_call),
      moel_formula=unique(x$model_formula),
      policy_speed=unique(x$policy_speed),
      n_implementation_years=unique(x$n_implementation_years),
      prior_control=unique(x$prior_control),
      bias_type=unique(x$bias_type),
      bias_size=unique(x$bias_size),
      effect_direction=unique(x$effect_direction),
      effect_magnitude=unique(x$effect_magnitude),
      b0=unique(x$b0),
      b1=unique(x$b1),
      b2=unique(x$b2),
      b3=unique(x$b3),
      b4=unique(x$b4),
      b5=unique(x$b5),
      a1=unique(x$a1),
      a2=unique(x$a2),
      a3=unique(x$a3),
      a4=unique(x$a4),
      a5=unique(x$a5),
      stringsAsFactors = FALSE
    ),
    summary_results
  )
  
  summary_results <- summary_results %>% 
    mutate(bias_es = abs(bias / sd))
  
  return(summary_results)
}

