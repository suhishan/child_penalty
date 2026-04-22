# Load required libraries
library(tidyverse)
library(cmdstanr)
library(fixest)
library(haven)
library(patchwork)
library(Matrix)

## --------------------------------------------------##
## Run all the cleaning and stuff beforehand for any changes ##
## -------------------------------------------------- ##

# source("scripts/cleaning_ipums.R")
source("scripts/matching_2001.R")
source("scripts/matching_2011.R")

## -------------------------------------------------- ##
## Load data (with districts used for matching)       ##
## -------------------------------------------------- ##


overall_df_01 <- read_rds("transformed_data/overall_df_for_analysis_2001.Rds")
overall_df_01 <- overall_df_01 |> 
  mutate(
    year = 2001, weights_total = weights * perwt
  )


overall_df_11 <- read_rds("transformed_data/overall_df_for_analysis_2011.Rds")
overall_df_11 <- overall_df_11 |> 
  mutate(
    year = 2011, weights_total = weights * perwt
  )
joined_df <- bind_rows(
  overall_df_01 ,
  overall_df_11
)

## -------------------------------------------------- ##
## Load data (with districts not used for matching)   ##
## -------------------------------------------------- ##

overall_df_nodis_01 <- read_rds(
    "transformed_data/overall_df_for_analysis_2001_nodis.Rds"
)
overall_df_nodis_01 <- overall_df_nodis_01 |> 
  mutate(
    year = 2001, weights_total = weights * perwt
  )

overall_df_nodis_11 <- read_rds(
    "transformed_data/overall_df_for_analysis_2011_nodis.Rds"
)

overall_df_nodis_11 <- overall_df_nodis_11 |> 
  mutate(
    year = 2011, weights_total = weights * perwt
  )

joined_df_nodis <- bind_rows(
    overall_df_nodis_01, overall_df_nodis_11
)

## -------------------------------------------------- ##
## A function that detrends the dependent variable.
## -------------------------------------------------- ##

 detrend <- function(df_sex) {
    df_sex$t_num <- as.numeric(df_sex$t)
    pre_data <- df_sex[df_sex$t_num < -1,]
   
    pre_reg <- feols(
      employed ~ t_num | factor(age) + factor(year),
      weights = ~weights_total,
      vcov = "hetero",
      data = df_sex
    )
   
    beta_time <- coef(pre_reg)['t_num']
    df_sex$employed_detrended <- df_sex$employed - (beta_time * df_sex$t_num)
   
    return (df_sex)
 }


## -------------------------------------------------- ##
## A function to DO the regression and return the estimates and their SES
## and all that jazz.
## -------------------------------------------------- ##


calc_estimates <- function(dataframe) {

  d_f <- dataframe[dataframe$sex == 1,]
  d_m <- dataframe[dataframe$sex == 2,]

  # ----- Detrend the dependent variable -----#
  d_f <- detrend(d_f)
  d_m <- detrend(d_m)

  # ----- t as factor with reference level -2 ----- #
  d_f$t <- factor(d_f$t)
  d_f$t <- relevel(d_f$t, ref = "-2")

  # Men
  d_m$t <- factor(d_m$t)
  d_m$t <- relevel(d_m$t, ref = "-2")

  ## Women
  # Overall Model (with event time dummies)
  mf.1 <- feols(
      employed_detrended ~ t | factor(age) +factor(year),
      weights = d_f$weights_total,
      data = d_f,
      vcov = 'hetero'
  )
  coefs.f1 <- coef(mf.1)
  coefs.f1 <- append(coefs.f1, 0, after = 3)  # Insert 0 for reference level (-2)
  se.f1 <- se(mf.1)
  se.f1 <- append(se.f1, 0, after = 3)
 
  ## ----- Counterfactuals ----- ##
  d_f$t_cf <- factor(
    "-2", levels = levels(d_f$t)
  )

  d_f$predict.f2 <- predict(
    mf.1, newdata = d_f |> select(age, year, t = t_cf)
  )
  denominator.f <- d_f |> group_by(t) |> 
    summarize(weighted.mean(predict.f2, weights_total)) |> pull()

  # Build estimates tibble for women
  estimates <- tibble(
      t = seq(-5, 10, by = 1),
      coefs.f1,
      se.f1,
      denominator.f,
  ) |> 
    mutate(
      penalty.f = coefs.f1 / denominator.f,
      penalty.f.se = se.f1/denominator.f # Treating the denominator as non-stochastic
    ) 

  ## Men
  mm.1 <- feols(
      employed_detrended ~ t | factor(age) + factor(year),
      weights = d_m$weights_total,
      data = d_m,
      vcov = 'hetero'
  )
  coefs.m1 <- coef(mm.1)
  coefs.m1 <- append(coefs.m1, 0, after = 3)
  se.m1 <- se(mm.1)
  se.m1 <- append(se.m1, 0, after = 3)

  ## ----- Counterfactuals ----- ##
  d_m$t_cf <- factor(
    "-2", levels = levels(d_m$t)
  )

  d_m$predict.m2 <- predict(
    mm.1, newdata = d_m |> select(age, year, t = t_cf)
  )

  denominator.m <- d_m |> group_by(t) |> 
    summarize(weighted.mean(predict.m2, weights_total)) |> pull()
    
  # Append men's estimates to the tibble
  estimates <- estimates |> mutate(
      coefs.m1,
      se.m1,
      denominator.m
  ) |> mutate(
      penalty.m = coefs.m1 / denominator.m,
      penalty.m.se = se.m1 / denominator.m # Treating the denominator as non-stochastic.
  )

  return (list(estimates, mf.1, mm.1))

}

## -------------------------------------------------- ##
## Extract Estimates.
## -------------------------------------------------- ##


## ----- Districts used in the matching -----##
est_01 <- calc_estimates(overall_df_01)
est_11 <- calc_estimates(overall_df_11)
est_join <- calc_estimates(joined_df)

## ----- Districts not used in matching ----- ##
est_join_nodis <- calc_estimates(joined_df_nodis)
est_nodis_01 <- calc_estimates(overall_df_nodis_01)
est_nodis_11 <- calc_estimates(overall_df_nodis_11)


## -------------------------------------------------- ##
## Calculate the child penalty and its standard error.
## -------------------------------------------------- ##

calc_penalty <- function(estimates_list) { # model_f and model_m are model objects.
  estimates <- estimates_list[[1]]
  # Calculate weights for every Beta.
  weights_m <- 1/estimates$denominator.m
  weights_f <- 1/estimates$denominator.f

  is_post <- c(rep(FALSE, times = 5), rep(TRUE, times = 11))

  weights_m <- ifelse(is_post, 1/11 * weights_m, -1/5 * weights_m)
  weights_f <- ifelse(is_post, -1/11 * weights_f, 1/5 * weights_f)

  w <- c(weights_m, weights_f)
  beta_s <- c(estimates$coefs.m1, estimates$coefs.f1)

  ## ----- Covariance ----- ##

  cov_m <- as.matrix(vcov(estimates_list[[3]]))
  cov_f <- as.matrix(vcov(estimates_list[[2]]))

  ## Add 0 rows and columns in t = -2.

  cov_m <- rbind(cov_m[1:3,] , `t-2`= rep(0, 15), cov_m[4:15,])
  cov_m <- cbind(cov_m[, 1:3], `t-2` = rep(0, 16), cov_m[, 4:15])

  cov_f <- rbind(cov_f[1:3,] , `t-2`= rep(0, 15), cov_f[4:15,])
  cov_f <- cbind(cov_f[, 1:3], `t-2` = rep(0, 16), cov_f[, 4:15])

  COV <- as.matrix(bdiag(cov_m, cov_f))

  ## Penalty ##
  penalty <- t(w) %*% beta_s
  variance <- t(w) %*% COV %*% w
  penalty_sd <- sqrt(variance)

  return(list(penalty, penalty_sd, variance))
}


## -------------------------------------------------- ##
## Plot things.
## -------------------------------------------------- ##


## ----- Plots raw estimates of child penalties. --- ##


#TODO: Think about the title.
  plot_coef <- function(estimates_list){
    estimates <- estimates_list[[1]]

    plot_object <- estimates |> 
    mutate(
        lc_f = coefs.f1 - 1.811 * se.f1, # 93% Confidence Intervals
        uc_f = coefs.f1 + 1.81 * se.f1,
        lc_m = coefs.m1 - 1.81 * se.m1,
        uc_m = coefs.m1 + 1.81 * se.m1
    ) |> 
    ggplot(aes(x = t))+
    geom_pointrange(aes(
        y = coefs.f1, ymin = lc_f, ymax = uc_f, color = "Female"
    ), , shape = 17) +
    geom_pointrange(aes(
        y = coefs.m1, ymin = lc_m, ymax = uc_m, color = "Male"
    ), shape = 16) +
    geom_hline(yintercept = 0, linetype = 2, alpha = 0.5)+
    geom_vline(xintercept = -0.5, linetype = 2)+
    coord_cartesian(ylim = c(-0.4, 0.3))+
    scale_color_viridis_d(
      option = "cividis", begin = 0.2, end = 0.8, 
      name = "Sex"
    )+
    scale_x_continuous(breaks = unique(estimates$t))+
    labs(
      x = "Event time(t)", y = "Impact on Employment Rate", 
      subtitle = "Estimates relative to event time t = -2"
    )+
    theme_classic()+
    theme(
      legend.position = "top"
    )
    
    ##----- Return the plot object  ----- #
    return (plot_object)

}

plot_penalty <- function(estimates_list, title) {
    estimates <- estimates_list[[1]]
    penalty <- round(calc_penalty(estimates_list)[[1]], 4) * 100
    penalty_sd <- round(calc_penalty(estimates_list)[[2]], 4) * 100

    plot_object <- estimates |> 
      mutate(
        pen_lc_f = penalty.f - 1.811 * penalty.f.se ,
        pen_uc_f = penalty.f + 1.811 * penalty.f.se ,
        pen_lc_m = penalty.m - 1.811 * penalty.m.se,
        pen_uc_m = penalty.m + 1.811 * penalty.m.se
      ) |> 
   ggplot(aes(x = t))+
    geom_pointrange(aes(
        y = penalty.f, ymin = pen_lc_f, ymax = pen_uc_f, color = "Female"
    ), shape = 17) +
    geom_pointrange(aes(
        y = penalty.m, ymin = pen_lc_m, ymax = pen_uc_m, color = "Male"
    ), shape = 16) +
    geom_hline(yintercept = 0, linetype = 2, alpha = 0.5)+
    geom_vline(xintercept = -0.5, linetype = 2)+
    annotate("text", label = paste0("Child Penalty = ", penalty, "%"," (", penalty_sd, ")"), x = 8, y = 0.2)+
    coord_cartesian(ylim = c(-0.4, 0.3))+
    scale_color_viridis_d(
      option = "cividis", begin = 0.2, end = 0.8, 
      name = "Sex"
    )+
    scale_x_continuous(breaks = unique(estimates$t))+
    labs(
      x = "Event time(t)", y = "Impact on Employment Rate", 
      subtitle = "Estimates relative to event time t = -2",
      title = paste(title)
    )+
    theme_classic()+
    theme(
      legend.position = "top"
    )

  return(plot_object)

}


calc_penalty(est_01)
plot_penalty(est_11, "Penalty")


## ---------- Save Plots ---------- ##

# Districts used to match people.
ggsave(
  "./blog/images/og/penalty_est_01.png", 
  plot_penalty(est_01, "Child Penalty Estimates for Census Year 2001"), 
  bg = "white", dpi = 300, units = "cm", height = 14, width = 20)

ggsave(
  "./blog/images/og/penalty_est_11.png", 
  plot_penalty(est_11, "Child Penalty Estimates for Census Year 2011"), 
  bg = "white", dpi = 300, units = "cm", height = 14, width = 20)


ggsave(
  "./blog/images/og/penalty_est_join.png",
  plot_penalty(est_join, "Child Penalty Estimates (Overall)"), 
  bg = "white", dpi = 300, units = "cm", height = 14, width = 20)

# Districts not used to match people.

ggsave(
  "./blog/images/og/penalty_est_nodis_01.png",
  plot_penalty(est_nodis_01, "Child Penalty Estimates 2001 (No District used for matching)"),
  bg = "white", dpi = 300, units = "cm", height = 14, width = 20
)


ggsave(
  "./blog/images/og/penalty_est_nodis_11.png",
  plot_penalty(est_nodis_11, "Child Penalty Estimates 2011 (No District used for matching)"),
  bg = "white", dpi = 300, units = "cm", height = 14, width = 20
)


ggsave(
  "./blog/images/og/penalty_est_join_nodis.png",
  plot_penalty(est_join_nodis, "Child Penalty Estimates Overall (No District used for matching)"),
  bg = "white", dpi = 300, units = "cm", height = 14, width = 20
)
