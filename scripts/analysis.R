# Load required libraries
library(tidyverse)
library(cmdstanr)
library(fixest)
library(haven)

## --------------------------------------------------##
## Run all the cleaning and stuff beforehand for any changes ##
## -------------------------------------------------- ##

source("scripts/cleaning_ipums.R")
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


overall_df <- read_rds("transformed_data/overall_df_for_analysis_2011.Rds")
overall_df <- overall_df |> 
  mutate(
    year = 2011, weights_total = weights * perwt
  )
joined_df <- bind_rows(
  overall_df_01 ,
  overall_df
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
## A function that takes a dataframe as input 
## and outputs estimates
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
  ) |> mutate(penalty.f = coefs.f1 / denominator.f)  # Fixed: was denominator (undefined)

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
      penalty.m = coefs.m1 / denominator.m
  )

  return (list(estimates, mf.1, mm.1))

}

## -------------------------------------------------- ##
## Extract Estimates.
## -------------------------------------------------- ##

est_join_nodis <- calc_estimates(joined_df_nodis)[[1]]

est_nodis_01 <- calc_estimates(overall_df_nodis_01)[[1]]
est_nodis_11 <- calc_estimates(overall_df_nodis_11)[[1]]

## -------------------------------------------------- ##
## Plot things.
## -------------------------------------------------- ##


## ----- Plots raw estimates of child penalties. --- ##


#TODO: Think about the title.
plot_coef <- function(dataframe){
    plot_object <- dataframe |> 
    mutate(
        lc_f = coefs.f1 - se.f1,
        uc_f = coefs.f1 + se.f1,
        lc_m = coefs.m1 - se.m1,
        uc_m = coefs.m1 + se.m1
    ) |> 
    ggplot(aes(x = t))+
    geom_pointrange(aes(
        y = coefs.f1, ymin = lc_f, ymax = uc_f
    ), color = "red") +
    geom_pointrange(aes(
        y = coefs.m1, ymin = lc_m, ymax = uc_m
    )) +
    geom_hline(yintercept = 0, linetype = 2, alpha = 0.2)+
    geom_vline(xintercept = -0.5, linetype = 2)+
    coord_cartesian(ylim = c(-1, 0.5))+
    theme_classic()
    
    ##----- Return the plot object  ----- #
    return (plot_object)

}

plot_penalty <- function(dataframe) {
    dataframe |> 
    pivot_longer(c(penalty.f, penalty.m)) |> 
    mutate(
        Sex = rep(c('Women', 'Men'), times = 16)
    ) |> ggplot(aes(x = t, y = value)) +
    geom_point(aes(color = Sex)) +
    geom_line(aes(color = Sex)) +
    geom_hline(yintercept = 0, linetype = 2, alpha = 0.2)+
    geom_vline(xintercept = -0.5, linetype = 2)+
    coord_cartesian(ylim = c(-0.5, 0.5))+
    scale_color_manual(
        values = c('Women' = 'red', 'Men' = 'black')
    )+
    theme_classic()

}


plot_penalty(est_join_nodis)
plot_coef(est_join_nodis)

plot_penalty(est_nodis_01)
plot_penalty(est_nodis_11)
