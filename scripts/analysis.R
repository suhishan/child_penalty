# Load required libraries
library(tidyverse)
library(cmdstanr)
library(fixest)

# Load the overall data for 2011 (and 2001 for reference)
overall_df <- read_rds("transformed_data/overall_df_for_analysis_2011.Rds")
overall_df$year <- 2011
overall_df_01 <- read_rds("transformed_data/overall_df_for_analysis_2001.Rds")
overall_df_01$year <- 2001

joined_df <- bind_rows(
  overall_df_01 |> mutate(year = 2001),
  overall_df |> mutate(year = 2011))

# NOTE: Think about weights later.

# Normal LPM (Non-aggregate)
# Prepare data: factorize event time t, set reference level to -2
# Women
calc_estimates <- function(dataframe) {
  d_f <- dataframe[dataframe$sex == 1,]
  d_f$t <- factor(d_f$t)
  d_f$t <- relevel(d_f$t, ref = "-2")

  # Men
  d_m <- dataframe[dataframe$sex == 2,]
  d_m$t <- factor(d_m$t)
  d_m$t <- relevel(d_m$t, ref = "-2")

  ## Women
  # Overall Model (with event time dummies)
  mf.1 <- feols(
      outcome ~ t | factor(age) + factor(year),
      weights = d_f$weights,
      data = d_f,
      vcov = 'hetero'
  )
  coefs.f1 <- coef(mf.1)
  coefs.f1 <- append(coefs.f1, 0, after = 3)  # Insert 0 for reference level (-2)
  se.f1 <- se(mf.1)
  se.f1 <- append(se.f1, 0, after = 3)

  # Model without the event time dummies (for denominator/normalization)
  mf.2 <- feols(
      outcome ~ factor(age),
      weights = d_f$weights,
      data = d_f,
      vcov = 'hetero'
  )

  d_f$predict.f2 <- predict(mf.2)
  denominator.f <- aggregate(predict.f2 ~ t, data = d_f, FUN = mean)$predict.f2

  # Build estimates tibble for women
  estimates <- tibble(
      t = seq(-5, 10, by = 1),
      coefs.f1,
      se.f1,
      denominator.f,
  ) |> mutate(penalty.f = coefs.f1 / denominator.f)  # Fixed: was denominator (undefined)

  ## Men
  mm.1 <- feols(
      outcome ~ t | factor(age) + factor(year),
      weights = d_m$weights,
      data = d_m,
      vcov = 'hetero'
  )
  coefs.m1 <- coef(mm.1)
  coefs.m1 <- append(coefs.m1, 0, after = 3)
  se.m1 <- se(mm.1)
  se.m1 <- append(se.m1, 0, after = 3)

  # Model without event time dummies (for denominator)
  mm.2 <- feols(
      outcome ~ factor(age),
      weights = d_m$weights,
      data = d_m,
      vcov = 'hetero'
  )

  d_m$predict.m2 <- predict(mm.2)
  denominator.m <- aggregate(predict.m2 ~ t, data = d_m, FUN = mean)$predict.m2

  # Append men's estimates to the tibble
  estimates <- estimates |> mutate(
      coefs.m1,
      se.m1,
      denominator.m
  ) |> mutate(
      penalty.m = coefs.m1 / denominator.m
  )
    
  return (estimates)

}

est_01 <- calc_estimates(overall_df_01)
est_11 <- calc_estimates(overall_df)
est_join <- calc_estimates(joined_df)

## The graph
# Plot 1: Normalized penalties (penalty = coef / denominator)
est_join |>
    pivot_longer(cols = c('penalty.f', 'penalty.m'), names_to = "metric", values_to = "value") |> 
    mutate(sex = rep(c("Women", "Men"), times = 16)) |> 
    ggplot(aes(x = as.numeric(as.character(t)), y = value, color = sex)) +
    geom_point() +
    geom_line() +
    geom_vline(xintercept = -0.5, linetype = 2) +
    coord_cartesian(ylim = c(-1, 1)) +
    theme_classic() +
    labs(x = "Event Time (t)", y = "Normalized Effect", title = "Penalty Estimates by Sex")



### -------------------- ###
## Fit a model for both census years ##

joined_df <- bind_rows(
  overall_df_01 |> mutate(year = 2001),
  overall_df |> mutate(year = 2011))



