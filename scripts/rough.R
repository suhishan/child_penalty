# Playing around to see the sample size.
x <- df_f |> filter(
  age %in% c(15:45) & 
  bio_mom == 1 &
  (is.na(age_fc) | age_fc %in% c(20:45)) & 
  year == 2011# if no child or if first child birthed between 20 and 49.
) |> filter(is.na(age_eldch) | age_eldch %in% c(0:10))

# Unmarried sample
x_unm <- x |> filter(ever_married == 0)
table(x_unm$age_eldch, useNA = "always")

# Let's look at the original dataset.
ip_f |> filter(MARST == 1) |> group_by(ELDCH) |> count() |> print(n = 1000)

df_f |> filter(
  ever_married == 0
) |> group_by(age_eldch) |> count


## Let's see what summarzing for every subclass in
## control neg looks like:
library(tidyverse)

control_neg |> group_by(subclass) |> 
  summarize(
   employed = weighted.mean(employed, weights)
  )

## Merging Merging.

treatment_0 |> left_join(control_neg, by = "subclass",
suffix = c('_t','_c')) |> View()

### Synthetic data to think about what we are doing here.
N <- 1e4
synthetic <- tibble(
  t = rep(seq(-5, 10, by = 1), N),
  outcome = rbinom(N * 16, 1, prob = 
    ifelse(t < 0, 0.6, 0.35))
)

synthetic$t <- factor(synthetic$t)
synthetic$t <- relevel(synthetic$t, ref = "-1")

syn_m <- lm(outcome ~ t, data = synthetic)
summary(syn_m)



### Binding rows.

long_neg |> select(age, employed = employed_c, parent_id, t)


long_pos <- df_use_11 |> 
  filter(age_eldch %in% c(0:10)) |> 
  mutate(t = age_eldch) |> 
  select(age, employed, parent_id, t)

model_df$t <- factor(model_df$t)
model_df$t <- relevel(model_df$t, ref = '-2')

m <- lm(employed ~ t + factor(age), data = model_df); summary(m)


tibble(
  age_m, year_m, dob_m, pernum_m, ever_married_m, age_fm_m,
  age_eldch_m, age_fc_m, year_fc_m, employed_m, months_worked_m, edu_levels_m, 
  br_ch_m, hindu_m, district_m, urban_m
)



model_df_m$t_m <- factor(model_df_m$t_m)
model_df_m$t_m <- relevel(model_df_m$t_m, ref = "-2")

m2 <- lm(employed_m ~ t_m + factor(age_m) , data = model_df_m); summary(m2)


## Overall dataframe

bind_rows(
  model_df |> mutate(sex = 1),
  model_df_m |> rename(
    age = age_m, employed = employed_m,
    parent_id = parent_id_m, t = t_m
  ) |> mutate(sex = 2)
)


## Rough Analysis:


model_df$t <- factor(model_df$t)
model_df$t <- relevel(model_df$t, ref = '-2')

m <- lm(employed ~ t + factor(age), data = model_df, weights = model_df$weight); summary(m)

# Men
model_df_m$t_m <- factor(model_df_m$t_m)
model_df_m$t_m <- relevel(model_df_m$t_m, ref = '-2')

m2 <- lm(employed_m ~ t_m + factor(age_m), data = model_df_m, weights = model_df_m$weight); summary(m2)



#### Different matching ####

nrow(df_use)
nrow(df_use_01)

m1 <- match_function(add_district)[[3]]
summary(m1)



a <- tibble(
  subclass = rep(c(1, 2, 3, 4, 7, 13), times = 3),
  t = rep(c(-5, 2, -5, 0, 1, 2), times = 3)
)

lookup <- tibble(
  subclass = unique(a$subclass)
)
lookup$t = a$t[match(lookup$subclass, a$subclass)]


df <- tibble(
  subclass = sample(c(1, 2, 3, 4, 7, 13), size = 100, replace = T),
  t = NA
)

df$t <- lookup$t[match(df$subclass, lookup$subclass)]



### feols regression ###


nrow(overall_df)



### Playing around with the regression


d_f <- overall_df[overall_df$sex == 1,]
  d_f$t <- factor(d_f$t)
  d_f$t <- relevel(d_f$t, ref = "-2")

  # Men
  d_m <- overall_df[overall_df$sex == 2,]
  d_m$t <- factor(d_m$t)
  d_m$t <- relevel(d_m$t, ref = "-2")

  ## Women
  # Overall Model (with event time dummies)
  mf.1 <- feols(
      employed ~ t | factor(age) +factor(year),
      weights = d_f$weights,
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
    summarize(weighted.mean(predict.f2, weights))

  # Build estimates tibble for women
  estimates <- tibble(
      t = seq(-5, 10, by = 1),
      coefs.f1,
      se.f1,
      denominator.f,
  ) |> mutate(penalty.f = coefs.f1 / denominator.f)  # Fixed: was denominator (undefined)

  ## Men
  mm.1 <- feols(
      employed ~ t | factor(age) + factor(year),
      weights = d_m$weights,
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
    summarize(weighted.mean(predict.m2, weights)) |> pull()
  # Append men's estimates to the tibble
  estimates <- estimates |> mutate(
      coefs.m1,
      se.m1,
      denominator.m
  ) |> mutate(
      penalty.m = coefs.m1 / denominator.m
  )


estimates$coefs.m1










## Playing around with the matching.

me_f <- match_function(add_district)[[3]]
me_m <- match_function(add_district)[[4]]

df_use_f |> group_by(ever_married) |> 
  summarise(
    sum(is.na(age_eldch)),
    sum(!is.na(age_eldch))
  )



df_use_m |> group_by(ever_married_m) |> 
  summarise(
    sum(is.na(age_eldch_m)),
    sum(!is.na(age_eldch_m))
  )

df_use_m |> filter(ever_married_m == 1) |> 
  group_by(age_eldch_m) |> count()

table(df_use_m$age_eldch_m[df_use_m$ever_married_m == 1], useNA = "always")

#Relationship to the head.

table(ip_f$RELATE)

## Estimates are so inflated.

overall_df_nodis_01 |> group_by(t) |> 
  summarize(
    sum(weights), sum(perwt), sum(weights_total),
    weighted.mean(employed, weights_total)
  )


summary(lm(employed ~ relevel(factor(t), ref = "-2") + factor(age),
 data = overall_df_nodis_01), 
 weights = overall_df_nodis_01$weights_total)


## Detrending.

joined_df_nodis$t_num <- as.numeric(joined_df_nodis$t)

joined_df_nodis_f <- joined_df_nodis[joined_df_nodis$sex == 1,]

pre_data_f <- joined_df_nodis_f[joined_df_nodis_f$t_num < -1, ]

# Regression 

pre_reg_f <- feols(
  employed ~ t_num | factor(age) + factor(year),
  weights = ~weights_total,
  vcov = "hetero",
  data = pre_data_f
)

beta_time  <- coef(pre_reg_f)["t_num"]

joined_df_nodis_f$employed_detrended <- joined_df_nodis_f$employed - beta_time * joined_df_nodis_f$t_num

joined_df_nodis_f$t <- factor(joined_df_nodis_f$t)
joined_df_nodis_f$t <- relevel(joined_df_nodis_f$t, ref = "-2")

model <- feols(
  employed_detrended ~ t | factor(age) + factor(year),
  weights = ~weights_total,
  vcov = "hetero",
  data = joined_df_nodis_f
)
