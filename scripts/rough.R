## Let's see what summarzing for every subclass in
## control neg looks like:

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


