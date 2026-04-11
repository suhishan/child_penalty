# Load required libraries
library(tidyverse)
library(MatchIt)
# library(feols)

## Load the cleaned and trimmed down sample
df_use <- read_rds("transformed_data/selected_f_ipums_use.Rds")
df_use_m <- read_rds("transformed_data/selected_m_ipums_use.Rds")

# Have a variable on a) whether a person is childless and b) whether their eldest child was born in the interview year.
df_use <- df_use |> mutate(
    childless = ifelse(is.na(age_eldch), 1, 0), # If a person is childless in the sample i.e. no own child in the household.
    child0 = ifelse(age_eldch == 0, 1, 0),
    parent_id = row_number() # If a person had a child in the given year.
)

df_use_m <- df_use_m |> mutate(
    childless_m = ifelse(is.na(age_eldch_m), 1, 0), # If a person is childless in the sample i.e. no own child in the household.
    child0_m = ifelse(age_eldch_m == 0, 1, 0),
    parent_id_m = row_number() # If a person had a child in the given year.
)

## Load only the 2011 sample and those with their eldest child <= 10 years old.
df_use_01 <- df_use |> filter(year == 2001 & (is.na(age_eldch) | age_eldch <= 10))
df_use_01_m <- df_use_m |> filter(year_m == 2001 & (is.na(age_eldch_m) | age_eldch_m <= 10)) # Men

# Matching:
# Separate the datasets into year 0 parents and children.

# Women
# This is the group of 2011 birthers that is the basis of comparison.
parents0 <- df_use_01 |> filter(child0 == 1) |> mutate(parent0_id = row_number())
# This is the group that will be used as counterfactuals for negative event times.
childless <- df_use_01 |> filter(childless == 1) |> mutate(childless_id = row_number())

# Men
# This is the group of 2011 birthers men that is going to be the basis of our comparison.
parents0_m <- df_use_01_m |> filter(child0_m == 1) |> mutate(parent0_id_m = row_number())
# This is the group that will be used as counterfactuals for negative event times. 
childless_m <- df_use_01_m |> filter(childless_m == 1) |> mutate(childless_id_m = row_number())

# For the parent dataset, for every parent, have their timeline from -1 to -5
event_times <- -5:-1
match_parents <- parents0 |> 
  slice(rep(1:n(), each = length(event_times))) |> 
  group_by(parent0_id) |> 
    mutate(
        t = event_times, 
        age_at_t = age - abs(t),
        treatment = 1,
    ) |> mutate(match_age = age_at_t) |> ungroup()

match_childless <- childless |> mutate(match_age = age, treatment = 0)

# Men
match_parents_m <- parents0_m |> 
  slice(rep(1:n(), each = length(event_times))) |> 
  group_by(parent0_id_m) |> 
    mutate(
        t_m = event_times, 
        age_at_t_m = age_m - abs(t_m),
        treatment_m = 1,
    ) |> mutate(match_age_m = age_at_t_m) |> ungroup()

match_childless_m <- childless_m |> mutate(match_age_m = age_m, treatment_m = 0)

match_joined_df <- bind_rows(
    match_parents |> select(-age_at_t),
    match_childless
) |> filter(!is.na(edu_levels)) # BIG FLAG: I AM REMOVING 1000 or so values with NA EDUCATION

match_joined_df_m <- bind_rows(
    match_parents_m |> select(-age_at_t_m),
    match_childless_m
) |> filter(!is.na(edu_levels_m)) # BIG FLAG: Some people with NA EDUCATION removed.

## Matching Algorithm
m_exact <- matchit(
    treatment ~ match_age + br_ch + edu_levels + urban + district, # Urban/rural status compared to district matches a lot of variables.
    data = match_joined_df,
    method = "exact", normalize = FALSE
)

m_exact_m <- matchit(
    treatment_m ~ match_age_m + br_ch_m + edu_levels_m + urban_m + district_m,
    data = match_joined_df_m, method = "exact", normalize = FALSE
)

## Dataset from the matching algorithm
matched_df <- match_data(m_exact) # Women
matched_df_m <- match_data(m_exact_m) # Men

write_rds(matched_df, "transformed_data/matched_df_01.Rds")
write_rds(matched_df_m, "transformed_data/matched_df_m_01.Rds")

## Negative Event times Control Group after matching.
# Women
control_neg <- matched_df[matched_df$treatment == 0,]
treatment_0 <- matched_df[matched_df$treatment == 1,]

# Men
control_neg_m <- matched_df_m[matched_df_m$treatment_m == 0,]
treatment_0_m <- matched_df_m[matched_df_m$treatment_m == 1,]

## Two Branching Paths:
# Run Matching Collapsed if you want a collapsed dataset. 
# Run Matching Long if you want a long-form many-to-many dataset.
# (Comment out the one you don't need for a given run)

### Matching Collapsed
source('scripts/matching_collapsed_2001.R')

### Matching Long
# source('scripts/matching_long.R')