# Load required libraries
library(tidyverse)
library(MatchIt)
# library(feols)

## -------------------------------------------------- ##
## NOTE: This is a flag for if you want to include   ##
## people's district of residence as a variable to ##
## match on for use-purpose later in the code . The ##
## outputs will be generated accordingly ##.

add_district <- FALSE
# add_district <- FALSE

## -------------------------------------------------- ##

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
df_use_11 <- df_use |> filter(year == 2011 & (is.na(age_eldch) | age_eldch <= 10))
df_use_11_m <- df_use_m |> filter(year_m == 2011 & (is.na(age_eldch_m) | age_eldch_m <= 10)) # Men

# Matching:
# Separate the datasets into year 0 parents and children.

# Women
# This is the group of 2011 birthers that is the basis of comparison.
parents0 <- df_use_11 |> filter(child0 == 1) |> mutate(parent0_id = row_number())
# This is the group that will be used as counterfactuals for negative event times.
childless <- df_use_11 |> filter(childless == 1) |> mutate(childless_id = row_number())

# Men
# This is the group of 2011 birthers men that is going to be the basis of our comparison.
parents0_m <- df_use_11_m |> filter(child0_m == 1) |> mutate(parent0_id_m = row_number())
# This is the group that will be used as counterfactuals for negative event times. 
childless_m <- df_use_11_m |> filter(childless_m == 1) |> mutate(childless_id_m = row_number())

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


## -------------------------------------------------- ##
## Matching Algorithm/function ##
## -------------------------------------------------- ##

match_function <- function(add_district) {
    if (add_district) {
        m_exact <- matchit(
            treatment ~ match_age + br_ch + edu_levels + urban + district, # Urban/rural status compared to district matches a lot of variables.
            data = match_joined_df, method = "exact", normalize = FALSE
        )

        m_exact_m <- matchit(
            treatment_m ~ match_age_m + br_ch_m + edu_levels_m + urban_m + district_m,
            data = match_joined_df_m, method = "exact", normalize = FALSE
        )
    } else {
        m_exact <- matchit(
            treatment ~ match_age + br_ch + edu_levels + urban, # Urban/rural status compared to district matches a lot of variables.
            data = match_joined_df, method = "exact", normalize = FALSE
        )

        m_exact_m <- matchit(
            treatment_m ~ match_age_m + br_ch_m + edu_levels_m + urban_m,
            data = match_joined_df_m, method = "exact", normalize = FALSE
        )
    }

    m_df <- match_data(m_exact) # Women
    m_df_m <- match_data(m_exact_m) # Men

    return (list(m_df, m_df_m, m_exact, m_exact_m))

}

matched_df <- match_function(add_district)[[1]]
matched_df_m <- match_function(add_district)[[2]]


write_rds(matched_df, "transformed_data/matched_df_01.Rds")
write_rds(matched_df_m, "transformed_data/matched_df_m_01.Rds")

## Negative Event times Control Group after matching.
# Women
control_neg <- matched_df[matched_df$treatment == 0,]
treatment_0 <- matched_df[matched_df$treatment == 1,]

# Men
control_neg_m <- matched_df_m[matched_df_m$treatment_m == 0,]
treatment_0_m <- matched_df_m[matched_df_m$treatment_m == 1,]

## -------------------------------------------------- ##
## The Dirt Work of having a final dataframe for analysis.##
## -------------------------------------------------- ##

## -------------------------------------------------- ##
## Women
## -------------------------------------------------- ##

lookup_table <- tibble(
    subclass = unique(treatment_0$subclass)
)

lookup_table$t <- treatment_0$t[match(lookup_table$subclass, treatment_0$subclass)]
control_neg$t <- lookup_table$t[match(control_neg$subclass, lookup_table$subclass)]



long_pos <- df_use_11 |> filter(age_eldch %in% c(0:10)) |> 
  mutate(weights = 1, subclass = NA, t = age_eldch) |> 
  select(age, year, dob, employed, weights, subclass, t, parent_id)

model_df <- bind_rows(
    control_neg |> select(age, year, dob, employed, weights, subclass, t, parent_id),
    long_pos
)

## -------------------------------------------------- ##
## Men
## -------------------------------------------------- ##

lookup_table_m <- tibble(
    subclass = unique(treatment_0_m$subclass)
)

lookup_table_m$t_m <- treatment_0_m$t_m[match(lookup_table_m$subclass, treatment_0_m$subclass)]
control_neg_m$t_m <- lookup_table_m$t_m[match(control_neg_m$subclass, lookup_table_m$subclass)]



long_pos_m <- df_use_11_m |> filter(age_eldch_m %in% c(0:10)) |> 
  mutate(weights = 1, subclass = NA, t_m = age_eldch_m) |> 
  select(age_m, year_m, dob_m, employed_m, weights, subclass, t_m, parent_id_m)

model_df_m <- bind_rows(
    control_neg_m |> select(age_m, year_m, dob_m, employed_m, weights, subclass, t_m, parent_id_m),
    long_pos_m
)


## Overall dataframe:

overall_df <- bind_rows(
  model_df |> mutate(sex = 1),
  model_df_m |> rename(
    age = age_m, employed = employed_m, year = year_m, dob = dob_m,
    parent_id = parent_id_m, t = t_m,
  ) |> mutate(sex = 2)
)


if (add_district) {
    write_rds(overall_df, "transformed_data/overall_df_for_analysis_2011.Rds")
} else {
    write_rds(overall_df, "transformed_data/overall_df_for_analysis_2011_nodis.Rds")
}



### Matching Long
# source('scripts/matching_long.R')