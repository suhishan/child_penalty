# Load required libraries
library(tidyverse)
library(MatchIt)
# library(feols)

## -------------------------------------------------- ##
## NOTE: This is a flag for if you want to include   ##
## people's district of residence as a variable to ##
## match on for use-purpose later in the code . The ##
## outputs will be generated accordingly ##.

## -------------------------------------------------- ##
df <- read_rds("big_df.Rds")
## Load the cleaned and trimmed down sample

# ---------- The variables (and rules) for matching specifications ---------- #

rule <- expand_grid(
  var1 = c("district", NA)
)

df_use <- df[[1]]

#matching_func_01 <- function(df, rule){
          # REmember to change this to df
df_use <- df_use |> mutate(
    childless = ifelse(is.na(age_eldch), 1, 0), # If a person is childless in the sample 
    child0 = ifelse(age_eldch == 0 & !is.na(age_eldch), 1, 0),
    parent_id = row_number() # If a person had a child in the given year.
)

## Load only the 2001 sample 

df_use_01 <- df_use |> filter(year == 2001)

## ----------------------------------------------------------- ##
## Get the dataframe ready for matching 
## -------------------------------------------------- ##

# Separate the datasets into year 0 parents and children.

# This is the group of 2011 birthers that is the basis of comparison.
parents0 <- df_use_01 |> filter(child0 == 1) |> mutate(parent0_id = row_number())

# This is the group that will be used as counterfactuals for negative event times.
childless <- df_use_01 |> filter(childless == 1) |> mutate(childless_id = row_number())



# ---------------------------------------------------------------------------------------#

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


# ----- Join the childless and parents at 0 dataset. ----- #

match_joined_df <- bind_rows(
    match_parents |> select(-age_at_t),
    match_childless
) 

## -------------------------------------------------- ##
## Matching Algorithm/function ##
## -------------------------------------------------- ##

#x <- lapply(seq_len(nrow(rule)), function(i){

  # ---------- Determine the matching formula using the matching variables provided.

  vars <- as.character(
      unlist(rule[i, 1:ncol(rule)])
  )
  vars <- vars[!is.na(vars)]
  vars <- rbind("district", "match_age", "edu_levels", "urban", "ever_married")
  my_formula <- reformulate(vars, response = "treatment")

  # ---------- The Usual Stuff now. ---------- #
  #for (s in sexes) {
    joined_s <- match_joined_df |> filter(sex == 2)
    df_s     <- df_use_01 |> filter(sex == 2)

    # Exact Matching (separate for each sex)
    m_exact <- matchit(
      formula = my_formula, 
      data = joined_s, method = "exact", normalize = FALSE
    )
    m_df <- match_data(m_exact)

    # Post-Matching Logic (identical to your original code)
    control_neg  <- m_df |> filter(treatment == 0)
    treatment_0  <- m_df |> filter(treatment == 1)

    lookup_table <- tibble(subclass = unique(treatment_0$subclass))
    lookup_table$t <- treatment_0$t[match(lookup_table$subclass, treatment_0$subclass)]
    control_neg$t  <- lookup_table$t[match(control_neg$subclass, lookup_table$subclass)]

    long_pos <- df_s |> filter(age_eldch %in% c(0:10)) |> 
      mutate(weights = 1, subclass = NA, t = age_eldch) |> 
      select(age, year, sex,dob, employed, weights, subclass, t, parent_id, perwt)

    model_list[[s]] <- bind_rows(
      control_neg |> select(age, year, sex, dob, employed, weights, subclass, t, parent_id, perwt),
      long_pos
    ) 
  #}

  bind_rows(model_list)

#})

x1 <- x[[1]]
x1 <- x1 |> mutate(
  weights_total = weights * perwt
)
x1 |> filter(sex == 2) |> 
  group_by(t) |> summarize(
  n(), sum(weights), sum(weights_total),
  weighted.mean(employed, weights)
)

#return (district_list)
#

## ---------- Original Df Troubleshoot ---------- #

df_use_01 |> filter(sex == 2) |> group_by(age) |> 
  summarize(mean(employed)) |> print(n = 100)
