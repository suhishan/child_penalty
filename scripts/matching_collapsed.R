# Group the control_neg by subclass and find the weighted mean of our outcome `employed`.
control_neg <- control_neg |> group_by(subclass) |> 
  summarize(
   outcome = mean(employed),
   outcome_int = sum(employed),
   count = n(),
   weights = weights[1]
  )
# Merge this with the treatment_0 parents by `subclass`
long_neg <- treatment_0 |> 
    left_join(control_neg, by = "subclass", suffix = c('_t','_c'))

# 0 and positive event times.
# Women
long_pos <- df_use_11 |> 
  filter(age_eldch %in% c(0:10)) |> 
  mutate(t = age_eldch, count = 1, outcome_int = employed, weights = 1) |> 
  select(age, outcome = employed, outcome_int, parent_id, t, count, weights)


# The dataframe we will need to use if we are analyzing only for women.
model_df <- bind_rows(
    long_neg |> select(age, outcome, outcome_int, parent_id, t, count, weights = weights_c),
    long_pos
)


## Men

control_neg_m <- control_neg_m |> group_by(subclass) |> 
    summarize(
        outcome_m = mean(employed_m),
        outcome_m_int = sum(employed_m),
        count_m = n(),
        weights_m = weights[1],
    )

long_neg_m <- treatment_0_m |> left_join(control_neg_m, by = "subclass", suffix = c('_t','_c'))

long_pos_m <- df_use_11_m |> 
    filter(age_eldch_m %in% c(0:10)) |> 
    mutate(t_m = age_eldch_m, count_m = 1, outcome_m_int = employed_m, weights_m = 1) |> 
    select(age_m, outcome_m = employed_m, outcome_m_int, parent_id_m, t_m, count_m, weights_m)

# The dataframe one needs to use if we are analyzing only for men.
model_df_m <- bind_rows(
    long_neg_m |> select(age_m, outcome_m, outcome_m_int, parent_id_m, t_m, count_m, weights_m),
    long_pos_m
)

## Overall dataframe:

overall_df <- bind_rows(
  model_df |> mutate(sex = 1),
  model_df_m |> rename(
    age = age_m, outcome = outcome_m, outcome_int = outcome_m_int,
    parent_id = parent_id_m, t = t_m, count = count_m, weights = weights_m
  ) |> mutate(sex = 2)
)

write_rds(overall_df, "transformed_data/overall_df_for_analysis.Rds")
