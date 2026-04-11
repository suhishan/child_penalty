# Women
long_neg <- treatment_0 |> 
    select(treatment, t, parent_id, employed, age, 
    subclass,weights) |> left_join(
        control_neg |> select(treatment, t, parent_id, employed, age, subclass, weights), by = "subclass",
        suffix = c("_t","_c"), relationship = "many-to-many"
    )

long_pos <- df_use_11 |> 
    filter(age_eldch %in% c(0:10)) |> 
    mutate(t = age_eldch, weight = 1) |> 
    select(age, employed, parent_id, t, weight)

model_df <- bind_rows(
    long_neg |> select(age = age_c, employed = employed_c, parent_id = parent_id_t, t = t_t, weight = weights_c),
    long_pos    
)

# Men
long_neg_m <- treatment_0_m |> 
    select(treatment_m, t_m, parent_id_m, employed_m, age_m, 
    subclass,weights) |> left_join(
        control_neg_m |> select(treatment_m, t_m, parent_id_m, employed_m, age_m, subclass, weights), by = "subclass",
        suffix = c("_t","_c"), relationship = "many-to-many"
    )

long_pos_m <- df_use_11_m |> 
    filter(age_eldch_m %in% c(0:10)) |> 
    mutate(t_m = age_eldch_m, weight = 1) |> 
    select(age_m, employed_m, parent_id_m, t_m, weight)

model_df_m <- bind_rows(
    long_neg_m |> select(age_m = age_m_c, employed_m = employed_m_c, parent_id_m = parent_id_m_t, t_m = t_m_t, weight = weights_c),
    long_pos_m   
)

# Overall Data Frame.
overall_df_long <- bind_rows(
  model_df |> mutate(sex = 1),
  model_df_m |> rename(
    age = age_m, employed = employed_m,
    parent_id = parent_id_m, t = t_m, weight = weight
  ) |> mutate(sex = 2)
)

write_rds(overall_df_long, "transformed_data/overall_df_long.Rds")
