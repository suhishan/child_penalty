library(tidyverse);
library(fixest)
library(MatchIt)
library(Matrix)
library(scales)

## -------------------------------------------------- ##
## List of all functions and their arguments and what they do.
## -------------------------------------------------- ##



## -------------------------------------------------- ##
## We get a big_df after cleaing the dataset and forking
## through different filtering specifications.
## -------------------------------------------------- ##
big_df <- read_rds("big_df.Rds")

## Multiple dataframes from forking the big_df through different matching specifications.

rule <- expand_grid(
  var1 = c("district", NA)
)

## ----- 2001 ----- ##
output_01 <- lapply(big_df, function(i) {
  matching_func_01(i, rule)
})

## ----- 2011 ----- ##

output_11 <- lapply(big_df, function(i) {
  matching_func_11(i, rule)
})

## -------------------------------------------------- ##
## Now, we have the dataframe ready for analysis
## -------------------------------------------------- ##

# Let's take one dataframe and see what the results are:

one_df <- output_01[[1]][[1]]
one_df_est <- calc_estimates(one_df)
one_df_pen <- calc_penalty(one_df_est)

## -------------------------------------------------- ##
## Brute Force all of this for all 8 combinations for both
## 2001 and 2011 datasets.
## -------------------------------------------------- ##

cp_estimates <- numeric(16)
cp_se <- numeric(16)
idx <- 1
for (i in 1:4){
  for (j in 1:2){
    df_01 <- output_01[[i]][[j]]
    df_11 <- output_11[[i]][[j]]

    temp_est_01 <- calc_estimates(df_01)
    temp_penalty_01 <- calc_penalty(temp_est_01)

    temp_est_11 <- calc_estimates(df_11)
    temp_penalty_11 <- calc_penalty(temp_est_11)
    
    cp_estimates[idx] <- temp_penalty_01[[1]][1]
    cp_se[idx] <- temp_penalty_01[[2]][1]

    cp_estimates[idx+8] <- temp_penalty_11[[1]][1]
    cp_se[idx + 8] <- temp_penalty_11[[2]][1]
    idx <- idx + 1
  }
}

filters <- c(
  "Relate & Ever Married & District", 
  "Relate & Ever Married","Relate & District", "Relate",
  "Ever Married & District", "Ever Married",
  "District", "No Filter"
)

## -------------------------------------------------- ##
## Overall Plot
## -------------------------------------------------- ##

NAME <- rep(filters, 2)

final_estimates <- tibble(
  cp_estimates = cp_estimates * 100,
  cp_se = cp_se * 100,
  year = factor(rep(c(2001, 2011), each = 8)),
  NAME
)

variation_plot <- final_estimates |> mutate(
  id = row_number(),
  lc = cp_estimates - 1.811 * cp_se, #93 % confidence interval
  uc = cp_estimates + 1.811 * cp_se, #93% Confidence interval.
) |> ggplot(aes(x = id))+
  scale_x_continuous(
    
  ) +
  geom_pointrange(aes(y = cp_estimates, ymin = lc, 
    ymax = uc, color = year), size = 0.8, shape = 16)+
  scale_color_viridis_d(
      option = "cividis", begin = 0.2, end = 0.8, 
      name = "Year"
    )+
  geom_text(aes(label = NAME, y = cp_estimates), vjust = -1)+
  geom_hline(yintercept = 0, linetype = 2)+
  coord_cartesian(ylim = c(-2, 10))+
  labs(
    title = "Difference in Child Penalty Estimates due to different specifications",
    subtitle = "93% confidence intervals (assuming normal errors)",
    y = "Child Penalty Estimates"
  )+
    scale_y_continuous(
    labels = scales::label_percent(scale = 1)
  ) +
  scale_x_continuous(
    expand = expansion(mult = 0.05),
    breaks = NULL,   
    name = NULL, label = NULL)+
  theme_classic()+
  theme(
    axis.ticks.x = element_blank()
  )+
  coord_flip()

variation_plot

# ---------- Save the image ---------- ##
ggsave(
  "./blog/images/og/variation_plot.png",
  variation_plot,
  bg = "white", dpi = 300, units = "cm", height = 15, width = 21.5
)

# ---------- Specific Plots ---------- ##

## ----- 2001: Relate & Ever Married & District ----- ##

est_11_red <- calc_estimates(output_11[[1]][[1]])

ggsave(
  "./blog/images/og/penalty_est_11_red.png", 
  plot_penalty(est_11_red, 
    "Child Penalty Estimates 2011",
    "Conditional on Ever Married, Person related to head of househod & matching on districts"
  ), 
  bg = "white", dpi = 300, units = "cm", height = 14, width = 20)


