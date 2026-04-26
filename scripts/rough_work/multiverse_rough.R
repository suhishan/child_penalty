# Load libraries.
library(tidyverse); library(MatchIt);
library(rlang)
# Create a dataframe for filtering and matching.
N <- 1e3

df <- tibble(
  sex = rbinom(N, 1, 0.5), # 1 is female
  age = as.integer(runif(N, 15, 49)),
  ever_child = rbinom(N, 1, 0.7),
  age_eldch = ifelse(ever_child == 1, 
    as.integer(runif(N, 0, 10)), NA),
  hindu = rbinom(N, 1, 0.8),
  brch = rbinom(N, 1, 0.4),
  ever_married = rbinom(N, 1, 0.85),
  urbrur = rbinom(N, 1, 0.4), # 40% live in urban areas.
  employed = rbinom(N, 1, 0.87)
)


## Select for age 15 to 45 and only those who have ever married.
## number of possible selections 2 * 2

# Match for urbrur, yes or no, hindu, yes or no.
# 2 * 2 * 2 * 2 = 16 possible combinations of selection and matching.


## Ideally have a list of all combinations and for every row, conduct the analysis.

rules <- expand_grid(
  age_filter = c(TRUE, FALSE),
  em_filter = c(TRUE, FALSE)
) |> mutate(
  age_eval = ifelse(age_filter, "age %in% c(15:45)", NA),
  em_eval = ifelse(em_filter, "ever_married == 1", NA)
)


run_specs <- function(df, rules) {
  list <- lapply(seq_len(nrow(rules)), function(i) {
    r <- rules[i, ]
    exprs <- c(r$age_eval, r$em_eval)
    exprs <- exprs[!is.na(exprs)]
    
    if (length(exprs) == 0) return(df)
    df |> filter(!!parse_expr(paste(exprs, collapse = " & ")))
  })

  names(list) <- c("a", "b", "c", "d")
  return (list)
}

results <- run_specs(df, rules)

rules_2 <- 

x <- function(df, rules){

}

mean(df$employed[df$urbrur == 1 & df$hindu == 1])
with(df, mean(employed[NULL == 1 & hindu == 1]))

rules_2 <- expand_grid(
  var1 = c("urbrur", NA),
  var2 = c("hindu", NA),
  var3 = c("brch",NA)
)

x <- function(df, rule){
  m_list <- lapply(seq_len(nrow(rule)),function(i) {
    vars <- as.character(
      unlist(rule[i, 1:3])
    )
    vars <- vars[!is.na(vars)]

    vars <- rbind(vars, "ever_married", "ever_child")
    formula <-  reformulate(vars, response = "employed")
    m <- matchit(
      formula,
      data = df, method = "exact"
    )
    
    match_data(m)
  })

  return(m_list)

}
