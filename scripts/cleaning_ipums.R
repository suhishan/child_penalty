# Load required libraries
library(tidyverse)
library(rethinking)
library(haven)
library(stargazer)

## ---------- Data ---------- ##

# Storing in RDS Format for later use.
# ip <- read.csv('raw_data/ipumsi_00003.csv')
# write_rds(ip, 'transformed_data/ipums.Rds')


ip <- readRDS('transformed_data/ipums.Rds')

## ----- Modularized function for selecting important variables.

select_ipums <- function(original_ipums) {

    ## ----- Sex Indicator ----- ##
    sex <- ifelse(original_ipums$SEX == 2, 1, 0) # 1 is female

    ## ----- Age & Year ----- ##
    age <- original_ipums$AGE
    year <- original_ipums$YEAR
    dob <- year - age

    ## ----- Person Number & Weights ----- ##
    pernum <- original_ipums$PERNUM
    perwt <- original_ipums$PERWT
    # hist(perwt, breaks = 20) # Optional: uncomment for visual debugging

    ## ----- Relationship & Family Pointers ----- ##

    relate <- original_ipums$RELATE

    # NOTE: ELDCH == 99 means no own child in the household
    age_eldch <- ifelse(original_ipums$ELDCH == 99, NA, original_ipums$ELDCH)

    ## ----- Age/Year at First Birth ----- ##
    # Age at first birth = Age - Age of the eldest child
    age_fc <- age - age_eldch
    year_fc <- year - age_eldch

    ## ----- Marriage ----- ##
    # MARST %in% c(1:4) indicates ever married (Married, Divorced, Widowed, etc.)
    ever_married <- ifelse(original_ipums$MARST %in% c(1:4), original_ipums$MARST, NA)
    age_fm <- ifelse(original_ipums$AGEMARR == 99, NA, original_ipums$AGEMARR)

    ## ----- Work and Employment ----- ##
    # EMPSTAT == 1 indicates employed
    employed <- ifelse(original_ipums$EMPSTAT == 1, 1, 0)
    months_worked <- ifelse(original_ipums$WRKMTHS %in% c(98, 99), NA, original_ipums$WRKMTHS)

    ## ----- Education ----- ##
    # EDATTAIN == 9 indicates missing/NIU
    edu_levels <- ifelse(original_ipums$EDATTAIN == 9, NA, original_ipums$EDATTAIN)

    ## ----- Ethnicity & Religion ----- ##
    br_ch <- ifelse(original_ipums$ETHNICNP %in% c(1, 2, 27), 1, 0) # Brahmin/Chhetri Indicator
    hindu <- ifelse(original_ipums$RELIGION == 3, 1, 0) # Hindu Indicator

    ## ----- Location & Urban/Rural ----- ##
    district <- as.numeric(substring(as.character(original_ipums$GEO2_NP), 8, 9))
    urban <- ifelse(original_ipums$URBAN == 2, 1, 0)

    ## ----- Biological Parent Indicators ----- ##
    bio_mom <- ifelse(original_ipums$STEPMOM == 0, 1, 0)
    bio_dad <- ifelse(original_ipums$STEPPOP == 0, 1, 0)

    ## ----- Combine into a single unified dataframe ----- ##
    df <- data.frame(
        sex, age, year, dob, pernum, perwt, relate,
        ever_married, age_fm,
        age_eldch, age_fc, year_fc,
        employed, months_worked, edu_levels,
        br_ch, hindu, district, urban,
        bio_mom, bio_dad
    )

    return(df)
}

df <- select_ipums(ip)


## ---------- Filtering out the sample ---------- ##
# Three Criterias:
# 1. Ages 15 to 45 (and age at first birth between 20 and 45)
# 2. Married, divorced or widowed.
# 3. Biological Mothers only.



filter_rules <- expand_grid(
    relate_filter = c(TRUE,FALSE),
    em_filter = c(TRUE, FALSE)
) |> mutate(
    relate_eval = ifelse(
        relate_filter, "relate %in% c(1, 2, 3)", NA
    ),
    em_eval = ifelse(
        em_filter, "ever_married %in% c(2, 3, 4)", NA
    )
)


filter_specs <- function(df, rules) {
    df <- df |> filter(
        #---------- Default Rules for filtering. ----------#
        age %in% c(15:45) & 
        !is.na(ever_married) &
        (is.na(age_fc) | age_fc %in% c(20:45)) &
        (is.na(age_eldch) | age_eldch %in% c(0:10)) &
        (is.na(edu_levels))
    )
        # ---------- Extra Rules ---------- #
    lapply(seq_len(nrow(rules)), function(i){
        r <- rules[i, ]
        exprs <- c(r$relate_eval, r$em_eval)
        exprs <- exprs[!is.na(exprs)]

        if(length(exprs) == 0) return (df)
        df |> filter(!!parse_expr(paste(exprs, collapse = " & ")))

    })
}

big_df <- filter_specs(df, filter_rules)

## ---------- Summary table (sample sizes) ---------- ##

lapply(big_df, function(i) {
    with(i, table(year, sex))
})
