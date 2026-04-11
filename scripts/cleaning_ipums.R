# Load required libraries
library(tidyverse)
library(rethinking)

# Storing in RDS Format for later use.
# ip <- read.csv('raw_data/ipumsi_00003.csv')
# write_rds(ip, 'transformed_data/ipums.Rds')

ip <- readRDS('transformed_data/ipums.Rds')

# Put all the required variables after they've been cleaned here:

## Separate the dataset by male and female ##
ip_m <- subset(ip, SEX == 1)
ip_f <- subset(ip, SEX == 2)

## Age
# Checking for discrepancies.
sum(is.na(ip_f$AGE)) # No NAS.
sum(is.na(ip_m$AGE)) # No NAS.

age <- ip_f$AGE
age_m <- ip_m$AGE

## Year of Census
# Checking for discrepancies.
sum(is.na(ip_f$YEAR)) # No NAS.
sum(is.na(ip_m$YEAR)) # No NAS

year <- ip_f$YEAR
year_m <- ip_m$YEAR

## Date of Birth
dob <- year - age # women
dob_m <- year_m - age_m # men

# Checking for discrepancies.
sum(is.na(dob))
sum(is.na(dob_m))

## Person Number
# Checking for discrepancies
sum(is.na(ip_f$PERNUM))
sum(is.na(ip_m$PERNUM))

pernum <- ip_f$PERNUM
pernum_m <- ip_m$PERNUM

## Age of the Oldest Child.
# Note: The child is linked to their father and mother via is the IPUMS family pointer i.e.
# for every row, this will the woman's own child. We probably only have information on married women aged 
# 15-49 on their children

age_eldch <- ifelse(ip_f$ELDCH == 99, NA, ip_f$ELDCH)
age_eldch_m <- ifelse(ip_m$ELDCH == 99, NA, ip_m$ELDCH)

# Checking for discrepancies.
sum(is.na(age_eldch)) # 1,555,050 NAs i.e. these women/girls did not have chidlren
sum(is.na(age_eldch_m)) # 1,675,043 NAs i.e. these men/boys did not have children.

## Age at first birth:
# Age at first birth = Age - Age of the eldest child.

age_fc <- age - age_eldch
age_fc_m <- age_m - age_eldch_m
## Some of the age at first birth values are absolutely improbable.
## We could call it measurement error.
## Later Filter out values only for STEPMOM == 0 and STEPPOP == 0

## Year of First Birth (for the parent):
year_fc <- year - age_eldch # Women
year_fc_m <- year_m - age_eldch_m # Men

# The NA counts are consistent across age_fc, age_eldch, year_fc ( and also for men _m)

## Marriage.
ip_f |> group_by(MARST) |> count()
ip_m |> group_by(MARST) |> count()

ever_married <- ifelse(ip_f$MARST %in% c(2:4), 1, 0) # Have you ever married or not? (Married, Divorced, Widowed)
age_fm <- ifelse(ip_f$AGEMARR == 99, NA, ip_f$AGEMARR) # Age at first marriage (Women), If every married: NA.

ever_married_m <- ifelse(ip_m$MARST %in% c(2:4), 1, 0) # Have you ever married or not (Married, Divorced, Widowed)
age_fm_m <- ifelse(ip_m$AGEMARR == 99, NA, ip_m$AGEMARR) # Age at first marriage (Men), If haven't married: NA.

## Work/Employment variables.
# Women
employed <- ifelse(ip_f$EMPSTAT == 1, 1, 0)
months_worked <- ifelse(ip_f$WRKMTHS %in% c(98, 99), NA, ip_f$WRKMTHS)

# Men
employed_m <- ifelse(ip_m$EMPSTAT == 1, 1, 0)
months_worked_m <- ifelse(ip_m$WRKMTHS %in% c(98, 99), NA, ip_m$WRKMTHS)

## Education:
# Checking for discrepancies.
ip_f |> group_by(EDATTAIN) |> count()
ip_m |> group_by(EDATTAIN) |> count()

edu_levels <- ifelse(ip_f$EDATTAIN == 9, NA, ip_f$EDATTAIN)
edu_levels_m <- ifelse(ip_m$EDATTAIN == 9, NA, ip_m$EDATTAIN)

## Ethnicity and Religion.
# Checking for discrepancies.
ip_f |> group_by(RELIGION) |> count() |> print(n = 100)
ip_m |> group_by(RELIGION) |> count() |> print(n = 100)

br_ch <- ifelse(ip_f$ETHNICNP %in% c(1, 2, 27), 1, 0 ) # Brahmin/Chhetri Indicator.
hindu <- ifelse(ip_f$RELIGION == 3, 1, 0) # Hindu Indicator

# Men
br_ch_m <- ifelse(ip_m$ETHNICNP %in% c(1, 2, 27), 1, 0 ) # Brahmin/Chhetri Indicator.
hindu_m <- ifelse(ip_m$RELIGION == 3, 1, 0) # Hindu Indicator

## Districts:
# 40 : Kaski and Manang Together
# 43 : Myagdi and Mustang Together
# 63 : Jumla and Kalikot Together
district <- as.numeric(substring(as.character(ip_f$GEO2_NP), 8, 9))
district_m <- as.numeric(substring(as.character(ip_m$GEO2_NP), 8, 9))

## Urban/Rural
urban <- ifelse(ip_f$URBAN == 2, 1, 0)
urban_m <- ifelse(ip_m$URBAN == 2, 1, 0)

## DataFrame vessel for all the required variables (female)
# Women
df_f <- tibble(
    age, year, dob, pernum, ever_married, age_fm,
    age_eldch, age_fc, year_fc, employed, months_worked, edu_levels, 
    br_ch, hindu, district, urban,
    bio_mom = ifelse(ip_f$STEPMOM == 0, 1, 0)
)

# Men
df_m <- tibble(
    age_m, year_m, dob_m, pernum_m, ever_married_m, age_fm_m,
    age_eldch_m, age_fc_m, year_fc_m, employed_m, months_worked_m, edu_levels_m, 
    br_ch_m, hindu_m, district_m, urban_m,
    bio_dad = ifelse(ip_m$STEPPOP == 0, 1, 0)
)


## Filtering out the sample.
# Three Criterias:
# 1. Ages 15 to 49 (and age at first birth between 20 and 49)
# 2. Married, divorced or widowed.
# 3. Biological Mothers only.

df_use <- df_f |> filter(
 age %in% c(15:49) & ever_married == 1 & bio_mom == 1 & 
    (is.na(age_fc) | age_fc %in% c(20:49)) # if no child or if first child birthed between 20 and 49.
)

df_use_m <- df_m |> filter(
    age_m %in% c(15:49) & ever_married_m == 1 & bio_dad == 1 & 
        (is.na(age_fc_m) | age_fc_m %in% c(20:49))
)

write_rds(df_use, "transformed_data/selected_f_ipums_use.Rds")
write_rds(df_use_m, "transformed_data/selected_m_ipums_use.Rds")