## ---- setup ----
library(tidyverse)
library(readstata13)

# Helper for printing block headers
hrule <- function(title) {
  cat("\n-------------------------------\n")
  cat(title, "\n")
  cat("-------------------------------\n\n")
}

# path to raw person file
persons <- read.dta13("data/CFPS_2022/ecfps2022person_202410.dta")

hrule("Initial raw CFPS dataset")
cat("Raw rows:", nrow(persons), "\n")

# helper to turn CFPS special codes into NA
fix_missing <- function(x, extra_bad = NULL) {
  bad <- c(-1, -2, -3, -8, -9, -10)
  if (!is.null(extra_bad)) bad <- union(bad, extra_bad)
  x[x %in% bad] <- NA
  x
}

## ---- recode key variables ----
dat <- persons %>%
  transmute(
    # basic IDs & geography
    person_id     = pid,
    family_id     = fid22,
    community_id  = cid22,
    county_id     = countyid22,
    province_id   = provcd22,
    
    # core demographics
    age           = fix_missing(age),
    gender_raw    = fix_missing(gender),
    ethnicity_raw = fix_missing(minzu),
    hukou_raw     = fix_missing(huk),
    urban_raw     = fix_missing(urban22),
    
    # household / family
    num_children_u16 = fix_missing(child16n),
    family_size      = fix_missing(fml_count),
    
    # education
    edu_last_raw  = fix_missing(edu_last),
    edu_update    = fix_missing(edu_update),
    
    # labor / retirement
    retired_raw   = fix_missing(retire),
    pension_raw   = fix_missing(pension),
    
    # politics
    party_member_raw = fix_missing(party),
    
    # smoking & handedness
    smoke_age     = fix_missing(smokeage),
    handedness    = fix_missing(hand),
    
    # cognitive-ish
    wordlist_raw  = fix_missing(wordlist),
    mathlist_raw  = fix_missing(mathlist),
    
    # interview timing
    interview_year  = fix_missing(cyear),
    interview_month = fix_missing(cmonth),
    
    # employment status
    employ_raw    = fix_missing(employ),
    
    # SES variables
    qg401         = qg401,
    qn8011        = qn8011,
    qn8012        = qn8012,
    
    # depression
    cesd8_raw     = cesd8
  )

hrule("After recoding key variables")
cat("Rows:", nrow(dat), "\n")
dat %>%
  summarise(
    missing_age = sum(is.na(age)),
    missing_gender = sum(is.na(gender_raw)),
    missing_cesd8 = sum(is.na(cesd8_raw))
  ) %>% print()

## ---- clean SES + depression ----
dat <- dat %>%
  mutate(
    income_satis = fix_missing(qg401),
    income_rel   = fix_missing(qn8011, extra_bad = 79),
    status_rel   = fix_missing(qn8012),
    cesd8 = ifelse(!is.na(cesd8_raw) & cesd8_raw > -8, cesd8_raw - 8, NA_real_)
  )

hrule("After SES + CESD8 cleaning")
cat("Rows:", nrow(dat), "\n")
dat %>%
  summarise(
    missing_income_satis = sum(is.na(income_satis)),
    missing_income_rel   = sum(is.na(income_rel)),
    missing_status_rel   = sum(is.na(status_rel)),
    missing_cesd8_clean  = sum(is.na(cesd8))
  ) %>% print()

## ---- clean categorical variables ----
dat <- dat %>%
  mutate(
    gender = case_when(
      gender_raw == 1 ~ "male",
      gender_raw == 2 ~ "female",
      TRUE ~ NA_character_
    ),
    ethnicity_han = ifelse(ethnicity_raw == 1, 1, 0),
    hukou_rural   = ifelse(hukou_raw == 1, 1, 0),
    urban = case_when(
      urban_raw == 1 ~ 1,
      urban_raw == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    party_member = case_when(
      party_member_raw == 1 ~ 1,
      party_member_raw == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    retired = case_when(
      retired_raw == 1 ~ 1,
      retired_raw == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    pension = case_when(
      pension_raw == 1 ~ 1,
      pension_raw == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    employ_status = case_when(
      employ_raw == 1 ~ "employed",
      employ_raw == 0 ~ "unemployed",
      employ_raw == 3 ~ "out_of_labor_force",
      TRUE ~ NA_character_
    )
  )

hrule("After categorical recoding")
cat("Rows:", nrow(dat), "\n")
dat %>%
  summarise(
    missing_gender = sum(is.na(gender)),
    missing_employ = sum(is.na(employ_status))
  ) %>% print()

## ---- fix structural NAs ----
dat <- dat %>%
  mutate(
    party_member = ifelse(is.na(party_member), 0, party_member),
    retired = ifelse(is.na(retired), 0, retired),
    pension = ifelse(is.na(pension), 0, pension),
    gender = ifelse(is.na(gender) & !is.na(gender_raw), "female", gender),
    handedness = ifelse(is.na(handedness), "right", handedness),
    smoke_age = ifelse(is.na(smoke_age), 0, smoke_age)
  )

hrule("After fixing structural NAs")
cat("Rows:", nrow(dat), "\n")

## ---- drop invalid IDs ----
dat <- dat %>%
  filter(
    person_id > 0,
    family_id > 0,
    community_id > 0,
    county_id > 0,
    province_id > 0
  )

hrule("After dropping invalid IDs")
cat("Rows:", nrow(dat), "\n")
dat %>%
  summarise(
    families = n_distinct(family_id),
    communities = n_distinct(community_id),
    counties = n_distinct(county_id),
    provinces = n_distinct(province_id)
  ) %>% print()

## ---- restrict to analytic sample ----
clean_dat <- dat %>%
  filter(
    !is.na(cesd8),
    !is.na(income_satis),
    !is.na(income_rel),
    !is.na(status_rel),
    !is.na(age),
    age >= 18
  )

hrule("After restricting to analytic sample")
cat("Rows:", nrow(clean_dat), "\n")
clean_dat %>%
  summarise(
    families = n_distinct(family_id),
    communities = n_distinct(community_id),
    counties = n_distinct(county_id),
    provinces = n_distinct(province_id)
  ) %>% print()

## ---- write data ----
write.csv(clean_dat,
          "data/cfps2022_clean_persons.csv",
          row.names = FALSE)

