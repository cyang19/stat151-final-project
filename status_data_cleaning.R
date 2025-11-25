## ---- setup ----
library(tidyverse)
library(readstata13)

# path to raw person file
persons <- read.dta13("data/CFPS_2022/ecfps2022person_202410.dta")

# helper to turn CFPS special codes into NA
fix_missing <- function(x, extra_bad = NULL) {
  bad <- c(-1, -2, -3, -8, -9, -10)
  if (!is.null(extra_bad)) bad <- union(bad, extra_bad)
  x[x %in% bad] <- NA
  x
}

## ---- recode key variables ----
dat <- persons %>%
  # basic IDs & geography
  transmute(
    person_id     = pid,
    family_id     = fid22,
    # you still have older family IDs (fid20,...), but we stick to 2022
    community_id  = cid22,
    county_id     = countyid22,
    province_id   = provcd22,
    
    # survey weights
    wt_cross      = rswt_natcs22n,
    wt_panel      = rswt_natpn1022n,
    
    # core demographics
    age           = fix_missing(age),
    gender_raw    = fix_missing(gender),
    ethnicity     = fix_missing(minzu),
    hukou         = fix_missing(huk),
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
    
    # cognitive-ish measures
    wordlist_raw  = fix_missing(wordlist),
    mathlist_raw  = fix_missing(mathlist),
    
    # subjective SES (home environment score)
    ses14_raw     = fix_missing(ses14),
    
    # interview timing
    interview_year  = fix_missing(cyear),
    interview_month = fix_missing(cmonth),
    
    # employment status (EMPLOY variable in raw data)
    employ_raw    = fix_missing(employ),
    
    # SES variables of interest
    qg401         = qg401,   # income satisfaction
    qn8011        = qn8011,  # relative income
    qn8012        = qn8012,  # relative status
    
    # depression (CES-D8) - should be in persons file
    cesd8_raw     = cesd8
  )

## ---- clean SES + depression ----

dat <- dat %>%
  mutate(
    # recode SES missing / “non-substantive” responses
    income_satis = fix_missing(qg401),                      # 1–5
    income_rel   = fix_missing(qn8011, extra_bad = 79),     # 1–5, 79 = "not sure"
    status_rel   = fix_missing(qn8012),                     # 1–5
    
    # clean CES-D8: CFPS stores it shifted; valid if > -8
    cesd8 = ifelse(!is.na(cesd8_raw) & cesd8_raw > -8,
                   cesd8_raw - 8, NA_real_)
  )

## ---- clean/label categorical variables ----

dat <- dat %>%
  mutate(
    # gender: CFPS usually 1 = male, 2 = female
    gender = case_when(
      gender_raw == 1 ~ "male",
      gender_raw == 2 ~ "female",
      TRUE ~ NA_character_
    ),
    
    # urban: 1 urban, 0 rural (check documentation if needed)
    urban = case_when(
      urban_raw == 1 ~ 1,
      urban_raw == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # party membership
    party_member = case_when(
      party_member_raw == 1 ~ 1,  # Communist Party member
      party_member_raw == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # retired indicator
    retired = case_when(
      retired_raw == 1 ~ 1,
      retired_raw == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # pension beneficiary indicator
    pension = case_when(
      pension_raw == 1 ~ 1,
      pension_raw == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # employment status
    # EMPLOY: 1 = employed, 0 = unemployed, 3 = out of labor force, negatives = not applicable / missing
    employ_status = case_when(
      employ_raw == 1 ~ "employed",
      employ_raw == 0 ~ "unemployed",
      employ_raw == 3 ~ "out_of_labor_force",
      TRUE ~ NA_character_
    )
  )

## ---- restrict to analytic sample ----
# For now: 
#  - valid depression score
#  - valid SES measures
#  - age >= 18 (adult sample)
clean_dat <- dat %>%
  filter(
    !is.na(cesd8),
    !is.na(income_satis),
    !is.na(income_rel),
    !is.na(status_rel),
    !is.na(age),
    age >= 18
  )

## ---- optional: select final columns & rename nicely ----
clean_dat <- clean_dat %>%
  select(
    # IDs & geography
    person_id, family_id, community_id, county_id, province_id,
    
    # weights
    wt_cross, wt_panel,
    
    # outcome
    cesd8,
    
    # core SES predictors
    income_satis, income_rel, status_rel,
    
    # demographics
    age, gender, ethnicity, hukou, urban,
    party_member, retired, pension,
    
    # family / household
    num_children_u16, family_size,
    
    # cognition / baseline SES
    edu_last_raw, edu_update,
    wordlist_raw, mathlist_raw, ses14_raw,
    
    # employment
    employ_status,
    
    # smoking, handedness
    smoke_age, handedness,
    
    # timing
    interview_year, interview_month
  )

## ---- write out cleaned data ----
write.csv(clean_dat,
          "data/CFPS_2022/cfps2022_clean_persons.csv",
          row.names = FALSE)

# Quick sanity check
nrow(clean_dat)
summary(clean_dat$cesd8)
summary(clean_dat$income_satis)
summary(clean_dat$income_rel)
summary(clean_dat$status_rel)
