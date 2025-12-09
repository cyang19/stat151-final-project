library(dplyr)
library(lme4)
library(lmerTest)

cfps <- read.csv("data/model_ready_data.csv",
                 stringsAsFactors = FALSE)

## Rescale per-capita COVID variables (x100,000)

per_capita_vars <- c(
  "new_confirmed_per_capita",
  "X3month_avg_new_confirmed_per_capita",
  "new_deceased_per_capita",
  "X3month_avg_new_deceased_per_capita",
  "cumulative_confirmed_per_capita",
  "X3month_avg_cumulative_confirmed_per_capita",
  "cumulative_deceased_per_capita",
  "X3month_avg_cumulative_deceased_per_capita"
)

# keep only those that exist in the data
per_capita_vars <- intersect(per_capita_vars, names(cfps))

cfps <- cfps %>%
  mutate(
    across(
      all_of(per_capita_vars),
      ~ . * 100000
    )
  )


cfps <- cfps %>%
  mutate(
    pid   = factor(pid),
    fid   = factor(fid),
    cid   = factor(cid),
    provcd = factor(provcd),
    wave  = factor(wave),
    
    ## group mean centering time varying covariates (example choices)
    age_c    = scale(age, center = TRUE, scale = FALSE)[, 1],
    health_c = scale(health, center = TRUE, scale = FALSE)[, 1],
    
    ## Time-invariant-ish covariates as factors
    gender    = factor(gender),
    education = factor(education),
    marital   = factor(marital),
    urban     = factor(urban)
  )

## variables to zero-fill in pre-COVID waves
pre_covid_zero_vars <- c(
  "new_confirmed",
  "X3month_avg_new_confirmed",
  "new_deceased",
  "X3month_avg_new_deceased",
  "cumulative_confirmed",
  "X3month_avg_cumulative_confirmed",
  "cumulative_deceased",
  "X3month_avg_cumulative_deceased",
  "StringencyIndex_Average",
  "X3month_avg_StringencyIndex_Average",
  "new_confirmed_per_capita",
  "X3month_avg_new_confirmed_per_capita",
  "new_deceased_per_capita",
  "X3month_avg_new_deceased_per_capita",
  "cumulative_confirmed_per_capita",
  "X3month_avg_cumulative_confirmed_per_capita",
  "cumulative_deceased_per_capita",
  "X3month_avg_cumulative_deceased_per_capita"
)

## keep only those that exist
pre_covid_zero_vars <- intersect(pre_covid_zero_vars, names(cfps))

## replace NA with 0 for wave 1 & 2 only
cfps <- cfps %>%
  mutate(
    across(
      all_of(pre_covid_zero_vars),
      ~ ifelse(wave %in% c(1, 2) & is.na(.), 0, .)
    )
  )

## restrict to rows with non-missing treatment and outcome -------
cfps_model <- cfps %>%
  filter(
    !is.na(cesd20),
    !is.na(X3month_avg_StringencyIndex_Average),
    !is.na(X3month_avg_new_deceased_per_capita),
    !is.na(wave)
  )

## --------------------------------------------------
## 1. baseline model
## --------------------------------------------------
model_full <- lmer(
  cesd20 ~
    X3month_avg_StringencyIndex_Average +
    X3month_avg_new_deceased_per_capita +
    wave +                  
    age_c +
    health_c +
    urban +
    gender +
    education +
    marital +
    (1 | provcd) +          # province
    (1 | cid) +             # community
    (1 | fid) +             # family
    (1 | pid),              # person
  data = cfps_model,
  REML = TRUE
)

summary(model_full)

## --------------------------------------------------
## 2. interacting stringency with wave 4
## --------------------------------------------------

cfps_model <- cfps_model %>%
  mutate(
    wave4 = ifelse(wave == 4, 1, 0)
  )

model_int_wave4 <- lmer(
  cesd20 ~
    X3month_avg_StringencyIndex_Average +
    X3month_avg_new_deceased_per_capita +
    wave +  # still have wave FE
    X3month_avg_StringencyIndex_Average:wave4 +
    age_c +
    health_c +
    urban +
    gender +
    education +
    marital +
    (1 | provcd) +
    (1 | cid) +
    (1 | fid) +
    (1 | pid),
  data = cfps_model,
  REML = TRUE
)

summary(model_int_wave4)

## --------------------------------------------------
## 3. removing wave fixed effects from baseline
## --------------------------------------------------

model_no_wave <- lmer(
  cesd20 ~
    X3month_avg_StringencyIndex_Average +
    X3month_avg_new_deceased_per_capita +
    age_c +
    health_c +
    urban +
    gender +
    education +
    marital +
    (1 | provcd) +
    (1 | cid) +
    (1 | fid) +
    (1 | pid),
  data = cfps_model,
  REML = TRUE
)

summary(model_no_wave)

## --------------------------------------------------
## 4. within province stringency
## --------------------------------------------------

cfps_model <- cfps_model %>%
  mutate(
    post2020 = cyear >= 2020
  ) %>%
  group_by(provcd) %>%
  mutate(
    ## province mean of stringency using only post-2020 observations
    stringency_mean_post2020 = ifelse(
      post2020,
      mean(X3month_avg_StringencyIndex_Average[post2020], na.rm = TRUE),
      NA_real_
    ),
    ## within-province, post 2020 demeaned stringency
    ## pre-\ 2020 stays exactly 0
    stringency_wp_post2020 = case_when(
      post2020 ~ X3month_avg_StringencyIndex_Average - stringency_mean_post2020,
      !post2020 ~ 0
    )
  ) %>%
  ungroup()

model_within_prov_post2020 <- lmer(
  cesd20 ~
    stringency_wp_post2020 +
    X3month_avg_new_deceased_per_capita +
    wave +              # you can drop this if you want
    age_c +
    health_c +
    urban +
    gender +
    education +
    marital +
    (1 | provcd) +
    (1 | cid) +
    (1 | fid) +
    (1 | pid),
  data = cfps_model,
  REML = TRUE
)

summary(model_within_prov_post2020)

## --------------------------------------------------
## 5. Random slope for stringency by province
## --------------------------------------------------

model_random_slope <- lmer(
  cesd20 ~
    X3month_avg_StringencyIndex_Average +
    X3month_avg_new_deceased_per_capita +
    wave +
    age_c +
    health_c +
    urban +
    gender +
    education +
    marital +
    (1 + X3month_avg_StringencyIndex_Average | provcd) +  # random slope
    (1 | cid) +
    (1 | fid) +
    (1 | pid),
  data = cfps_model,
  REML = TRUE
)

summary(model_random_slope)