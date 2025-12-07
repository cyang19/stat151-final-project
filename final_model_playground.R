
## packages

library(dplyr)
library(lme4)
library(lmerTest)

## load merged data ------------------------------------------------------
cfps <- read.csv("data/final_merged_data.csv",
                 stringsAsFactors = FALSE)

## Handle CFPS missing code (-8) in covariates

# List of covariates used in the model where -8 means missing
covariates_with_missing_codes <- c(
  "age",
  "health",
  "education",
  "gender",
  "marital",
  "urban"
)

# Drop rows with -8 in any of these covariates
cfps <- cfps %>%
  filter(
    !if_any(
      all_of(covariates_with_missing_codes),
      ~ .x == -8
    )
  )

## Quick sanity check
cat("Rows:", nrow(cfps), "  Cols:", ncol(cfps), "\n")
str(cfps[, c("pid", "fid", "cid", "provcd", "cyear", "cmonth",
             "cesd20", "StringencyIndex_Average")])



## variable types --------------------------------------
## IDs as factors for random effects
cfps <- cfps %>%
  mutate(
    pid   = factor(pid),
    fid   = factor(fid),
    cid   = factor(cid),
    provcd = factor(provcd),
    wave  = factor(wave),
    
    ## Time varying covariates (example choices)
    age_c    = scale(age, center = TRUE, scale = FALSE)[, 1],
    health_c = scale(health, center = TRUE, scale = FALSE)[, 1],
    education_c = scale(education, center = TRUE, scale = FALSE)[, 1],
    
    ## Time-invariant-ish covariates as factors
    gender    = factor(gender),
    education = factor(education),
    marital   = factor(marital),
    urban     = factor(urban)
  )

## restrict to rows with non-missing treatment and outcome -------
cfps_model <- cfps %>%
  filter(
    !is.na(cesd20),
    !is.na(StringencyIndex_Average),
    !is.na(wave)
  )

cat("Rows used in model:", nrow(cfps_model), "\n")

## 4. Specify multilevel model ---------------------------------------------
## Outcome: cesd20
## Key treatment: StringencyIndex_Average (province-month)
## Time-varying individual covariates Z_tifcp: age_c, health_c, urban, wave
## Time-invariant individual covariates X_ifcp: gender, education, marital
## Random intercepts: province, community, family, person

## This corresponds to:
## y_tifcp = β0
##           + β1 * StringencyIndex_Average_pt
##           + β_Z' * Z_tifcp
##           + β_X' * X_ifcp
##           + u_p + u_cp + u_fcp + u_ifcp + ε_tifcp

model_full <- lmer(
  cesd20 ~
    StringencyIndex_Average +
    wave +                  # time / wave effects
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

## 5. Inspect results -------------------------------------------------------
summary(model_full)

## Variance decomposition (ICCs-ish)
VarCorr(model_full)

## 6. save model object -----------------------------------------
# saveRDS(model_full, file = "output/model_full_lmer.rds")



# without wave model
model_n_wave <- lmer(
  cesd20 ~
    StringencyIndex_Average +
    age_c + health_c +
    urban + gender + education + marital +
    (1 | provcd) + (1 | cid) + (1 | fid) + (1 | pid),
  data = cfps_model
)

summary(model_n_wave)

