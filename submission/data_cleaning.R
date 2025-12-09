library(dplyr)
library(haven)
library(foreign)
library(tidyverse)

## --------------------------------------------------
## merges survey data years
## --------------------------------------------------

## Loading all of the data
data_2022 <- read_dta("join_data/ecfps2022person_202410.dta")
data_2020 <- read_dta("join_data/cfps2020person_202306.dta")
data_2018 <- read_sas("join_data/ecfps2018person_202012.sas7bdat")
data_2016 <- read_dta("join_data/ecfps2016adult_201906.dta")

# Create a list of shared people IDs
inner <- inner_join(data_2022, data_2020, join_by(pid == pid)) %>%
  inner_join(data_2018, join_by(pid == PID)) %>%
  inner_join(data_2016, join_by(pid == pid))

shared_people <- inner$pid

# Filter for only this data before binding rows
data_2022 <- filter(data_2022, pid %in% shared_people)
data_2020 <- filter(data_2020, pid %in% shared_people)
data_2018 <- filter(data_2018, PID %in% shared_people)
data_2016 <- filter(data_2016, pid %in% shared_people)

# Preliminary cleaning to create wave as a categorical
data_2022$releaseversion <- as.character(data_2022$releaseversion)
data_2020$releaseversion <- as.character(data_2020$releaseversion)
data_2018$releaseversion <- as.character(data_2018$ReleaseVersion)
data_2016$releaseversion <- as.character(data_2016$releaseversion)

## --------------------------------------------------
## Backward-impute education: use 2018 EDU_LAST for 2016
## --------------------------------------------------

# 1. Build a pid–EDU_LAST lookup from 2018
#    (assumes the 2018 file has a column called EDU_LAST)
edu_2018 <- data_2018 %>%
  transmute(
    pid = PID,
    EDU_LAST_2018 = EDU_LAST
  ) %>%
  # make absolutely sure there is only one row per pid
  group_by(pid) %>%
  summarise(EDU_LAST_2018 = first(EDU_LAST_2018), .groups = "drop")

# 2. Make sure 2016 has an EDU_LAST column
if (!"EDU_LAST" %in% names(data_2016)) {
  data_2016$EDU_LAST <- NA
}

# 3. Join and fill missing 2016 EDU_LAST with the 2018 value
data_2016 <- data_2016 %>%
  left_join(edu_2018, by = "pid") %>%
  mutate(
    EDU_LAST = ifelse(is.na(EDU_LAST), EDU_LAST_2018, EDU_LAST)
  ) %>%
  select(-EDU_LAST_2018)

# (Optional sanity-check: ensure still 1 row per pid in 2016)
stopifnot(!any(duplicated(data_2016$pid)))

## --------------------------------------------------
## Row stacking with dplyr to create NAs where new rows are introduced.
## --------------------------------------------------
d <- bind_rows(data_2022, data_2018, data_2016, data_2020)
write.csv(d, "data/all_years_data_long.csv")




# Row stacking with dplyr to create NAs where new rows are introduced.
d <- bind_rows(data_2022, data_2018, data_2016, data_2020)
write.csv(d, 'data/all_years_data_long.csv')


## ---------------------------------------------------
## takes merged survey years
## selects variables to be included in smaller dataset for analysis
## ---------------------------------------------------

input_path  <- "data/all_years_data_long.csv"
output_path <- "data/cfps_selected.csv"

dat <- read.csv(input_path, stringsAsFactors = FALSE)

## 3. Helper: coalesce several vectors (base R)
coalesce_vec <- function(...) {
  args <- list(...)
  if (length(args) == 0L) return(NULL)
  if (length(args) == 1L) return(args[[1]])
  
  out <- args[[1]]
  for (j in 2:length(args)) {
    idx <- is.na(out)
    out[idx] <- args[[j]][idx]
  }
  out
}

## defines synonymous variables across years to be merged 

var_groups <- list(
  pid          = c("pid", "PID"),
  code         = c("code", "CODE"),
  fid          = c("fid22", "fid20", "fid18", "fid16", "fid14", "fid12",
                   "fid10", "fid_base", "FID18", "FID16", "FID14", "FID12", "FID10"),
  cid          = c("cid22", "cid20", "cid18", "cid16"),
  countyid     = c("countyid22", "countyid20", "countyid18", "countyid16"),
  provcd       = c("provcd22", "provcd20", "provcd16", "PROVCD18"),
  psu          = c("psu", "PSU"),
  subsample    = c("subsample", "SUBSAMPLE"),
  subpopulation = c("subpopulation", "SUBPOPULATION"),
  wt_natcs     = c("rswt_natcs22n", "rswt_natcs20n", "rswt_natcs18n", "rswt_natcs16"),
  wt_natpn10   = c("rswt_natpn1022n", "rswt_natpn1020n",
                   "rswt_natpn1018n", "rswt_natpn1016"),
  
  ## time variables
  cyear        = c("cyear"),              # year that survey was taken
  cmonth       = c("cmonth"),             # month that survey was taken
  qa001y       = c("qa001y", "QA001Y"),   # birth year
  qa001m       = c("qa001m", "QA001M"),   # birth month
  
  ## Outcome variable: depression (CESD20 is main one; 8 is kept for robustness)
  cesd20       = c("cesd20sc", "CESD20SC"),
  cesd8        = c("cesd8", "CESD8"),
  
  ## core covariates
  age          = c("age", "AGE", "CFPS_AGE", "Age"),
  gender       = c("gender", "GENDER", "CFPS_GENDER", "Gender"),
  education    = c("EDU_LAST", "edu_last"),
  urban        = c("urban22", "urban20", "urban16", "urban18","URBAN22", "URBAN20", "URBAN16", "URBAN18"),
  marital      = c("qea0", "QEA0"),
  health       = c("qp201", "QP201"),
  party        = c("party", "PARTY"),
  hukou        = c("huk", "cfps_hk"),
  ethnicity    = c("minzu", "MINZU")
)

## harmonizes variables
for (canon in names(var_groups)) {
  candidates <- intersect(var_groups[[canon]], names(dat))
  
  if (length(candidates) == 0L) {
    ## Variable not present in this dataset – skip
    next
  }
  
  if (length(candidates) == 1L) {
    ## Single column: just rename it to canonical name
    old_name <- candidates
    if (old_name != canon) {
      names(dat)[names(dat) == old_name] <- canon
    }
  } else {
    ## multiple columns: coalesce into one
    dat[[canon]] <- do.call(coalesce_vec, dat[candidates])
    
    ## drop the old columns (keep only the canonical)
    drop_these <- setdiff(candidates, canon)
    for (nm in drop_these) {
      dat[[nm]] <- NULL
    }
  }
}

## dropping covariates with high missingness
## (keep IDs, time, and outcome regardless)

## canonical variables we just defined
canon_vars <- names(var_groups)

## IDs, time, outcome that always kept
always_keep <- c("pid", "cyear", "cmonth", "cesd20")

## computing missingness for each canonical variable that exists
missing_prop <- sapply(
  canon_vars[canon_vars %in% names(dat)],
  function(v) mean(is.na(dat[[v]]))
)

## threshold for dropping covariates:
## anything with >= 0.20 missingness is dropped unless in always_keep
missing_thresh <- 0.20

## variables that pass the missingness filter or are in always_keep
keep_vars <- names(missing_prop)[missing_prop <= missing_thresh]
keep_vars <- union(always_keep, keep_vars)
keep_vars <- intersect(keep_vars, names(dat))

## orders variables nicely : ()
core_order <- intersect(
  c("pid", "cyear", "cmonth", "provcd", "countyid", "cid", "urban", "cesd20"),
  keep_vars
)

covar_order <- setdiff(keep_vars, core_order)

dat_clean <- dat[, c(core_order, covar_order), drop = FALSE]

## write a clear output csv
write.csv(dat_clean, file = output_path, row.names = FALSE)

## prints out a summary for checking
cat("Cleaned file written to:", output_path, "\n")
cat("Number of rows:", nrow(dat_clean), "\n")
cat("Number of columns:", ncol(dat_clean), "\n\n")

cat("Missingness for core variables:\n")
print(round(missing_prop[intersect(names(missing_prop), core_order)], 3))

cat("\nMissingness for other kept covariates:\n")
other_kept <- setdiff(keep_vars, core_order)
print(round(missing_prop[intersect(names(missing_prop), other_kept)], 3))

cat("\nVariables dropped due to missingness (>= ", missing_thresh, "):\n", sep = "")
dropped <- setdiff(names(missing_prop), keep_vars)
print(round(missing_prop[dropped], 3))

## ---------------------------------------------------
## continues above; handles missingess encoded into data, recenters otucome var
## ---------------------------------------------------

input_path  <- "data/cfps_selected.csv"       # <-- output from above script
output_path <- "data/cfps_cleaned.csv"    # <-- outputs cleaned CFPS data

dat <- read.csv(input_path, stringsAsFactors = FALSE)

cat("Loaded file:", input_path, "\n")
cat("Rows:", nrow(dat), "  Columns:", ncol(dat), "\n\n")

## identifies columns that are covariates
## idenifying as being as not covariates: IDs, time, outcome
non_covars <- c("pid", "cyear", "cmonth",
                "provcd", "countyid", "cid",
                "cesd20")

covar_cols <- setdiff(names(dat), non_covars)

cat("Number of covariate columns:", length(covar_cols), "\n")
cat("Covariate columns:\n")
print(covar_cols)
cat("\n")

## helper function to flag bad covariate values: -1, -8, or NA
is_bad_covar <- function(x) {
  # handle numeric or character
  is_na <- is.na(x)
  is_m1 <- (x == -1) | (x == "-1")
  is_m8 <- (x == -8) | (x == "-8")
  is_m9 <- (x == -9) | (x == "-9")
  
  # replace NAs in comparisons with FALSE
  is_m1[is.na(is_m1)] <- FALSE
  is_m8[is.na(is_m8)] <- FALSE
  is_m9[is.na(is_m9)] <- FALSE
  
  is_na | is_m1 | is_m8 | is_m9
}

## identifying people with -1, -8, or NA in any covariate
if (length(covar_cols) == 0L) {
  row_has_bad <- rep(FALSE, nrow(dat))
} else {
  bad_list <- lapply(dat[covar_cols], is_bad_covar)
  bad_mat  <- do.call(cbind, bad_list)
  row_has_bad <- apply(bad_mat, 1, any)
}

bad_pids <- unique(dat$pid[row_has_bad])

cat("Number of rows with -1, -8, or NA in at least one covariate:",
    sum(row_has_bad), "\n")
cat("Number of unique pids to drop due to bad covariates:",
    length(bad_pids), "\n\n")

## Drop all rows for these pids
dat_clean <- dat[!(dat$pid %in% bad_pids), ]

cat("After dropping those pids:\n")
cat("Rows:", nrow(dat_clean), "  Columns:", ncol(dat_clean), "\n\n")

cat("After dropping those pids:\n")
cat("Rows:", nrow(dat_clean), "  Columns:", ncol(dat_clean), "\n\n")


## removing people with missing CESD-20 scores (-8)

# identify rows with CESD-20 == -8
is_minus8_cesd <- (dat_clean$cesd20 == -8) | (dat_clean$cesd20 == "-8")
is_minus8_cesd[is.na(is_minus8_cesd)] <- FALSE

bad_pids_cesd <- unique(dat_clean$pid[is_minus8_cesd])

cat("Number of rows with CESD-20 == -8:", sum(is_minus8_cesd), "\n")
cat("Number of unique pids to drop due to CESD-20 == -8:",
    length(bad_pids_cesd), "\n\n")

# drop all rows for those pids
dat_clean <- dat_clean[!(dat_clean$pid %in% bad_pids_cesd), ]

cat("After dropping CESD-20 == -8 pids:\n")
cat("Rows:", nrow(dat_clean), "  Columns:", ncol(dat_clean), "\n\n")


## Recentering CESD 20 by subtracting 20
dat_clean$cesd20 <- as.numeric(dat_clean$cesd20) - 20

cat("CESD-20 summary after subtracting 20:\n")
print(summary(dat_clean$cesd20))
cat("\n")

## writing cleaned CFPS data as csv
write.csv(dat_clean, file = output_path, row.names = FALSE)

cat("Final modeling file written to:", output_path, "\n")

## printing out sample size and whatnot

n_people <- length(unique(dat_clean$pid))
n_rows   <- nrow(dat_clean)

cat("Final sample after cleaning:\n")
cat("  Number of unique individuals:", n_people, "\n")
cat("  Number of person-year observations:", n_rows, "\n")

cat("\nAverage number of waves per person:",
    round(n_rows / n_people, 2), "\n")

## distribution of waves per person
waves_per_person <- table(table(dat_clean$pid))

cat("\nDistribution of waves per person:\n")
print(waves_per_person)

length(unique(dat_clean$cid))



## -----------------------
# takes in cleaned CFPS data and outside covid  & stringency data
# outputs csv ready for the models
## -----------------------

# reads in cleaned CFPS data
cfps <- read.csv("data/cfps_cleaned.csv",
                 stringsAsFactors = FALSE)

# reads in outside data
outside <- read.csv("data/outside_data.csv",
                       stringsAsFactors = FALSE)

## this is a key to convert the province codes into the names
province_crosswalk <- data.frame(
  provcd = c(
    11, 12, 13, 14, 15,
    21, 22, 23,
    31, 32, 33, 34, 35, 36, 37,
    41, 42, 43, 44, 45, 46,
    50, 51, 52, 53, 54,
    61, 62, 63, 64, 65
  ),
  province = c(
    "Beijing",
    "Tianjin",
    "Hebei",
    "Shanxi",
    "Inner Mongolia",
    "Liaoning",
    "Jilin",
    "Heilongjiang",
    "Shanghai",
    "Jiangsu",
    "Zhejiang",
    "Anhui",
    "Fujian",
    "Jiangxi",
    "Shandong",
    "Henan",
    "Hubei",
    "Hunan",
    "Guangdong",
    "Guangxi", 
    "Hainan",
    "Chongqing",
    "Sichuan",
    "Guizhou",
    "Yunnan",
    "Tibet",
    "Shaanxi",
    "Gansu",
    "Qinghai",
    "Ningxia",
    "Xinjiang"
  ),
  stringsAsFactors = FALSE
)

## uses the key to convert provences
cfps <- cfps %>%
  left_join(province_crosswalk, by = "provcd")

# check for any provcd that didn't match
setdiff(sort(unique(cfps$provcd)), province_crosswalk$provcd)

## prepares the ouside data to be merged to the survey data
outside_clean <- outside %>%
  transmute(
    province                  = province,
    cyear                     = year,
    cmonth                    = month_num,
    new_confirmed,
    X3month_avg_new_confirmed,
    new_deceased,
    X3month_avg_new_deceased,
    cumulative_confirmed,
    X3month_avg_cumulative_confirmed,
    cumulative_deceased,
    X3month_avg_cumulative_deceased,
    StringencyIndex_Average,
    X3month_avg_StringencyIndex_Average,
    new_confirmed_per_capita,
    X3month_avg_new_confirmed_per_capita,
    new_deceased_per_capita,
    X3month_avg_new_deceased_per_capita,
    cumulative_confirmed_per_capita,
    X3month_avg_cumulative_confirmed_per_capita,
    cumulative_deceased_per_capita,
    X3month_avg_cumulative_deceased_per_capita
  )

## marges the survey and outside data 
# also imputes stringency and COVID deaths for pre covid years (just putting 0s)
cfps <- cfps %>%
  left_join(outside_clean,
            by = c("province", "cyear", "cmonth"))

cfps <- cfps %>%
  mutate(
    StringencyIndex_Average = ifelse(
      cyear < 2020, 0, StringencyIndex_Average
    )
  )

cfps <- cfps %>%
  group_by(province) %>%
  arrange(cyear, cmonth) %>%
  tidyr::fill(
    StringencyIndex_Average,
    new_confirmed,
    new_deceased,
    cumulative_confirmed,
    cumulative_deceased,
    .direction = "down"
  ) %>%
  ungroup()

cfps <- cfps %>%
  mutate(
    wave = case_when(
      cyear %in% c(2016, 2017) ~ 1,
      cyear %in% c(2018, 2019) ~ 2,
      cyear == 2020            ~ 3,
      cyear == 2022            ~ 4,
      TRUE ~ NA_real_
    )
  )

## quick checks to see if done correctly
summary(cfps$StringencyIndex_Average)

# how many NAs by year
table(cfps$cyear, is.na(cfps$StringencyIndex_Average))


# checking that province name sets match
sort(unique(cfps$province))
sort(unique(outside_clean$province))

# saving data
write.csv(
  cfps,
  file = "data/model_ready_data.csv",
  row.names = FALSE
)