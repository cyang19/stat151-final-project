## ---------------------------------------------------
## CFPS: Harmonize columns across years and output
## a person-year level cleaned dataset
## ---------------------------------------------------

## 1. User inputs: paths
input_path  <- "data/all_years_data_long.csv"  # <-- merged long file
output_path <- "data/cfps_selected.csv"         # <-- cleaned output

## 2. Read data (assumes a reasonably large CSV)
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

## 4. Define groups of synonymous variables to be merged
##    (canonical name = list of possible column names in the raw data)

var_groups <- list(
  ## IDs and design
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
  
  ## Time
  cyear        = c("cyear"),
  cmonth       = c("cmonth"),
  qa001y       = c("qa001y", "QA001Y"),   # birth year
  qa001m       = c("qa001m", "QA001M"),   # birth month
  
  ## Outcome: depression
  cesd20       = c("cesd20sc", "CESD20SC"),
  cesd8        = c("cesd8", "CESD8"),
  
  ## Core covariates
  age          = c("age", "AGE", "cfps_age"),
  gender       = c("gender", "GENDER", "cfps_gender", "gender_pre"),
  education    = c("edu_last", "EDU_LAST", "cfps_latest_edu",
                   "cfps2016edu", "CFPS2018EDU", "cfps2022edu"),
  urban        = c("urban22", "urban20", "urban16", "URBAN18"),
  marital      = c("qea0", "QEA0", "marriage_last", "MARRIAGE_LAST",
                   "marriage_last_update"),
  health       = c("qp201", "QP201"),
  party        = c("party", "PARTY"),
  hukou        = c("huk", "cfps_hk"),
  ethnicity    = c("minzu", "MINZU")
)

## 5. Harmonize (coalesce) variables
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
    ## Multiple columns: coalesce into one
    dat[[canon]] <- do.call(coalesce_vec, dat[candidates])
    
    ## Drop the old columns (keep only the canonical)
    drop_these <- setdiff(candidates, canon)
    for (nm in drop_these) {
      dat[[nm]] <- NULL
    }
  }
}

## 6. Drop covariates with high missingness
##    (keep IDs, time, and outcome regardless)

## Canonical variables we just defined
canon_vars <- names(var_groups)

## IDs, time, outcome that we *always* keep
always_keep <- c("pid", "cyear", "cmonth", "cesd20")

## Compute missingness for each canonical variable that exists
missing_prop <- sapply(
  canon_vars[canon_vars %in% names(dat)],
  function(v) mean(is.na(dat[[v]]))
)

## Stricter threshold for dropping covariates:
## anything with >= 0.20 missingness is dropped unless in always_keep
missing_thresh <- 0.20

## Variables that pass the missingness filter OR are in always_keep
keep_vars <- names(missing_prop)[missing_prop <= missing_thresh]
keep_vars <- union(always_keep, keep_vars)
keep_vars <- intersect(keep_vars, names(dat))  # safety check

## 7. Final variable order: ID + time + outcome + covariates
core_order <- intersect(
  c("pid", "cyear", "cmonth", "provcd", "countyid", "cid", "urban", "cesd20"),
  keep_vars
)

covar_order <- setdiff(keep_vars, core_order)

dat_clean <- dat[, c(core_order, covar_order), drop = FALSE]

## 8. Write cleaned CSV (person-year level)
write.csv(dat_clean, file = output_path, row.names = FALSE)

## 9. Print a small summary so you can inspect what was kept/dropped
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