## ---------------------------------------------------
## CFPS: Additional cleaning for modeling
##  - Start from cfps_selected.csv
##  - Treat -1 as missing in covariates
##  - Drop any person who ever has -1 in a covariate
##  - Re-center CESD-20 by subtracting 20
## ---------------------------------------------------

## 1. User inputs: paths
input_path  <- "data/cfps_selected.csv"       # <-- output from previous script
output_path <- "data/cfps_model_ready.csv"    # <-- final modeling file

## 2. Read data
dat <- read.csv(input_path, stringsAsFactors = FALSE)

cat("Loaded file:", input_path, "\n")
cat("Rows:", nrow(dat), "  Columns:", ncol(dat), "\n\n")

## 3. Identify covariate columns
## We'll treat these as *not* covariates: IDs, time, outcome
non_covars <- c("pid", "cyear", "cmonth",
                "provcd", "countyid", "cid",
                "cesd20")

covar_cols <- setdiff(names(dat), non_covars)

cat("Number of covariate columns:", length(covar_cols), "\n")
cat("Covariate columns:\n")
print(covar_cols)
cat("\n")

## 4. Helper: flag -1 values in a vector (numeric or character)
is_minus1 <- function(x) {
  out <- (x == -1) | (x == "-1")
  out[is.na(out)] <- FALSE
  out
}

## 5. Find people who ever have -1 in any covariate
if (length(covar_cols) == 0L) {
  # No covariates (weird, but handle gracefully)
  row_has_minus1 <- rep(FALSE, nrow(dat))
} else {
  minus_list <- lapply(dat[covar_cols], is_minus1)
  minus_mat  <- do.call(cbind, minus_list)
  row_has_minus1 <- apply(minus_mat, 1, any)
}

bad_pids <- unique(dat$pid[row_has_minus1])

cat("Number of rows with -1 in at least one covariate:", sum(row_has_minus1), "\n")
cat("Number of unique pids to drop:", length(bad_pids), "\n\n")

## 6. Drop all rows for these pids
dat_clean <- dat[!(dat$pid %in% bad_pids), ]

cat("After dropping those pids:\n")
cat("Rows:", nrow(dat_clean), "  Columns:", ncol(dat_clean), "\n\n")


## ---------------------------------------------------
## Remove people with CESD-20 coded as -8 (not applicable)
## ---------------------------------------------------

# Identify rows with CESD-20 == -8
is_minus8_cesd <- (dat_clean$cesd20 == -8) | (dat_clean$cesd20 == "-8")
is_minus8_cesd[is.na(is_minus8_cesd)] <- FALSE

bad_pids_cesd <- unique(dat_clean$pid[is_minus8_cesd])

cat("Number of rows with CESD-20 == -8:", sum(is_minus8_cesd), "\n")
cat("Number of unique pids to drop due to CESD-20 == -8:",
    length(bad_pids_cesd), "\n\n")

# Drop all rows for those pids
dat_clean <- dat_clean[!(dat_clean$pid %in% bad_pids_cesd), ]

cat("After dropping CESD-20 == -8 pids:\n")
cat("Rows:", nrow(dat_clean), "  Columns:", ncol(dat_clean), "\n\n")


## 7. Re-center CESD-20: subtract 20
## Make sure it's numeric first
dat_clean$cesd20 <- as.numeric(dat_clean$cesd20) - 20

cat("CESD-20 summary after subtracting 20:\n")
print(summary(dat_clean$cesd20))
cat("\n")

## 8. Write final cleaned CSV
write.csv(dat_clean, file = output_path, row.names = FALSE)

cat("Final modeling file written to:", output_path, "\n")

## ---------------------------------------------------
## Sample size diagnostics
## ---------------------------------------------------

n_people <- length(unique(dat_clean$pid))
n_rows   <- nrow(dat_clean)

cat("Final sample after cleaning:\n")
cat("  Number of unique individuals:", n_people, "\n")
cat("  Number of person-year observations:", n_rows, "\n")

cat("\nAverage number of waves per person:",
    round(n_rows / n_people, 2), "\n")

## Distribution of waves per person
waves_per_person <- table(table(dat_clean$pid))

cat("\nDistribution of waves per person:\n")
print(waves_per_person)