############################################
## Script: list_columns_merged_cfps.R
############################################

## read the merged person-year CSV from disk
merged_path <- "data/all_years_data_long.csv"
dat <- read.csv(merged_path, stringsAsFactors = FALSE)



## Check basic dimensions
cat("Number of rows:", nrow(dat), "\n")
cat("Number of columns:", ncol(dat), "\n\n")

## Create a numbered list of variable names
var_list <- data.frame(
  index   = seq_along(names(dat)),
  varname = names(dat),
  stringsAsFactors = FALSE
)

## Print to console in a copy-friendly way
print(var_list, row.names = FALSE)

## OPTIONAL: also write just the names to a text file for easy copy/paste
## (uncomment if you want this)
writeLines(names(dat), "cfps_merged_column_names.txt")
cat("\nColumn names written to: cfps_merged_column_names.txt\n")
