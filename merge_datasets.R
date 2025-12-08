## --------------------------------------------------
## 0. Packages
## --------------------------------------------------
library(dplyr)

## --------------------------------------------------
## 1. Read data
## --------------------------------------------------
cfps <- read.csv("data/cfps_model_ready.csv",
                 stringsAsFactors = FALSE)

stringency <- read.csv("data/FULLY_READY.csv",
                       stringsAsFactors = FALSE)

## --------------------------------------------------
## 2. Province crosswalk (provcd -> province name)
##    Names must match stringency$province exactly
## --------------------------------------------------
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
    "Guangxi",   # shorten to match stringency
    "Hainan",
    "Chongqing",
    "Sichuan",
    "Guizhou",
    "Yunnan",
    "Tibet",
    "Shaanxi",
    "Gansu",
    "Qinghai",
    "Ningxia",   # shorten to match stringency
    "Xinjiang"   # shorten to match stringency
  ),
  stringsAsFactors = FALSE
)

## --------------------------------------------------
## 3. Attach province names to CFPS
## --------------------------------------------------
cfps <- cfps %>%
  left_join(province_crosswalk, by = "provcd")

# (optional) check for any provcd that didn't match
setdiff(sort(unique(cfps$provcd)), province_crosswalk$provcd)
# should just be -9 (missing province)

## --------------------------------------------------
## 4. Clean stringency data and prepare for merge
## --------------------------------------------------
stringency_clean <- stringency %>%
  transmute(
    province                  = province,
    cyear                     = year,
    cmonth                    = month_num,
    new_confirmed,
    new_deceased,
    cumulative_confirmed,
    cumulative_deceased,
    StringencyIndex_Average, 
    new_confirmed_per_capita,
    new_deceased_per_capita,
    cumulative_confirmed_per_capita,
    cumulative_deceased_per_capita
  )



## --------------------------------------------------
## 5. Merge CFPS with stringency; impute pre covid
## --------------------------------------------------
cfps <- cfps %>%
  left_join(stringency_clean,
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

## --------------------------------------------------
## 6. Quick sanity checks
## --------------------------------------------------
summary(cfps$StringencyIndex_Average)

# How many NAs by year?
table(cfps$cyear, is.na(cfps$StringencyIndex_Average))


# Check that province name sets now match
sort(unique(cfps$province))
sort(unique(stringency_clean$province))

# save data
write.csv(
  cfps,
  file = "data/final_merged_data.csv",
  row.names = FALSE
)