#### Background data for cleaning HJ Andrews data 
# There are long-term data sets, but this script is intended to only analyze 
# data during field period times for WY 23-24
# The v-notch was not put on this year so we can use a single rating curve eq for flow
# note: WS1 rating curve is different from WS 3
# Created 2025-01-08 JAK
### H.J. Andrews environmental data processing: WY24–26
#
# This script processes environmental monitoring data from WS01 and the
# nearby PRIMET climate station for use with the WS01 dissolved oxygen dataset.
#
# Workflow:
# 1. Flow:
#    Combine annual WS01 gage records, standardize variable names, calculate
#    discharge from stage using the appropriate WS01 rating curve, remove
#    flagged water-temperature and conductivity observations, and clean the
#    resulting gage dataset.
#
# 2. Climate:
#    Combine annual PRIMET climate records, summarize data-quality flags,
#    and remove flag and metadata columns not needed for subsequent analyses.
#
# 3. Nutrients:
#    Combine WS01 long-term chemistry records from 2024 and 2025, standardize
#    variable names and dates, and retain water-chemistry variables used in
#    subsequent analyses.
#
# 4. Combine and export:
#    Join the cleaned climate and WS01 gage datasets by timestamp and export
#    the environmental and nutrient datasets for subsequent analyses.
#
# Final outputs:
# Processed_data/HJA_climate_clean.csv
# Processed_data/HJA_nutrient.csv

library(tidyverse)
library(lubridate)
library(readxl)


#### 1. Flow ####

data_dir <- "/Users/jasminekrause/Documents/GitHub/HJ-Andrews-WY2324/Raw_data/hja_data/"

# Read annual 5-minute WS01 gage records.
discharge24 <- readr::read_csv(paste0(data_dir, "gsws01_115_5min_2024.csv"), skip = 2)
discharge25 <- readr::read_csv(paste0(data_dir, "gsws01_115_5min_2025.csv"), skip = 2)
discharge26 <- readr::read_csv(paste0(data_dir, "gsws01_115_5min_2026.csv"), skip = 2)

# Standardize 2024 conductivity column names to match later years.
discharge24 <- discharge24 %>%
  dplyr::rename(
    ECD_INST_0_0_01 = EC_INST_0_0_01,
    Flag_ECD_INST_0_0_01 = Flag_EC_INST_0_0_01
  )

# Standardize air-temperature column names across years.
rename_discharge <- function(df) {
  df %>%
    dplyr::rename(
      AIRTEMP_MEAN = matches("^AIRTEMP_MEAN"),
      AIRTEMP_MAX = matches("^AIRTEMP_MAX"),
      AIRTEMP_MIN = matches("^AIRTEMP_MIN")
    )
}

discharge24 <- rename_discharge(discharge24)
discharge25 <- rename_discharge(discharge25)
discharge26 <- rename_discharge(discharge26)

# Check that annual files have matching structures before combining.
identical(names(discharge24), names(discharge25))
identical(names(discharge24), names(discharge26))

merged_discharge <- dplyr::bind_rows(discharge24, discharge25, discharge26) %>%
  dplyr::mutate(
    Date = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S"),
    STAGE_INST_0_0_01 = as.numeric(STAGE_INST_0_0_01)
  )


### WS01 rating curve

# Rating-curve parameters are organized by rating period and stage interval.
rating_params_A <- list(
  c(1.6549710, 1.0780520, 0.0, 0.0, 0.020),
  c(3.4585459, 1.5371380, 0.0, 0.0, 0.076),
  c(2.8872161, 1.3155500, 0.0, 0.0, 0.274),
  c(3.3595591, 1.6805220, 0.0, 0.0, 2.000)
)

rating_params_B <- list(
  c(3.4810755, 1.6658920, 0.0, 0.0, 0.041),
  c(1.7674992, 1.1277280, 0.0, 0.0, 0.085),
  c(3.4640393, 1.8171530, 0.0, 0.0, 0.113),
  c(2.4405980, 1.3469430, 0.0, 0.0, 0.186),
  c(3.4657080, 1.9568490, 0.0, 0.0, 0.342),
  c(3.0569163, 1.5761290, 0.0, 0.0, 0.701),
  c(3.2120695, 2.0129220, 0.0, 0.0, 2.000)
)

rating_params_D <- list(
  c(4.3900970, 2.1141930, 0.0, 0.0, 0.045),
  c(2.4261690, 1.4800410, 0.0, 0.0, 0.186),
  c(3.1040300, 1.8834030, 0.0, 0.0, 2.000)
)

rating_sets <- list(
  set_A = rating_params_A,
  set_B = rating_params_B,
  set_D = rating_params_D
)

# Select the appropriate rating-curve set for each observation date.
get_rating_params <- function(Date) {
  if (is.na(Date)) return(NULL)
  
  if (Date < as.POSIXct("2024-04-01")) {
    rating_sets$set_A
  } else if (Date < as.POSIXct("2026-12-07")) {
    rating_sets$set_B
  } else {
    rating_sets$set_D
  }
}

# Calculate discharge for one stage observation using the selected equation.
calculate_discharge <- function(gage_ht, params) {
  if (is.na(gage_ht) || !is.numeric(gage_ht) || gage_ht <= 0) return(NA_real_)
  
  ln_a <- params[1]
  b <- params[2]
  c <- params[3]
  d <- params[4]
  
  ln_x <- log(gage_ht)
  ln_y <- log(ln_a) + b * ln_x + c * ln_x^2 + d * ln_x^3
  
  exp(ln_y)
}

# Vectorized rating curve
calculate_Q_vec <- function(stage, date) {
  out <- rep(NA_real_, length(stage))
  
  set_id <- dplyr::case_when(
    is.na(date)                              ~ NA_character_,
    date < as.POSIXct("2024-04-01")          ~ "set_A",
    date < as.POSIXct("2026-12-07")          ~ "set_B",
    TRUE                                     ~ "set_D"
  )
  
  for (s in c("set_A", "set_B", "set_D")) {
    idx <- which(set_id == s & !is.na(stage) & stage > 0)
    if (length(idx) == 0) next
    
    P <- do.call(rbind, rating_sets[[s]])        # cols: a, b, c, d, upper bound
    k <- findInterval(stage[idx], c(-Inf, P[, 5]), left.open = TRUE)   # first interval with stage <= bound
    ok <- k >= 1 & k <= nrow(P)                  # stage above last bound -> NA
    i  <- idx[ok]; kk <- k[ok]
    
    ln_x <- log(stage[i])
    out[i] <- exp(log(P[kk, 1]) + P[kk, 2] * ln_x + P[kk, 3] * ln_x^2 + P[kk, 4] * ln_x^3)
  }
  out
}

merged_discharge <- merged_discharge %>%
  dplyr::mutate(Discharge_cfs = calculate_Q_vec(STAGE_INST_0_0_01, Date),
                Discharge_L_s = Discharge_cfs * 28.3168)

### Clean gage data

# Remove observations flagged for conductivity or water temperature.
merged_discharge_clean <- merged_discharge %>%
  dplyr::filter(
    is.na(Flag_ECD_INST_0_0_01),
    is.na(Flag_WATERTEMP_MEAN_0_0_01)
  ) %>%
  # Remove remaining conductivity spikes outside the expected range.
  dplyr::filter(ECD_INST_0_0_01 >= 25, ECD_INST_0_0_01 <= 67) %>%
  dplyr::select(
    -dplyr::contains("Flag"),
    -SiteID, -LOGGERID, -PROGID,
    -VNOTCH_INST_0_0_01, -BATTERY_INST_0_0_01, -Site
  )

# Check missing air-temperature observations.
sum(is.na(merged_discharge_clean$AIRTEMP_MEAN))

# Diagnostic plots.
ggplot(merged_discharge_clean, aes(Date, Discharge_L_s)) +
  geom_line(linewidth = 0.7) +
  labs(x = "Date", y = "Discharge (L/s)", title = "WS01 discharge") +
  theme_classic()

ggplot(merged_discharge_clean, aes(Date, AIRTEMP_MEAN)) +
  geom_line(linewidth = 0.7) +
  labs(x = "Date", y = "Air temperature (°C)", title = "WS01 air temperature") +
  theme_classic()


#### 2. Climate ####

# Read annual 5-minute PRIMET climate records and standardize timestamps.
climate24 <- readr::read_csv(paste0(data_dir, "primet_230_a_5min_2024.csv"), skip = 2) %>%
  dplyr::mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S"))

climate25 <- readr::read_csv(paste0(data_dir, "primet_230_a_5min_2025.csv"), skip = 2) %>%
  dplyr::mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S"))

climate26 <- readr::read_csv(paste0(data_dir, "primet_230_a_5min_2026.csv"), skip = 2) %>%
  dplyr::mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S"))

merged_climate <- dplyr::bind_rows(climate24, climate25, climate26)


### Check climate data-quality flags

flag_columns <- grep("^Flag_", names(merged_climate), value = TRUE)

flag_counts <- flag_columns %>%
  stats::setNames(., .) %>%
  lapply(function(x) sum(!is.na(merged_climate[[x]]))) %>%
  unlist() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Flag_Variable") %>%
  dplyr::rename(Flag_Count = ".")

ggplot(flag_counts, aes(reorder(Flag_Variable, -Flag_Count), Flag_Count)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Number of flagged observations per climate variable",
    x = "Flag variable",
    y = "Number of flags"
  )

# Remove flag, logger, and other metadata variables not needed for analysis.
merged_climate_clean <- merged_climate %>%
  dplyr::select(
    -dplyr::contains("Flag"),
    -RecNum, -LOGGERID, -PROGID, -BATTERY_V_CHECK,
    -SWC_MEAN_0_20_02, -Site, -Hour
  )


#### 3. Nutrients ####

# Read long-term WS01 chemistry records.
nutrient24 <- readr::read_csv(paste0(data_dir, "CF00201_v7.csv")) %>%
  dplyr::filter(SITECODE == "GSWS01") %>%
  stats::setNames(tolower(names(.))) %>%
  dplyr::mutate(date_time = lubridate::ymd_hms(date_time))

nutrient25 <- readxl::read_excel(paste0(data_dir, "cf002v_2025.xlsx")) %>%
  dplyr::filter(sitecode == "GSWS01") %>%
  stats::setNames(tolower(names(.))) %>%
  dplyr::mutate(date_time = lubridate::dmy_hms(date_time))

# Retain columns shared between the two source datasets.
common_cols <- intersect(names(nutrient24), names(nutrient25))

# Convert to character before binding because some source columns differ in type,
# then restore date_time after the datasets are combined.
nutrient_combined <- dplyr::bind_rows(
  nutrient24 %>%
    dplyr::select(dplyr::all_of(common_cols)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)),
  nutrient25 %>%
    dplyr::select(dplyr::all_of(common_cols)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
) %>%
  dplyr::mutate(date_time = lubridate::ymd_hms(date_time)) %>%
  dplyr::arrange(date_time)

# convert nh3 into nh4 (total ammonia-N -> ammonium-N)
partition_ammonia <- function(tan_mgN_L, pH, temp_C) {
  pKa   <- 0.09018 + 2729.92 / (temp_C + 273.15)       # Emerson et al. (1975)
  f_NH3 <- 1 / (1 + 10^(pKa - pH))
  tibble::tibble(f_NH3 = f_NH3, nh4n = tan_mgN_L * (1 - f_NH3))
}

# daily stream temperature from merged_all
temp_daily <- merged_all %>%
  dplyr::transmute(sample_date = as.Date(Date),
                   t = dplyr::coalesce(as.numeric(WATERTEMP_MEAN_0_0_01),
                                       as.numeric(WATERTEMP_INST_0_0_01))) %>%
  dplyr::group_by(sample_date) %>%
  dplyr::summarise(t = mean(t, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(t = ifelse(is.nan(t), NA_real_, t))
sum(!is.na(temp_daily$t))                                   # days with temperature

# mean stream temperature over the 21-day composite window ending at each sample date
win_temp <- function(end) {
  x <- temp_daily$t[temp_daily$sample_date >= end - 21 & temp_daily$sample_date <= end]
  if (sum(is.finite(x)) < 10) NA_real_ else mean(x, na.rm = TRUE)
}

nutrient_combined <- nutrient_combined %>%
  dplyr::mutate(sample_date = as.Date(date_time),
                nh3n_total  = suppressWarnings(as.numeric(nh3n)),
                ph_num      = suppressWarnings(as.numeric(ph)),
                temp_win    = purrr::map_dbl(sample_date, win_temp),
                ph_use      = dplyr::coalesce(ph_num, 7.0),    # fixed fallbacks; f_NH3 is insensitive to these
                temp_use    = dplyr::coalesce(temp_win, 10))

# check inputs before converting
nutrient_combined %>% dplyr::summarise(n = dplyr::n(),
                                       nh3_na = sum(is.na(nh3n_total)),
                                       ph_na = sum(is.na(ph_num)),
                                       temp_win_na = sum(is.na(temp_win)))

nh <- partition_ammonia(nutrient_combined$nh3n_total, nutrient_combined$ph_use, nutrient_combined$temp_use)

nutrient_combined2 <- nutrient_combined %>%
  dplyr::mutate(nh4n = nh$nh4n, f_NH3 = nh$f_NH3) %>%
  dplyr::select(-nh3n, -nh3n_total, -ph_num, -ph_use, -temp_use, -temp_win, -sample_date) %>%
  dplyr::rename(nh4ncode = nh3ncode)           # keep the below-detection flag with the renamed column

summary(nutrient_combined2$f_NH3)               # should be < 0.01 at WS01 pH and temperature

# Retain chemistry variables used in subsequent analyses from 2024 onward.
nutrient_save <- nutrient_combined2 %>%
  dplyr::filter(lubridate::year(date_time) >= 2024) %>%
  dplyr::select(
    date_time, ph, alk, ssed, si, utp, tdp, partp, po4p,
    utn, tdn, don, partn, nh4n, no3n, na, k, ca, mg,
    so4s, cl, doc
  )



#### 4. Combine and export ####

# Join PRIMET climate data with WS01 gage data using the 5-minute timestamp.
merged_all <- merged_climate_clean %>%
  dplyr::full_join(merged_discharge_clean, by = "Date") %>%
  dplyr::filter(!is.na(Date))

dplyr::glimpse(merged_all)

# Export environmental time series.
write.csv(
  merged_all,
  "/Users/jasminekrause/Documents/GitHub/HJ-Andrews-WY2324/Processed_data/HJA_climate_clean.csv",
  row.names = FALSE
)

# Export WS01 chemistry.
write.csv(
  nutrient_save,
  "/Users/jasminekrause/Documents/GitHub/HJ-Andrews-WY2324/Processed_data/HJA_nutrient.csv",
  row.names = FALSE
)
