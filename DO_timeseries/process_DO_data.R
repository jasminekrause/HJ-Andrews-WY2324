################################################################################
# Process miniDOT dissolved oxygen (DO) data and assemble the final dataset
# WS01, H.J. Andrews Experimental Forest, Oregon (2024-2025)
#
# Author:  Jasmine Krause (Oregon State University)
# Purpose: Reads raw miniDOT files, corrects sensor offsets, filters bad data,
#          calculates DO saturation, joins climate, discharge, light and
#          distance data, and exports the final dataset used in the manuscript.
#
# Steps
#   1. Read raw miniDOT files
#   2. Correct for intersensor differences using calibration data
#   3. Clean data (flag spikes, remove unreasonable values, apply manual flags)
#   4. Calculate DO saturation (calc_DO_sat from streamMetabolizer)
#   5. Export cleaned DO data (DO_data_clean.csv)
#   6. Join DO with climate, discharge, PAR, and benthic light data
#   7. Identify wet and dry periods
#   8. Reorder columns and export the final dataset (final_data.csv)
#
# Inputs (Raw_data/)
#   DO/*.TXT                          raw miniDOT files
#   DO/Sensorcalibrations/*_sensorcal.TXT   side-by-side sensor calibrations
# Inputs (Processed_data/)
#   Air_pressure_clean.csv, HJA_climate_clean.csv, PAR_clean.csv,
#   lux_data_clean.csv
# Outputs (Processed_data/)
#   DO_data_clean.csv, final_data.csv
#
# Note: all times are America/Los_Angeles (PST/PDT) and rounded to 15 minutes.
################################################################################

#### Setup ####
library(tidyverse)  # includes dplyr, ggplot2, readr, purrr, stringr, lubridate
library(zoo)        # rollapply, rollmean
library(streamMetabolizer)  # calc_DO_sat
library(units)

# Set this to the folder that contains Raw_data/ and Processed_data/
project_dir   <- "/Users/jasminekrause/Documents/GitHub/HJ-Andrews-WY2324"
raw_dir       <- file.path(project_dir, "Raw_data")
processed_dir <- file.path(project_dir, "Processed_data")

tz_pst <- "America/Los_Angeles"

#### Step 1. Read in data ####

read_and_preprocess <- function(filepath, round_to_mins = 15) {
  
  tz_pst <- "America/Los_Angeles"
  
  # Find header
  lines <- readr::read_lines(filepath, n_max = 120)
  header_idx <- which(stringr::str_detect(stringr::str_trim(lines), "^Unix Timestamp"))[1]
  if (is.na(header_idx)) stop("Header not found in: ", filepath)
  
  # Read table
  df <- readr::read_csv(
    filepath,
    skip = header_idx - 1,
    show_col_types = FALSE,
    progress = FALSE,
    trim_ws = TRUE
  )
  
  # Drop units / bad rows
  df <- df %>%
    mutate(unix_time = suppressWarnings(as.numeric(`Unix Timestamp`))) %>%
    filter(!is.na(unix_time))
  
  # Build authoritative time from unix
  df <- df %>%
    mutate(
      PCT_utc = as.POSIXct(unix_time, origin = "1970-01-01", tz = "UTC"),
      PCT     = lubridate::with_tz(PCT_utc, tz_pst),
      PCT     = lubridate::floor_date(PCT, paste0(round_to_mins, " minutes")),
      Day     = as.Date(PCT)
    )
  
  # Extract site + SN
  filename <- basename(filepath)
  site <- stringr::str_extract(filename, "^[^_]+")
  SN   <- stringr::str_extract(filename, "(?<=_DO_)\\d{6}")
  
  df %>%
    transmute(
      site = site,
      SN = SN,
      PCT,
      Day,
      Temp_C     = suppressWarnings(as.numeric(Temperature)),
      DO    = suppressWarnings(as.numeric(`Dissolved Oxygen`)),
      DOT_saturation = suppressWarnings(as.numeric(`Dissolved Oxygen Saturation`)),
      Q          = suppressWarnings(as.numeric(Q))
    ) %>%
    distinct(site, SN, PCT, .keep_all = TRUE) %>%
    arrange(site, PCT)
}

setwd(raw_dir)

base_dir_PMEDO <- "DO"
filepaths <- list.files(path = base_dir_PMEDO, pattern = "\\.TXT$", full.names = TRUE)

all_data_PMEDO <- purrr::map_dfr(filepaths, ~{
  message("Reading: ", .x)
  read_and_preprocess(.x, round_to_mins = 15)
})

# drop dups
all_data_PMEDO <- all_data_PMEDO %>%
  distinct(site, SN, PCT, .keep_all = TRUE)

# Plot raw DO
ggplot(all_data_PMEDO, aes(x = PCT, y = DO, color = site)) +
  geom_line() +
  labs(x = "DateTime", y = "DO, mg/L") +
  theme_minimal() +
  theme(legend.position = "bottom")


#### Step 2. Sensor calibration (intersensor offsets) ####
# Define the function to read calibration data (specific to calibration files)
read_calibration_data <- function(filepath) {
  # Read in the data
  df <- read.csv(filepath, skip = 8)[, 3:6]
  # do not read in DO saturation
  colnames(df) <- c("PCT", "Q", "Temp_C", "DO")
  
  # Extract filename and parse SN only (first 6 digits before _bin)
  filename <- basename(filepath)
  SN <- str_extract(filename, "^\\d{6}")  # e.g., 923301
  
  # Add SN to df
  df$SN <- SN
  
  # Convert PCT to datetime
  df$PCT <- as.POSIXct(df$PCT, format = "%Y-%m-%d %H:%M:%S", tz = tz_pst)
  
  # Round to nearest 10 minutes
  df$PCT <- as.POSIXct(
    round(as.numeric(df$PCT) / 600) * 600,
    origin = "1970-01-01",
    tz = tz_pst
  )
  
  return(df)
}

# Define the path to calibration files
basedir_sensorcal <- "DO/Sensorcalibrations"

# List all calibration files that match the pattern "_sensorcal.TXT"
calibration_files <- list.files(
  path = basedir_sensorcal,
  pattern = "_sensorcal.TXT",
  full.names = TRUE
)

# Dictionary to store calibration data
calibration_data <- list()

# Loop through calibration files
for (cal_filepath in calibration_files) {
  tryCatch({
    # Read and clean the calibration data
    df_cal <- read_calibration_data(cal_filepath)
    
    # Get sensor ID
    sensor_id <- unique(df_cal$SN)
    
    # Append or bind rows if that sensor already exists
    if (sensor_id %in% names(calibration_data)) {
      calibration_data[[sensor_id]] <- bind_rows(
        calibration_data[[sensor_id]],
        df_cal
      )
    } else {
      calibration_data[[sensor_id]] <- df_cal
    }
    
    print(paste("Successfully read calibration file", cal_filepath))
  }, error = function(e) {
    print(paste(
      "Error reading calibration file",
      cal_filepath,
      ":",
      e$message
    ))
  })
}

# Concatenate all cleaned calibration DataFrames into one df
minidot_calibration_data <- bind_rows(calibration_data)

# Check the concatenated data
print(head(minidot_calibration_data))


### Keep only the calibration events and label them
cal_data <- minidot_calibration_data %>%
  dplyr::mutate(
    cal_date = as.Date(PCT),
    Year = lubridate::year(PCT),
    SN = as.character(SN),
    calibration_event = dplyr::case_when(
      cal_date >= as.Date("2024-04-09") & cal_date <= as.Date("2024-04-11") ~ "2024_pre",
      cal_date >= as.Date("2024-11-16") & cal_date <= as.Date("2024-11-17") ~ "2024_post",
      cal_date >= as.Date("2025-05-20") & cal_date <= as.Date("2025-05-21") ~ "2025_pre",
      cal_date == as.Date("2025-11-24") ~ "2025_post",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(calibration_event))


cal_max <- cal_data %>%
  dplyr::group_by(calibration_event, SN) %>%
  dplyr::summarize(
    DO_max = max(DO, na.rm = TRUE),
    .groups = "drop"
  )

cal_offsets <- cal_max %>%
  dplyr::group_by(calibration_event) %>%
  mutate(
    DO_ref = max(DO_max, na.rm = TRUE),
    SN_ref = SN[which.max(DO_max)],
    DO_offset = DO_ref - DO_max
  ) %>%
  ungroup()

cal_offsets_avg <- cal_offsets %>%
  separate(calibration_event, into = c("Year", "Period"), sep = "_") %>%
  mutate(
    Year = as.integer(Year),
    SN = as.character(SN)
  ) %>%
  dplyr::group_by(Year, SN) %>%
  dplyr::summarize(
    DO_offset_avg = mean(DO_offset, na.rm = TRUE),
    n_events = sum(!is.na(DO_offset)),
    .groups = "drop"
  )

# determine a representative year in case there was no calibration
rep_year <- cal_offsets_avg %>%
  dplyr::group_by(Year) %>%
  dplyr::summarize(rep_offset_year = mean(DO_offset_avg, na.rm = TRUE), .groups = "drop")

rep_overall <- mean(cal_offsets_avg$DO_offset_avg, na.rm = TRUE)


# apply correction to do data
field_corrected <- all_data_PMEDO %>%
  mutate(
    PCT = as.POSIXct(PCT, tz = "America/Los_Angeles"),
    Year = year(PCT),
    SN = as.character(SN)
  ) %>%
  left_join(cal_offsets_avg %>% dplyr::select(Year, SN, DO_offset_avg), by = c("Year", "SN")) %>%
  left_join(rep_year, by = "Year") %>%
  mutate(
    DO_offset_used = coalesce(DO_offset_avg, rep_offset_year, rep_overall),
    DO_corrected   = DO + DO_offset_used,
    offset_source  = case_when(
      !is.na(DO_offset_avg) ~ "sensor-year avg (pre+post)",
      is.na(DO_offset_avg) & !is.na(rep_offset_year) ~ "year representative",
      TRUE ~ "overall representative"
    )
  )


field_corrected %>%
  distinct(SN, Year, offset_source, DO_offset_used) %>%
  arrange(offset_source, Year, SN)

field_corrected %>%
  count(offset_source)


# plot
field_corrected %>%
  distinct(SN, Year, DO_offset_used) %>%
  ggplot(aes(reorder(SN, DO_offset_used), DO_offset_used)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  coord_flip() +
  facet_wrap(~Year) +
  theme_classic(base_size = 10) +
  labs(x = "Sensor (SN)", y = "DO offset applied")

field_corrected %>%
  dplyr::select(SN, Year, PCT, DO, DO_corrected) %>%
  pivot_longer(c(DO, DO_corrected), names_to = "type", values_to = "value") %>%
  slice_sample(n = 20000) %>%  # subsample for plotting speed if very large
  ggplot(aes(value, fill = type)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("DO" = "#969696", "DO_corrected" = "#2a78d6"),
                    labels = c("Raw", "Corrected"), name = NULL) +
  theme_classic(base_size = 12) +
  labs(x = "DO (mg/L)", y = "Density")





##
## Clean final dataset (post-calibration)
all_data_postcal <- field_corrected %>%
  dplyr::select(-c(
    DO,
    Q,
    Year,
    DO_offset_avg,
    rep_offset_year,
    DO_offset_used,
    DOT_saturation,
    offset_source
  )) %>%
  dplyr::rename(DO_mg_L = DO_corrected)






#### Step 3. Clean data ####
###### Flag spikes: values >2 units from a 9-point (centered) rolling median
all_data_postcal_1 <- all_data_postcal %>%
  dplyr::group_by(site) %>%
  dplyr::arrange(PCT, .by_group = TRUE) %>%
  dplyr::mutate(
    
    DO_local_median = zoo::rollapply(
      DO_mg_L,
      width = 9,
      FUN = function(x) {
        if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE)
      },
      align = "center",
      fill = NA_real_,
      partial = TRUE
    ),
    
    Temp_local_median = zoo::rollapply(
      Temp_C,
      width = 9,
      FUN = function(x) {
        if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE)
      },
      align = "center",
      fill = NA_real_,
      partial = TRUE
    ),
    
    DO_change = abs(DO_mg_L - DO_local_median),
    Temp_change = abs(Temp_C - Temp_local_median),
    
    DO_flag = dplyr::coalesce(DO_change > 2, FALSE),
    Temp_flag = dplyr::coalesce(Temp_change > 2, FALSE),
    
    DO_clean = dplyr::if_else(DO_flag, DO_local_median, DO_mg_L),
    Temp_clean = dplyr::if_else(Temp_flag, Temp_local_median, Temp_C)
  ) %>%
  dplyr::ungroup()


# Summarize flags by site
noise_summary <- all_data_postcal_1 %>%
  dplyr::group_by(site) %>%
  summarise(
    DO_flags = sum(DO_flag),
    Temp_flags = sum(Temp_flag),
    either_flag = sum(DO_flag | Temp_flag),
    .groups = "drop"
  ) %>%
  arrange(desc(either_flag))

noise_summary


# Plot DO flags
ggplot(all_data_postcal_1, aes(PCT, DO_mg_L)) +
  geom_line(color = "gray60", linewidth = 0.3) +
  geom_point(
    data = all_data_postcal_1 %>% filter(DO_flag),
    color = "red",
    size = 1.5
  ) +
  facet_wrap(~site, scales = "free_y") +
  labs(
    x = "Time",
    y = "DO (mg/L)"
  ) +
  theme_minimal()


# Plot temperature flags
ggplot(all_data_postcal_1, aes(PCT, Temp_C)) +
  geom_line(color = "gray60", linewidth = 0.3) +
  geom_point(
    data = all_data_postcal_1 %>% filter(Temp_flag),
    color = "blue",
    size = 1.5
  ) +
  facet_wrap(~site, scales = "free_y") +
  labs(
    x = "Time",
    y = "Temperature (°C)"
  ) +
  theme_minimal()


## Remove flagged values and unreasonable values (DO outside 0-15 mg/L)
all_data_postcal_filtered <- all_data_postcal_1 %>%
  mutate(
    any_flag = coalesce(DO_flag, FALSE) |
      coalesce(Temp_flag, FALSE)
  ) %>%
  filter(
    !any_flag,
    between(DO_clean, 0, 15),
    !is.na(Temp_clean)
  )

# remove cols
all_data_postcal_filtered <- all_data_postcal_filtered %>%
  dplyr::select(
    -DO_mg_L,-Temp_C,-Temp_change, -DO_change,-DO_flag, -Temp_flag,-any_flag, -Temp_local_median,-DO_local_median
  ) %>%
  dplyr::rename(
    DO_mg_L = DO_clean,Temp_C = Temp_clean
  )

# Plot DO after automated cleaning
ggplot(all_data_postcal_filtered, aes(x = Day, y = DO_mg_L, group = site, color = site)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~ site, scales = "free_y") +
  labs(
    title = "",
    x = "Date",
    y = "Dissolved Oxygen (mg/L)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"  # remove legend since facets already separate sites
  )



##### Rename sensors to site positions (S1-S17, upstream to downstream)
site_to_position <- c(
  "minimeso"      = "S1",
  "miniSS0"       = "S2",
  "SS1"           = "S3",
  "miniSS1"       = "S4",
  "miniSS2"       = "S5",
  "SS2"           = "S6",
  "miniSS3"       = "S7",
  "P1" = "S8",
  "miniSS4"       = "S9",
  "P2" = "S10",
  "miniSS5"       = "S11",
  "miniSS6"       = "S12",
  "miniSS7"     = "S13",
  "SS3"         = "S14",
  "miniSS8"       = "S15",
  # miniSS9 and S14 are the same site but were renamed in diff years
  "miniSS9"       = "S16",
  "S14"         = "S16",
  "SS4"         = "S17"
)


# Apply the mapping to create site_position
all_data_postcal_filtered <- all_data_postcal_filtered %>%
  mutate(
    site = trimws(as.character(site)),
    site_position = unname(site_to_position[site])
  )


### Manual flagging (deployment, storms, lost/dry/out-of-water sensors, noise)
all_data_postcal_filtered <- all_data_postcal_filtered %>%
  filter(
    # Deployment times
    !(site_position %in% c("S3", "S6", "S9","S11","S12","S14","S16","S17") &
        PCT <  as.POSIXct("2024-04-22 11:00:00", tz="America/Los_Angeles")),
    
    !(site_position %in% c("S1", "S2", "S4","S5","S7","S13","S15") &
        PCT <  as.POSIXct("2024-05-05 08:00:00", tz="America/Los_Angeles")),
    
    # Storm event
    !(site_position %in% c("S2", "S5") &
        PCT >  as.POSIXct("2024-11-05 00:00:00", tz="America/Los_Angeles")),
    
    !(site_position %in% c("S6") &
        PCT >  as.POSIXct("2024-11-05 00:00:00", tz="America/Los_Angeles") &
        PCT <  as.POSIXct("2024-11-11 00:00:00", tz="America/Los_Angeles")),
    
    !(site_position %in% c("S4","S7","S9","S13") &
        PCT >  as.POSIXct("2024-11-06 00:00:00", tz="America/Los_Angeles")),
    
    !(site_position %in% c("S12","S15") &
        PCT >  as.POSIXct("2024-11-02 00:00:00", tz="America/Los_Angeles")),
    
    # Lost sensor
    !(site_position %in% c("S14") &
        PCT >  as.POSIXct("2024-12-17 00:00:00", tz="America/Los_Angeles") &
        PCT <  as.POSIXct("2025-04-06 00:00:00", tz="America/Los_Angeles")),
    
    # Dry --> look at time series for P2
    !(site_position %in% c("S10") &
        PCT >= as.POSIXct("2025-07-15 00:00:00", tz="America/Los_Angeles") &
        PCT <= as.POSIXct("2025-10-11 00:00:00", tz="America/Los_Angeles")),
    
    # Random sensor noise
    !(site_position == "S11" &
        PCT >= as.POSIXct("2024-10-16 00:00:00", tz="America/Los_Angeles")),
    
    !(site_position == "S3" &
        PCT >= as.POSIXct("2025-06-10 13:15:00", tz="America/Los_Angeles") &
        PCT <= as.POSIXct("2025-06-10 13:30:00", tz="America/Los_Angeles")),
    
    !(site_position == "S3" &
        PCT >= as.POSIXct("2025-07-08 17:30:00", tz="America/Los_Angeles") &
        PCT <= as.POSIXct("2025-07-08 20:00:00", tz="America/Los_Angeles")),
    
    !(site_position == "S16" &
        PCT >= as.POSIXct("2024-05-26 13:30:00", tz="America/Los_Angeles") &
        PCT <= as.POSIXct("2024-05-26 14:30:00", tz="America/Los_Angeles")),
    
    !(site_position == "S16" &
        PCT >= as.POSIXct("2024-07-03 15:00:00", tz="America/Los_Angeles") &
        PCT <= as.POSIXct("2024-07-03 19:00:00", tz="America/Los_Angeles")),
    
    !(site_position == "S11" &
        PCT >= as.POSIXct("2024-07-21 15:15:00", tz="America/Los_Angeles") &
        PCT <= as.POSIXct("2024-07-21 15:45:00", tz="America/Los_Angeles")),
    
    !(site_position == "S16" &
        PCT >= as.POSIXct("2025-07-17 00:00:00", tz="America/Los_Angeles") &
        PCT <= as.POSIXct("2025-07-26 00:00:00", tz="America/Los_Angeles")),
    
    !(site_position == "S17" &
        PCT >= as.POSIXct("2025-11-21 00:00:00", tz="America/Los_Angeles")),
    
    # taken out of water
    !(site_position == "S6" &
        PCT >= as.POSIXct("2025-12-07 00:00:00", tz="America/Los_Angeles"))
  )


# Plot DO after manual flagging
ggplot(all_data_postcal_filtered, aes(x = Day, y = DO_mg_L, group = site, color = site)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~ site, scales = "free_y") +
  labs(
    title = "",
    x = "Date",
    y = "Dissolved Oxygen (mg/L)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"  # remove legend since facets already separate sites
  )



## Split S13 and S17 into separate site-periods based on drying (.a = before, .b = after)
all_data_postcal_filtered2 <- all_data_postcal_filtered %>%
  mutate(site_position = case_when(
    site_position == "S13" & Day < as.Date("2024-08-01") ~ "S13.a",
    site_position == "S13" & Day >= as.Date("2024-08-29") ~ "S13.b",
    site_position == "S17" & Day < as.Date("2024-06-18") ~ "S17.a",
    site_position == "S17" & Day >= as.Date("2024-06-19") ~ "S17.b",
    TRUE ~ site_position
  )) %>%
  filter(!(site_position == "S13" & Day >= as.Date("2024-08-01") & Day < as.Date("2024-08-29")))






#### Step 4. Calculate DO saturation ####
# Read in cleaned baro data
baro <- read_csv(file.path(processed_dir, "Air_pressure_clean.csv"))

# Join and interpolate over time
all_data_postcal_baro <- all_data_postcal_filtered2 %>%
  mutate(PCT = round_date(PCT, "15 minutes")) %>%
  left_join(baro, by = "PCT")


# Convert units, then immediately drop them to avoid errors
input_df <- tibble(
  temp = drop_units(set_units(all_data_postcal_baro$Temp_C, "degC")),
  pressure = drop_units(set_units(all_data_postcal_baro$P_mbar, "mbar")),
  salinity = rep(0, nrow(all_data_postcal_baro)) 
)

# Apply calc_DO_sat row by row
all_data_postcal_baro$DO_sat_mg_L <- pmap_dbl(input_df, function(temp, pressure, salinity) {
  calc_DO_sat(
    temp.water = temp,
    pressure.air = pressure,
    salinity.water = salinity
  )
})

# Calculate % saturation
all_data_postcal_baro <- all_data_postcal_baro %>%
  mutate(DO_sat_percent = (DO_mg_L / DO_sat_mg_L) * 100)




#### Step 5. Export cleaned DO data ####
all_data_postcal_baro_export <- all_data_postcal_baro %>%
  dplyr::select(-Air_Temp_C, -site) %>%
  dplyr::rename(Water_temp_C = Temp_C) %>%
  dplyr::mutate(
    dplyr::across(
      where(is.numeric),
      ~ round(., 2)
    )
  )

# Export (see note below)
# NOTE: Step 6 reads DO_data_clean.csv. Uncomment the line below the first time
# the script is run to create it.
#write.csv(all_data_postcal_baro_export, file.path(processed_dir, "DO_data_clean.csv"), row.names = FALSE)





#### Step 6. Join all data ####
site_positions <- c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8","S9","S10","S11","S12",
                    "S13.a", "S13.b", "S14", "S15", "S16","S17.a","S17.b")

setwd(processed_dir)

## round all to the nearest 15 min
# Read in cleaned DO data (output of Step 5)
do_data <- read_csv("DO_data_clean.csv") %>%
  mutate(
    PCT = as.POSIXct(PCT, format = "%Y-%m-%d %H:%M:%S"),
    PCT = lubridate::floor_date(PCT, "15 min")
  ) %>%
  dplyr::rename(SN_DO = SN) %>%
  filter(!is.na(site_position)) %>%
  dplyr::select(-SN_DO)

# Remove duplicate rows
do_data <- do_data %>%
  distinct(site_position, PCT, .keep_all = TRUE) %>%
  arrange(PCT)

# Read in climate and discharge data
climate_data <- read_csv("HJA_climate_clean.csv") %>%
  dplyr::rename(PCT = Date) %>%
  mutate(
    PCT = as.POSIXct(PCT, format="%Y-%m-%d %H:%M:%S")
  ) %>%
  mutate(PCT = lubridate::floor_date(PCT, "15 min"))

# Remove duplicate rows
climate_data <- climate_data %>%
  distinct(PCT, .keep_all = TRUE) %>%
  arrange(PCT)

### clean up columns
climate_data_join <- climate_data %>%
  dplyr::select(PCT, ECD_INST_0_0_01, Discharge_L_s, PRECIP_TOT_100_0_01, PRECIP_ACC_100_0_01) %>%
  dplyr::rename(Gage_EC_us_cm = ECD_INST_0_0_01) %>%
  dplyr::rename(Gage_precip_accumulate_mm = PRECIP_ACC_100_0_01) %>%
  dplyr::rename(Gage_precip_total_mm = PRECIP_TOT_100_0_01)

# Read in understory PAR data (S3)
par_ss3 <- read_csv("PAR_clean.csv") %>%
  dplyr::rename(PAR_understory = PAR) %>%
  mutate(
    PAR_understory  = as.numeric(PAR_understory),
    PAR_understory  = if_else(PAR_understory < 0, 0, PAR_understory),
    Air_temp_PAR_C  = as.numeric(Air_temp_PAR_C),
    PCT             = as.POSIXct(PCT, format = "%Y-%m-%d %H:%M:%S"),
    PCT             = floor_date(PCT, "15 min")
  ) %>%
  filter(site_position == "S3") %>%
  dplyr::group_by(PCT) %>%
  dplyr::summarise(
    PAR_understory = mean(PAR_understory, na.rm = TRUE),
    air_temp_par   = mean(Air_temp_PAR_C, na.rm = TRUE),
    .groups        = "drop"
  )

# Remove duplicate rows
par_ss3 <- par_ss3 %>%
  distinct(PCT, .keep_all = TRUE) %>%
  arrange(PCT)



# Read in lux (benthic light) data
# Only one lux sensor per site was kept so the join is one-to-one
lux <- read_csv("lux_data_clean.csv") %>%
  mutate(
    PCT = as.POSIXct(PCT, format="%Y-%m-%d %H:%M:%S")
  ) %>%
  mutate(PCT = lubridate::floor_date(PCT, "15 min")) %>%
  dplyr::select(-deployment_position,-SN_lux)

# Rename sites based on when they dried (matches Step 3 site-periods)
lux <- lux %>%
  dplyr::mutate(
    site_position = dplyr::case_when(
      site_position == "S13" & PCT < as.POSIXct("2024-08-29") ~ "S13.a",
      site_position == "S13" & PCT >= as.POSIXct("2024-08-29") ~ "S13.b",
      site_position == "S17" & PCT < as.POSIXct("2024-08-29") ~ "S17.a",
      site_position == "S17" & PCT >= as.POSIXct("2024-08-29") ~ "S17.b",
      TRUE ~ site_position
    )
  ) %>%
  # drop S3 lux because it was deployed differently from other sites
  dplyr::filter(site_position != "S3")


lux %>%
  count(PCT, site_position) %>%
  filter(n > 1) %>%
  arrange(desc(n))

# Remove duplicate rows
lux <- lux %>%
  distinct(site_position, PCT, .keep_all = TRUE) %>%
  arrange(PCT)


### Nutrients (joined separately in the analysis scripts)
# nut <- read_csv("HJA_nutrient.csv") %>%
#   mutate(
#     Day = as.Date(Day, origin = "1899-12-30")
#   )

# Merge all datasets
final_data <- do_data %>%
  left_join(par_ss3, by = c("PCT")) %>%
  left_join(lux, by = c("PCT","site_position")) %>%
  left_join(climate_data_join, by = "PCT")



# final_data <- final_data %>%
#   left_join(nut, by = c("Day"))

# Ensure PCT is in datetime format
final_data <- final_data %>%
  mutate(PCT = as.POSIXct(PCT, format="%Y-%m-%d %H:%M:%S"))

nrow(final_data)
nrow(distinct(final_data))


# Round numeric columns
final_data <- final_data %>%
  dplyr::mutate(
    dplyr::across(
      c(Gage_EC_us_cm,
        Discharge_L_s,
        air_temp_par,
        benthic_light),
      ~ round(as.numeric(.), 2)
    )
  )

## Assign distance downstream (m) to each site-period
site_distance_lookup <- tribble(
  ~site_position, ~Distance_m,
  "S1",     5,
  "S2",     30,
  "S3",     75,
  "S4",     112,
  "S5",     124.5,
  "S6",     150.5,
  "S7",     167.5,
  "S8",     177.5,
  "S9",     184,
  "S10",    196.5,
  "S11",    211.5,
  "S12",    224,
  "S13.a",  256,
  "S13.b",  261,
  "S14",    275,
  "S15",    309.5,
  "S16",    321.5,
  "S17.a",  340,
  "S17.b",  339
)

# Join distances back
final_data <- final_data %>%
  dplyr::select(-any_of("Distance_m")) %>%
  left_join(site_distance_lookup, by = "site_position")

# View
glimpse(final_data)

### DO deficit
final_data <- final_data %>%
  mutate(
    # Absolute deficit in mg/L
    DO_deficit_mgL = DO_sat_mg_L - DO_mg_L,
    
    # Percent deficit relative to saturation
    DO_deficit_pct = (1 - (DO_mg_L / DO_sat_mg_L)) * 100
  )
summary(final_data$DO_deficit_mgL)
summary(final_data$DO_deficit_pct)


# Order sites
final_data <- final_data %>%
  mutate(site_position = factor(site_position, levels = site_positions)) %>%
  # filter when sensors were pulled
  filter(Day <= as.Date("2025-11-21"))


#### Step 7. Identify wet and dry periods ####
daily <- final_data %>%
  mutate(Day = as.Date(Day)) %>%
  dplyr::group_by(Day) %>%
  dplyr::summarise(
    flow_mean  = mean(Discharge_L_s,      na.rm = TRUE),
    precip_sum = sum(Gage_precip_total_mm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    flow_roll7  = rollmean(flow_mean, k = 7, fill = NA, align = "center"),
    flow_slope  = c(NA, diff(flow_roll7)),
    rain_day    = precip_sum > 2
  )

# last rain before baseflow
wet_to_dry_2024 <- daily %>%
  filter(Day >= as.Date("2024-05-15") & Day <= as.Date("2024-06-20"), rain_day) %>%
  dplyr::summarise(date = max(Day)) %>% pull(date)

wet_to_dry_2025 <- daily %>%
  filter(Day >= as.Date("2025-05-15") & Day <= as.Date("2025-07-05"), rain_day) %>%
  dplyr::summarise(date = max(Day)) %>% pull(date)

# first sustained rise before reconnection
find_first_sustained_rise <- function(data, start, end, min_days = 3) {
  window <- data %>%
    filter(Day >= as.Date(start) & Day <= as.Date(end)) %>%
    mutate(rising = flow_slope > 0)

  rle_result  <- rle(window$rising)
  cum_lengths <- cumsum(rle_result$lengths)
  sustained   <- which(rle_result$values == TRUE & rle_result$lengths >= min_days)[1]

  if (is.na(sustained)) return(NA_Date_)
  start_idx <- ifelse(sustained == 1, 1, cum_lengths[sustained - 1] + 1)
  return(window$Day[start_idx])
}

dry_to_wet_2024 <- find_first_sustained_rise(daily, "2024-09-15", "2024-11-15")
dry_to_wet_2025 <- find_first_sustained_rise(daily, "2025-09-15", "2025-11-22")

# print
cat("=== Wet→Dry (last rain before Low Baseflow) ===\n")
cat("2024:", format(wet_to_dry_2024), "\n")
cat("2025:", format(wet_to_dry_2025), "\n\n")

cat("=== Dry→Wet (first sustained rise at reconnection) ===\n")
cat("2024:", format(dry_to_wet_2024), "\n")
cat("2025:", format(dry_to_wet_2025), "\n")

# plots
plot_window <- function(data, start, end, vline_date = NULL, title) {

  scale_factor <- max(data$flow_mean, na.rm = TRUE) /
    max(data$precip_sum[data$Day >= as.Date(start) &
                          data$Day <= as.Date(end)], na.rm = TRUE)

  p <- data %>%
    filter(Day >= as.Date(start) & Day <= as.Date(end)) %>%
    ggplot(aes(x = Day)) +
    geom_col(aes(y = precip_sum * scale_factor, fill = "Precip (mm)"), alpha = 0.5) +
    geom_line(aes(y = flow_mean,  color = "Discharge (cfs)"),       linewidth = 0.8) +
    geom_line(aes(y = flow_roll7, color = "Discharge (7d roll)"),   linewidth = 1.1, linetype = "dashed") +
    scale_fill_manual(values  = c("Precip (mm)"         = "steelblue")) +
    scale_color_manual(values = c("Discharge (cfs)"     = "gray60",
                                  "Discharge (7d roll)" = "darkblue")) +
    scale_y_continuous(
      name     = "Discharge (cfs)",
      sec.axis = sec_axis(~ . / scale_factor, name = "Precip (mm/day)")
    ) +
    labs(title = title, x = NULL, fill = NULL, color = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")

  if (!is.null(vline_date) && !is.na(vline_date)) {
    p <- p +
      geom_vline(xintercept = as.Date(vline_date),
                 color = "firebrick", linewidth = 1, linetype = "dashed") +
      annotate("text", x = as.Date(vline_date), y = Inf,
               label = format(as.Date(vline_date), "%b %d"),
               vjust = 1.5, hjust = -0.1, color = "firebrick", size = 3.5)
  }
  p
}

p1 <- plot_window(daily, "2024-05-01", "2024-07-15", wet_to_dry_2024,
                  "2024 Wet→Dry: last rain before Low Baseflow")
p2 <- plot_window(daily, "2025-05-01", "2025-08-01", wet_to_dry_2025,
                  "2025 Wet→Dry: last rain before Low Baseflow")
p3 <- plot_window(daily, "2024-09-01", "2024-11-30", dry_to_wet_2024,
                  "2024 Dry→Wet: first sustained rise at reconnection")
p4 <- plot_window(daily, "2025-09-01", "2025-11-30", dry_to_wet_2025,
                  "2025 Dry→Wet: first sustained rise at reconnection")

###### Assign wet and dry periods
final_data <- final_data %>%
  mutate(
    wet_dry = case_when(
      # 2024: Wet → Dry → Wet
      Day >= as.Date("2024-04-22") & Day <= as.Date("2024-06-17") ~ "Wet",  # High Baseflow until last rain
      Day >  as.Date("2024-06-17") & Day <  as.Date("2024-10-24") ~ "Dry",  # Low Baseflow + Disconnected
      Day >= as.Date("2024-10-24") & Day <= as.Date("2025-04-01") ~ "Wet",  # Reconnected + Peak Flow
      
      # 2025: Wet → Dry → Wet
      Day >= as.Date("2025-04-02") & Day <= as.Date("2025-06-22") ~ "Wet",  # High Baseflow until last rain
      Day >  as.Date("2025-06-22") & Day <  as.Date("2025-10-08") ~ "Dry",  # Low Baseflow + Disconnected
      Day >= as.Date("2025-10-08") & Day <= as.Date("2025-11-22") ~ "Wet",  # Reconnected
      
      TRUE ~ NA_character_
    ),
    wet_dry = factor(wet_dry, levels = c("Wet", "Dry"))
  )



#### Step 8. Clean and export ####
## Reorder columns
final_data_export <- final_data %>%
  dplyr::select(
    PCT, Day, site_position, Distance_m, wet_dry, 
    DO_mg_L, DO_sat_percent, DO_deficit_pct,
    Water_temp_C, air_temp_par,
    PAR_understory, benthic_light,
    P_mbar,
    Gage_EC_us_cm, Discharge_L_s, Gage_precip_total_mm, Gage_precip_accumulate_mm
  )


#### Save
write_csv(final_data_export, file.path(processed_dir, "final_data.csv"))


###### End of script.