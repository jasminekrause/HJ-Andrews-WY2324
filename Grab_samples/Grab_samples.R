### WS01 grab sample processing: WY24–25
#
# This script cleans and combines water chemistry, stable isotope, radon,
# and EEM/DOM optical measurements collected from surface water and groundwater
# in WS01 during WY24–25.
#
# Workflow:
# 1. Read raw isotope, radon, chemistry, EEM, and probe datasets.
# 2. Standardize dates and site names and average duplicate measurements.
# 3. Reshape chemistry data so each constituent is stored as a separate column.
# 4. Clean and retain selected DOM optical metrics from EEM measurements.
# 5. Merge datasets by sampling date and site.
# 6. Calculate SUVA254 from absorbance and DOC.
# 7. Assign standardized surface-water site positions and identify groundwater wells.
# 8. Export one combined grab-sample dataset for subsequent analyses.
#
# Final output:
# Processed_data/grab_samples.csv

library(tidyverse)


#### 1. Read raw data ####

iso <- read_csv("/Users/jasminekrause/Documents/GitHub/HJ-Andrews-WY2324/Raw_data/Rn_Iso_Nutrient/WS1 24-25 Metadata - Isotope.csv")
rn <- read_csv("/Users/jasminekrause/Documents/GitHub/HJ-Andrews-WY2324/Raw_data/Rn_Iso_Nutrient/WS1 24-25 Metadata - Radon.csv")
probe <- read_csv("/Users/jasminekrause/Documents/GitHub/HJ-Andrews-WY2324/Raw_data/Rn_Iso_Nutrient/WS1 24-25 Metadata - Probe measurements.csv")
nutrient <- read_csv("/Users/jasminekrause/Documents/GitHub/HJ-Andrews-WY2324/Raw_data/Rn_Iso_Nutrient/WS1 24-25 Metadata - Chemistry.csv")
eem <- read_csv("/Users/jasminekrause/Documents/GitHub/HJ-Andrews-WY2324/Raw_data/Rn_Iso_Nutrient/WS1 24-25 Metadata - EEMs.csv")


#### 2. Clean individual datasets ####

# Stable isotopes
# Remove "_dup" from duplicate sample IDs and average replicate measurements.
iso_clean <- iso %>%
  dplyr::mutate(
    Date = as.Date(Date),
    CS_Site = stringr::str_remove(CS_Site, "_dup$")
  ) %>%
  dplyr::rename(Type = `Type (Surface, Groundwater, Sediment)`) %>%
  dplyr::group_by(Date, CS_Site, Type) %>%
  dplyr::summarise(
    dplyr::across(c(dD_avg, dD_sd, d18O_avg, d18O_sd, Dexcess), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# Radon
#### Correct Radon values 
lambda <- log(2) / (3.8235 * 24)   # Rn-222 decay constant, per hour

# Average replicate radon measurements and associated channel measurements.
rn_corr <- rn %>%
  mutate(
    Date    = as.Date(Date),
    CS_Site = str_remove(CS_Site, "_dup$"),
    t_collected = parse_date_time(paste(Date, Time_collected),
                                  orders = c("Ymd HM", "Ymd HMS"), tz = "America/Los_Angeles", quiet = TRUE),
    t_run       = parse_date_time(Time_run,
                                  orders = c("mdY HM", "mdY IMS p", "Ymd IMS p", "Ymd HMS", "Ymd HM"),
                                  tz = "America/Los_Angeles", quiet = TRUE),
    hours_held  = as.numeric(difftime(t_run, t_collected, units = "hours")),
    decay_factor = exp(lambda * hours_held),
    Radon_raw   = Radon_pCi_L,
    Radon_pCi_L = Radon_raw * decay_factor,
    SD          = SD * decay_factor
  ) %>%
  filter(!is.na(hours_held))

# select only cols needed
rn_clean <- rn_corr %>%
  select(Date, CS_Site, Radon_pCi_L, Radon_raw, SD, Width_m, Depth_m, Area_m2, hours_held)


# Water chemistry
# Convert reported concentrations to numeric values and average replicate samples.
nutrient_clean <- nutrient %>%
  dplyr::mutate(
    Date = as.Date(Date_collected),
    CS_Site = stringr::str_remove(CS_Site, "_dup$"),
    Concentration_mg_L = readr::parse_number(as.character(Concentration_mg_L))
  )

# Convert chemistry from long to wide format: one row per date/site.
nutrient_wide <- nutrient_clean %>%
  dplyr::filter(!is.na(CS_Site)) %>%
  dplyr::group_by(Date, CS_Site, Nutrient) %>%
  dplyr::summarise(
    Concentration_mg_L = mean(Concentration_mg_L, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = Nutrient,
    values_from = Concentration_mg_L,
    names_glue = "{Nutrient}_mg_L"
  )

# EEM / DOM optical properties
# Retain selected absorbance and fluorescence metrics and average replicates.
eem_clean <- eem %>%
  dplyr::transmute(
    Date = as.Date(Day),
    CS_Site,
    abs254,
    int_abs = intabs300400,
    slope_275_295 = slope279295,
    slope_350_400 = slope350400,
    slope_ratio = `slope ratio`,
    slope_300_700 = slope300700,
    flour_index = `Flour index`,
    peakA,
    peakC,
    peakT,
    int_flour = `integrated flour`
  ) %>%
  dplyr::group_by(Date, CS_Site) %>%
  dplyr::summarise(
    dplyr::across(dplyr::where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )


#### 3. Merge grab-sample datasets ####

# Full joins retain samples even when not all measurements were collected
# on the same date or at the same site.
final_merged <- nutrient_wide %>%
  dplyr::full_join(iso_clean, by = c("Date", "CS_Site")) %>%
  dplyr::full_join(rn_clean, by = c("Date", "CS_Site")) %>%
  dplyr::full_join(eem_clean, by = c("Date", "CS_Site"))

# Calculate specific UV absorbance at 254 nm.
# Assumes abs254 is dimensionless absorbance measured with a 1-cm path length.
final_merged <- final_merged %>%
  dplyr::mutate(
    SUVA254 = dplyr::if_else(
      DOC_mg_L > 0,
      (abs254 * 100) / DOC_mg_L,
      NA_real_
    )
  )


#### 4. Standardize site names and water type ####

# Crosswalk between chemistry sampling IDs and standardized sensor positions.
# Distance is measured longitudinally along the study reach.
surface_lookup <- tibble::tribble(
  ~CS_Site, ~site_position, ~Distance_m,
  "CS01", "S1",      5,
  "CS02", "S2",     30,
  "CS03", "S3",     75.5,
  "CS05", "S5",    124.5,
  "CS06", "S6",    150.5,
  "CS07", "S7",    167.5,
  "CS08", "S8",    177.5,
  "CS09", "S9",    184,
  "CS10", "S10",   196.5,
  "CS11", "S11",   211.5,
  "CS12", "S12",   224,
  "CS14", "S13.b", 261,
  "CS15", "S14",   275,
  "CS16", "S15",   309.5,
  "CS17", "S16",   321.5,
  "CS20", "S17.b", 339
)

# Identify groundwater wells from sampling IDs beginning with "W".
# Wells retain their original IDs because they do not correspond to sensor sites.
wells_lookup <- final_merged %>%
  dplyr::filter(stringr::str_detect(CS_Site, "^W")) %>%
  dplyr::distinct(CS_Site) %>%
  dplyr::mutate(
    site_position = CS_Site,
    Distance_m = NA_real_
  ) %>%
  dplyr::select(CS_Site, site_position, Distance_m)

site_lookup <- dplyr::bind_rows(surface_lookup, wells_lookup)

# Add standardized site information, classify samples as surface water or
# groundwater, and retain variables used in subsequent analyses.
final_merged <- final_merged %>%
  dplyr::left_join(site_lookup, by = "CS_Site") %>%
  dplyr::mutate(
    water_type = dplyr::case_when(
      stringr::str_detect(CS_Site, "^W") ~ "Groundwater",
      !stringr::str_detect(CS_Site, "^W") ~ "Surface",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::select(
    Date, site_position, Distance_m, water_type,
    ALK_mg_L, DIC_mg_L, DOC_mg_L, NH3_mg_L, NO3_mg_L, PO4_mg_L,
    CA_mg_L, Cl_mg_L, K_mg_L, MG_mg_L, SO4_mg_L, TN_mg_L, TP_mg_L, NA_mg_L,
    dD_avg, dD_sd, d18O_avg, d18O_sd, Dexcess, Radon_pCi_L,
    abs254, int_abs, slope_275_295, slope_350_400, slope_ratio,
    slope_300_700, SUVA254, flour_index, peakA, peakC, peakT, int_flour
  ) %>%
  dplyr::filter(!is.na(site_position))



#### 5. Export processed dataset ####

readr::write_csv(
  final_merged %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ signif(.x, 2))),
  "/Users/jasminekrause/Documents/GitHub/HJ-Andrews-WY2324/Processed_data/grab_samples.csv"
)
