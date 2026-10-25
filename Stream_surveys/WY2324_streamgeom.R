### Stream geometry
# This script processes and summarizes stream geomorphic data for WS01.
#
# 1. Geomorphic features and substrate:
#    Expands mapped distance ranges to 1-m intervals and combines geomorphic
#    units, substrate type, and large wood into a longitudinal reach dataset.
#
# 2. Elevation survey:
#    Processes the longitudinal streambed elevation survey, visualizes the
#    elevation profile and local slope, and calculates reach-scale slope.
#
# 3. Stream surveys:
#    Cleans and combines WY24 and WY25 longitudinal stream surveys, standardizes
#    width/depth variables, and groups measurements collected over multiple days
#    into common survey dates.
#
# 4. Sensor widths:
#    Cleans and combines WY24 and WY25 wetted-width and water-depth measurements
#    at sensor locations, standardizes site names, exports the combined dataset,
#    and summarizes spatial variation in width and depth across sites.

library(ggplot2)
library(viridis)
library(dplyr)
library(tidyverse)
library(data.table)


#### 1. Geomorphic features and substrate type ####
# Read the df_geom
df_features <- read_csv("/Users/jasminekrause/Documents/GitHub/HJ-Andrews-WY2324/Raw_data/Stream_geom/WS1 23-24 Metadata - Geomorphic features cleaned.csv")

# Function to expand distances
expand_distances <- function(distances) {
  split_distances <- strsplit(as.character(distances), "-")
  expanded_distances <- lapply(split_distances, function(x) {
    if (length(x) == 1) {
      return(as.numeric(x))
    } else if (length(x) == 2) {
      return(seq(as.numeric(x[1]), as.numeric(x[2]), by = 1))
    }
  })
  return(unlist(expanded_distances))
}

# Separate dataframes
wood_df       <- df_features[, c("Distance_wood_m", "Wood")]
geomorphic_df <- df_features[, c("Distance_geom_m", "Geomorphic_unit")]
substrate_df  <- df_features[, c("Distance_substrate_m", "Substrate_type")]

# Expand
wood_df_expanded <- data.frame(
  Distance_wood_m = expand_distances(wood_df$Distance_wood_m),
  Wood = rep(wood_df$Wood,
             sapply(wood_df$Distance_wood_m,
                    function(x) length(expand_distances(x))))
)

geomorphic_df_expanded <- data.frame(
  Distance_geom_m = expand_distances(geomorphic_df$Distance_geom_m),
  Geomorphic_unit = rep(geomorphic_df$Geomorphic_unit,
                        sapply(geomorphic_df$Distance_geom_m,
                               function(x) length(expand_distances(x))))
)

substrate_df_expanded <- data.frame(
  Distance_substrate_m = expand_distances(substrate_df$Distance_substrate_m),
  Substrate_type = rep(substrate_df$Substrate_type,
                       sapply(substrate_df$Distance_substrate_m,
                              function(x) length(expand_distances(x))))
)

# Merge
merged_geom_substrate <- merge(geomorphic_df_expanded,
                               substrate_df_expanded,
                               by.x = "Distance_geom_m",
                               by.y = "Distance_substrate_m",
                               all = TRUE)

final_merged_df <- merge(merged_geom_substrate,
                         wood_df_expanded,
                         by.x = "Distance_geom_m",
                         by.y = "Distance_wood_m",
                         all = TRUE)

colnames(final_merged_df) <- c("Distance_m", "Geomorphic_unit", "Substrate_type", "Wood")

# Melt for plotting
final_merged_df_long <- reshape2::melt(
  final_merged_df,
  id.vars = "Distance_m",
  measure.vars = c("Geomorphic_unit", "Substrate_type", "Wood"),
  variable.name = "Category",
  value.name = "Type"
)

final_merged_df_long_clean <- final_merged_df_long %>%
  filter(!is.na(Type) & !is.na(Category))

# Example plot
ggplot(final_merged_df_long_clean,
       aes(x = Distance_m, y = Type, color = Category, shape = Category)) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~Category, scales = "free_y")

# export to cleaned
# write.csv(final_merged_df_long_clean,"/Users/jasminekrause/Documents/GitHub/HJ-Andrews-WY2324/Processed_data/df_geomorphicfeatures.csv",row.names = FALSE)


#### 2. Elevation survey ####
df_slope <- read_csv("/Users/jasminekrause/Documents/GitHub/HJ-Andrews-WY2324/Raw_data/Stream_geom/HJA WS01 Streambed Elevation 24-06-09 .csv") %>%
  drop_na(Distance)

# plot
ggplot(df_slope,
       aes(x = Distance, y = Streambed_elevation_m)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_x_reverse() +   # if 0 m is downstream; remove if not
  theme_bw() +
  labs(
    x = "Distance along reach (m)",
    y = "Streambed elevation (m)",
    title = "Stitched Longitudinal Elevation Profile"
  )


df_slope_check <- df_slope %>%
  arrange(Distance) %>%
  mutate(
    dZ = Streambed_elevation_m - lag(Streambed_elevation_m),
    dX = Distance - lag(Distance),
    slope = dZ / dX
  )

ggplot(df_slope_check,
       aes(x = Distance, y = slope)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  labs(
    x = "Distance (m)",
    y = "Local slope (m/m)",
    title = "Local Slope Diagnostic"
  )

# reach slope
df_slope_sorted <- df_slope %>%
  arrange(Distance)

z_down <- df_slope_sorted$Streambed_elevation_m[1]
z_up   <- df_slope_sorted$Streambed_elevation_m[nrow(df_slope_sorted)]

L <- max(df_slope_sorted$Distance) - 
  min(df_slope_sorted$Distance)

reach_slope <- (z_up - z_down) / L
reach_slope


# export to cleaned
# write.csv(df_slope,"/Users/jasminekrause/Documents/GitHub/HJ-Andrews-WY2324/Processed_data/df_slope.csv",row.names = FALSE)


#### 3. Stream surveys ####
#### sensors width are already in 25 data but not 24
## WY24 + WY25
df_geom24 <- read_csv("/Users/jasminekrause/Documents/GitHub/HJ-Andrews-WY2324/Raw_data/Stream_geom/WS1 23-24 Metadata - Stream geom processed.csv")
df_geom25 <- read_csv("/Users/jasminekrause/Documents/GitHub/HJ-Andrews-WY2324/Raw_data/Stream_geom/WS1 24-25 Metadata - Stream geom processed.csv")

# clean
df_geom24_clean <- df_geom24 %>%
  dplyr::transmute(
    Date = as.Date(Date),
    Distance_m,
    Wetted_width_m = Wetted_width,
    Water_depth_m,
    Wet_dry,
    Flowing_notflowing,
    Riffle_run_pool
  )

df_geom25_clean <- df_geom25 %>%
  dplyr::transmute(
    Date = as.Date(Date),
    Distance_m,
    Wetted_width_m,
    Water_depth_m,
    Wet_dry,
    Flowing_notflowing,
    Riffle_run_pool
  )

df_geom <- dplyr::bind_rows(df_geom24_clean, df_geom25_clean) %>%
  dplyr::arrange(Date, Distance_m)

# assign survey dates
df_geom <- df_geom %>%
  dplyr::mutate(
    survey = dplyr::case_when(
      Date %in% as.Date(c("2024-05-22","2024-05-23")) ~ as.Date("2024-05-22"),
      Date %in% as.Date(c("2024-06-09","2024-06-10")) ~ as.Date("2024-06-09"),
      Date == as.Date("2024-06-21") ~ as.Date("2024-06-21"),
      Date %in% as.Date(c("2024-06-23","2024-06-24","2024-06-25","2024-06-26")) ~ as.Date("2024-06-23"),
      Date %in% as.Date(c("2024-07-03","2024-07-04","2024-07-06","2024-07-07")) ~ as.Date("2024-07-03"),
      Date == as.Date("2024-07-13") ~ as.Date("2024-07-13"),
      Date == as.Date("2024-07-16") ~ as.Date("2024-07-16"),
      Date == as.Date("2024-08-29") ~ as.Date("2024-08-29"),
      Date == as.Date("2024-09-11") ~ as.Date("2024-09-11"),
      Date == as.Date("2024-10-24") ~ as.Date("2024-10-24"),
      Date == as.Date("2025-05-24") ~ as.Date("2025-05-24"),
      Date == as.Date("2025-06-10") ~ as.Date("2025-06-10"),
      Date == as.Date("2025-08-05") ~ as.Date("2025-08-05"),
      Date == as.Date("2025-08-26") ~ as.Date("2025-08-26"),
      Date == as.Date("2025-11-20") ~ as.Date("2025-11-20"),
      TRUE ~ as.Date(NA)
    )
  )

# export
readr::write_csv(
  df_geom,
  "/Users/jasminekrause/Documents/GitHub/HJ-Andrews-WY2324/Processed_data/stream_geom_surveys.csv"
)


#### 4. Sensor widths ####
width_24 <- read_csv("/Users/jasminekrause/Documents/GitHub/HJ-Andrews-WY2324/Raw_data/Stream_geom/WS1 23-24 Metadata - Sensor widths.csv")
width_25 <- read_csv("/Users/jasminekrause/Documents/GitHub/HJ-Andrews-WY2324/Raw_data/Stream_geom/WS1 24-25 Metadata - Stream_widths.csv")

site_crosswalk <- tibble::tibble(
  site = c("minimeso","miniSS0","SS1","miniSS1","miniSS2","SS2","miniSS3","P1","miniSS4","P2","miniSS5","miniSS6","miniSS7.a","miniSS7.b","SS3","miniSS8","miniSS9","SS4.a","SS4.b"),
  site_position = c("S1","S2","S3","S4","S5","S6","S7","S8","S9","S10","S11","S12","S13.a","S13.b","S14","S15","S16","S17.a","S17.b")
)

width_24_clean <- width_24 %>%
  dplyr::transmute(
    Date = as.Date(Date),
    site,
    Distance_m = NA_real_,
    Wetted_width_m = Wetted_width,
    Water_depth_m = Depth_m
  )

width <- dplyr::bind_rows(width_24_clean, width_25) %>%
  dplyr::left_join(site_crosswalk, by = "site") %>%
  dplyr::select(Date, site_position, Wetted_width_m, Water_depth_m)

# export
write_csv(
  width,
  "/Users/jasminekrause/Documents/GitHub/HJ-Andrews-WY2324/Processed_data/width_depth_surveys.csv"
)

## plot
width <- width %>%
  dplyr::mutate(site_position = factor(site_position, levels = site_distance_order))

p_width <- width %>%
  dplyr::filter(!is.na(site_position), is.finite(Wetted_width_m)) %>%
  ggplot(aes(site_position, Wetted_width_m)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.4) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.5) +
  labs(x = "Site", y = "Wetted width (m)") +
  theme_classic()

p_depth <- width %>%
  dplyr::filter(!is.na(site_position), is.finite(Water_depth_m)) %>%
  ggplot(aes(site_position, Water_depth_m)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.4) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.5) +
  labs(x = "Site", y = "Water depth (m)") +
  theme_classic()

p_width
p_depth



#### End of script.

