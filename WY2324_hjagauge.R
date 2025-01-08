#### Background data for cleaning HJ Andrews data 
# There are long-term data sets, but this script is intended to only analyze 
# data during field period times for WY 23-24
# The v-notch was not put on this year so we can use a single rating curve eq for flow
# note: WS1 rating curve is different from WS 3
# Created 2025-01-08 JAK


library(ggplot2)
library(tidyverse)
library(dplyr)
library(readr)


#### 1. Flow ####
setwd("/Users/jasminekrause/Documents/GitHub/HJ-Andrews-WY2324/Sensor_downloads/hjadata")

# Read the CSV files
discharge24 <- read_csv("Discharge/gsws01_115_5min_2024.csv", skip = 2)
discharge25 <- read_csv("Discharge/gsws01_115_5min_2025.csv", skip = 2)

# Merge the two data frames by their common columns (assuming they have the same structure)
merged_discharge <- bind_rows(discharge24, discharge25)

# Convert 'STAGE_INST_0_0_01' to numeric and replace non-numeric values with NA
merged_discharge$STAGE_INST_0_0_01 <- as.numeric(merged_discharge$STAGE_INST_0_0_01)

# Convert 'Date' (Date column) to POSIXct for Date comparison
merged_discharge$Date <- as.POSIXct(merged_discharge$Date, format="%Y-%m-%d %H:%M:%S")

# WS1 rating curve
# Define your rating equation parameters for each set
rating_params_A <- list(
  c(1.6549710, 1.0780520, 0.0, 0.0, 0.020),
  c(3.4585459, 1.5371380, 0.0, 0.0, 0.076),
  c(2.8872161, 1.3155500, 0.0, 0.0, 0.274),
  c(3.3595591, 1.6805220, 0.0, 0.0, 2.0000)
)

rating_params_B <- list(
  c(3.4810755, 1.6658920, 0.0, 0.0, 0.041),
  c(1.7674992, 1.1277280, 0.0, 0.0, 0.085),
  c(3.4640393, 1.8171530, 0.0, 0.0, 0.113),
  c(2.4405980, 1.3469430, 0.0, 0.0, 0.186),
  c(3.4657080, 1.9568490, 0.0, 0.0, 0.342),
  c(3.0569163, 1.5761290, 0.0, 0.0, 0.701),
  c(3.2120695, 2.0129220, 0.0, 0.0, 2.0000)
)

rating_params_D <- list(
  c(4.3900970, 2.1141930, 0.0, 0.0, 0.045),
  c(2.4261690, 1.4800410, 0.0, 0.0, 0.186),
  c(3.1040300, 1.8834030, 0.0, 0.0, 2.0000)
)

rating_sets <- list(
  set_A = rating_params_A,
  set_B = rating_params_B,
  set_D = rating_params_D
)

# Function to select rating parameters based on the Date
get_rating_params <- function(Date) {
  # Check if Date is NA
  if (is.na(Date)) {
    return(NULL)  # Return NULL if the Date is missing
  }
  
  start_Date <- as.POSIXct('2023-10-01 00:05:00', format="%Y-%m-%d %H:%M:%S")
  end_Date <- as.POSIXct('2025-01-01 13:20:00', format="%Y-%m-%d %H:%M:%S")
  
  # Ensure the Date is within the specified range
  if (Date >= start_Date & Date <= end_Date) {
    if (Date >= as.POSIXct('2023-10-01', format="%Y-%m-%d") & Date <= as.POSIXct('2024-08-31', format="%Y-%m-%d")) {
      return(rating_sets$set_A)
    } else if (Date >= as.POSIXct('2024-09-01', format="%Y-%m-%d") & Date <= as.POSIXct('2024-12-31', format="%Y-%m-%d")) {
      return(rating_sets$set_B)
    } else if (Date >= as.POSIXct('2025-01-01', format="%Y-%m-%d")) {
      return(rating_sets$set_D)
    }
  }
  return(NULL)
}

# Function to calculate discharge based on gauge height
calculate_discharge <- function(gage_ht, params) {
  if (is.na(gage_ht) || !is.numeric(gage_ht) || gage_ht <= 0) {
    return(NA)  # Return NA if the gauge height is invalid
  }
  
  ln_a <- params[1]
  b <- params[2]
  c <- params[3]
  d <- params[4]
  max_gage_ht <- params[5]
  
  # Apply the rating curve equation
  ln_x <- log(gage_ht)
  ln_y <- log(ln_a) + (b * ln_x) + (c * (ln_x^2)) + (d * (ln_x^3))
  
  return(exp(ln_y))  # Return discharge in cfs
}

# Function to apply the rating curve based on the intervals
apply_rating_curve <- function(row) {
  gage_ht <- row$STAGE_INST_0_0_01
  params <- get_rating_params(row$Date)
  
  if (!is.null(params)) {
    for (param_set in params) {
      if (gage_ht <= param_set[5]) {  # Apply the corresponding rating curve for the gauge height interval
        return(calculate_discharge(gage_ht, param_set))
      }
    }
  }
  return(NA)  # Return NA if the gauge height is outside the defined intervals
}

# Use rowwise() from dplyr to apply function row by row
merged_discharge <- merged_discharge %>%
  rowwise() %>%
  mutate(Discharge_cfs = apply_rating_curve(cur_data())) %>%
  ungroup()

# Convert discharge from cubic feet per second to liters per second
merged_discharge$Discharge_L_s <- merged_discharge$Discharge_cfs * 28.3168


### Plotting
# Create a dataset with flagged discharge data
flagged_discharge <- merged_discharge %>%
  filter(!is.na(Flag_STAGE_INST_0_0_01))  # Keep rows where Flag_STAGE_INST_0_0_01 is not NA

# Create a dataset with flagged EC data
flagged_ec <- merged_discharge %>%
  filter(!is.na(Flag_EC_INST_0_0_01))  # Keep rows where Flag_EC_INST_0_0_01 is not NA

# Create a dataset with flagged temperature data
flagged_temp <- merged_discharge %>%
  filter(!is.na(Flag_WATERTEMP_MEAN_0_0_01))  # Keep rows where Flag_WATERTEMP_MEAN_0_0_01 is not NA

# Save the plot to a high-quality PNG file
png("flagged_data_plot.png", width = 1000, height = 1000, res = 150)  # Increased resolution for better quality

# Set up a plot with three panels (Discharge, EC, and Temperature)
par(mfrow = c(3, 1), mar = c(4, 4, 2, 6))  # Adjust margins to fit the legend on the right side

# Plot discharge, with flagged discharge data highlighted
plot(merged_discharge$Date, merged_discharge$Discharge_cfs, 
     type = "l",  # Line plot for the full data
     xlab = "Time", 
     ylab = "Discharge (cfs)",
     main = "Discharge with Flagged Data",
     col = "black",  # Color for the full data
     lwd = 2,  # Line width for the full data
     cex.axis = 1.2,  # Increase axis text size
     cex.lab = 1.5,   # Increase label size
     cex.main = 1.5)  # Increase title size

# Highlight the flagged discharge data in red
points(flagged_discharge$Date, flagged_discharge$Discharge_cfs, 
       col = "red", pch = 19)

# Plot temperature, with flagged temperature data highlighted
plot(merged_discharge$Date, merged_discharge$WATERTEMP_MEAN_0_0_01, 
     type = "l",  # Line plot for the full data
     xlab = "Time", 
     ylab = "Temperature (°C)",
     main = "Temperature with Flagged Data",
     col = "black",  # Color for the full data
     lwd = 2,  # Line width for the full data
     cex.axis = 1.2,  # Increase axis text size
     cex.lab = 1.5,   # Increase label size
     cex.main = 1.5)  # Increase title size

# Highlight the flagged temperature data in green
points(flagged_temp$Date, flagged_temp$WATERTEMP_MEAN_0_0_01, 
       col = "green", pch = 19) 

# Plot EC, with flagged EC data highlighted
plot(merged_discharge$Date, merged_discharge$EC_INST_0_0_01, 
     type = "l",  # Line plot for the full data
     xlab = "Time", 
     ylab = "EC (us/cm)",
     main = "EC with Flagged Data",
     col = "black",  # Color for the full data
     lwd = 2,  # Line width for the full data
     cex.axis = 1.2,  # Increase axis text size
     cex.lab = 1.5,   # Increase label size
     cex.main = 1.5)  # Increase title size

# Highlight the flagged EC data in blue
points(flagged_ec$Date, flagged_ec$EC_INST_0_0_01, 
       col = "blue", pch = 19) 

# Close the PNG device to save the plot
dev.off()






#### 2.CHEMISTRY ####
setwd("/Users/jasminekrause/Documents/GitHub/HJ-Andrews-WY2324/Sensor_downloads/hjadata")


