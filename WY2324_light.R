## Script to process lux and PAR data
# Step 1. Load data lux
# Step 2. Filter out flagged data from sensor downloads and adjust site names accordingly
# Step 3. Create flagging for diel variation in temperature and pressure
# Step 4. 

# Step 1. Load PAR

library(ggplot2)
library(tidyverse)
library(viridis)
library(dplyr)

#### Step 1. ####
setwd("/Users/jasminekrause/Documents/GitHub/HJ-Andrews-WY2324/Sensor_downloads")

# Define the base directory containing the data
base_dir_lux <- 'Lux/Raw'

# List all CSV files in the base directory
filepaths <- list.files(path = base_dir_lux, pattern = "\\.csv$", full.names = TRUE)

# Initialize an empty list to store data frames
data_frames_lux <- list()

# Loop through the file paths, read data, and store in the list
for (filepath in filepaths) {
  tryCatch({
    # Extract site, data type, deployment position, serial number, and date from the file name
    filename <- basename(filepath)
    parts <- unlist(strsplit(filename, "_"))
    site <- parts[1]         # Assumes the first part is the site, e.g., 'miniSS6'
    data_type <- parts[2]    # Assumes the second part is the data type, e.g., 'lux'
    deployment_position <- parts[3]  # Deployment position in the third part
    serial_number <- parts[4]  # Assumes the fourth part is the serial number
    date <- substr(parts[5], 1, 8)  # Assumes the fifth part is the date, e.g., '20241109'
    
    print(paste("Site:", site, "Data Type:", data_type, "Serial Number:", serial_number, "Date:", date))
    
    # Read the data into a data frame, skipping the first row (metadata)
    df <- read.csv(filepath, skip = 1)
    
    # Store the raw data frame and associated metadata for cleaning later
    data_frames_lux[[paste0(site, "_", data_type, "_", serial_number)]] <- list(df = df, site = site, data_type = data_type, deployment_position = deployment_position, serial_number = serial_number, date = date)
    print(paste("Successfully read", filepath))
  }, error = function(e) {
    print(paste("Error reading", filepath, ":", e$message))
  })
}

# Data cleaning loop: process each data frame stored in the list
for (key in names(data_frames_lux)) {
  # Access the data frame and associated metadata
  data_info <- data_frames_lux[[key]]
  df <- data_info$df
  site <- data_info$site
  data_type <- data_info$data_type
  deployment_position <- data_info$deployment_position
  serial_number <- data_info$serial_number
  date <- data_info$date
  
  # Remove the first row (assuming the first row is not part of the actual data)
  df <- df[-1, ]
  
  # Dynamically handle the number of columns (7, 8, or 9)
  if (ncol(df) == 7) {
    colnames(df) <- c("Row", "PCT", "Temp_F", "Intensity_lum_ft2", "Host_Connected", "Coupler_Attached", "End_Of_File")
  } else if (ncol(df) == 8) {
    colnames(df) <- c("Row", "PCT", "Temp_F", "Intensity_lum_ft2", "Coupler_Attached", 
                      "Coupler_Detached", "Host_Connected", "End_Of_File")
  } else if (ncol(df) == 9) {
    colnames(df) <- c("Row", "PCT", "Temp_F", "Intensity_lum_ft2", "Coupler_Attached", 
                      "Coupler_Detached", "Host_Connected", "Stopped", "End_Of_File")
  } else {
    stop("Unexpected number of columns in file:", key)
  }
  
  # Remove unwanted columns
  unwanted_cols <- c("Coupler_Detached", "Coupler_Attached", "Host_Connected", "Stopped", "End_Of_File")
  df <- df[, !(colnames(df) %in% unwanted_cols)]
  
  # Rename 'PCT' column to reflect 'PCT'
  colnames(df)[colnames(df) == "PCT"] <- "PCT"
  
  # Add a new 'Temp_C' column (converting Temp_F to Celsius)
  df$Temp_C <- (df$Temp_F - 32) * 5 / 9
  
  # Add site, data type, serial number, deployment position, and download_date to the data frame
  df$site <- site
  df$data_type <- data_type
  df$deployment_position <- deployment_position
  df$serial_number <- serial_number
  df$download_date <- date  # Renaming 'date' to 'download_date'
  
  # Convert 'PCT' column to DateTime type
  df$PCT <- as.POSIXct(df$PCT, format = "%m/%d/%y %I:%M:%S %p", tz = "UTC")
  
  # Store the cleaned data frame back in the list with the same key
  data_frames_lux[[key]] <- df
  print(paste("Successfully cleaned", key))
}

# Check if any data frames were created and cleaned
if (length(data_frames_lux) == 0) {
  print("No data frames were created. Please check the file contents and format.")
} else {
  # Combine all cleaned data frames into a single data frame
  all_data_lux_combined <- do.call(rbind, data_frames_lux)
  
  # Ensure the 'PCT' column is correctly parsed as DateTime
  all_data_lux_combined$PCT <- as.POSIXct(all_data_lux_combined$PCT, format = "%m/%d/%y %I:%M:%S %p", tz = "UTC")
}

# Plotting Intensity (lux) from all sites over time
ggplot(all_data_lux_combined, aes(x = PCT, y = Intensity_lum_ft2, color = site)) +
  geom_line() +
  labs(
    x = "DateTime",
    y = "Intensity (lux/ft²)",
    title = "Intensity from All Sites Over Time",
    color = "Site"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "top") +
  theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_color_discrete(name = "Site") +
  theme(panel.grid.major = element_line(color = "gray", size = 0.5), panel.grid.minor = element_blank())




#### STEP 2: Adjusted for Lux Data ####
# Read in the manual flagging CSV for lux data
manual_flag_lux <- read.csv("Lux/Sensor cleaning and filtering WY23-24 - light.csv")

# Ensure the date columns are in Date format but keep the time component for PCT
manual_flag_lux$Visual_flagdate <- as.POSIXct(manual_flag_lux$Visual_flagdate, format = "%Y-%m-%d")
all_data_lux_combined$PCT <- as.POSIXct(all_data_lux_combined$PCT, format = "%Y-%m-%d %H:%M:%S")

# Strip suffix from manual_flag_lux$Site (everything after the underscore)
manual_flag_lux$Site_base <- sub("_.*", "", manual_flag_lux$site)  # Removes everything after the first underscore

# Add the 'flag1' column with initial value NA
all_data_lux_combined$flag1 <- NA

# Iterate through each site change (Code == "SM") for lux data
for (i in 1:nrow(manual_flag_lux)) {
  # Only handle rows where Code is "SM"
  if (manual_flag_lux$Code[i] == "SM") {
    # Extract relevant values
    site_to_change <- manual_flag_lux$Site_base[i]  # Base site without suffix
    site_name_change <- manual_flag_lux$New_site[i]  # New site
    flag_date <- manual_flag_lux$Visual_flagdate[i]  # Visual flag date
    
    # Debugging: Print the values we're working with
    print(paste("Processing site:", site_to_change, "->", site_name_change))
    print(paste("Flag date:", flag_date))
    
    # Match rows in all_data_lux_combined that contain the base site name
    matching_rows <- grepl(site_to_change, all_data_lux_combined$site)  # Match base site name
    
    # Update the site name based on date and match condition
    all_data_lux_combined$site[matching_rows & all_data_lux_combined$PCT < flag_date] <- paste0(site_to_change, "_1")  # Before flag date
    all_data_lux_combined$site[matching_rows & all_data_lux_combined$PCT >= flag_date] <- paste0(site_name_change, "_2")  # After flag date
    
    # Flag the rows with 'flag1' as "Flagged for Site Change" after the flag date
    all_data_lux_combined$flag1[matching_rows & all_data_lux_combined$PCT >= flag_date] <- "Flagged for Site Change"
  }
}

# Check the updated data with flag1 column
print(head(all_data_lux_combined))



#### STEP 3 ####
sensor_metadata <- read.csv("Lux/Sensor downloads, stream geom + sample metadata WS1 23-24 - Sensor download dates.csv")

# Filter for Lux sensors deployed in water
lux_sensors <- sensor_metadata %>%
  filter(Air.Water == "Water" & grepl("Lux", Sensor))  # Filter Lux sensors deployed in water

# Display the Lux sensors to confirm
print(lux_sensors)

# Define the Lux sensors deployed in water for filtering
# You can manually input the serial numbers or filter from the dataset
lux_sensors_serials <- lux_sensors$SN

# Assuming `all_data_lux_combined` is your light data frame
# Filter the light data based on SN from the metadata
all_data_lux_combined_filtered <- all_data_lux_combined %>%
  filter(serial_number %in% lux_sensors_serials)

# Now that we have filtered data, we can calculate daily temperature variations and flag outliers
# Convert 'PCT' to Date format (if not already)
all_data_lux_combined_filtered$Date <- as.Date(all_data_lux_combined_filtered$PCT)

# Calculate daily temperature variation for each site in filtered light data
daily_temp_variation_lux <- all_data_lux_combined_filtered %>%
  group_by(site, Date) %>%
  summarise(daily_temp_variation = max(Temp_F, na.rm = TRUE) - min(Temp_F, na.rm = TRUE)) %>%
  ungroup()

# Flag days with temperature variation greater than 10°F (for light sensors)
all_data_lux_combined_filtered <- left_join(all_data_lux_combined_filtered, daily_temp_variation_lux, by = c("site", "Date")) %>%
  mutate(flag1 = ifelse(daily_temp_variation > 10, "Temp Variation > 10°F", NA))

# Check the filtered and flagged data
print(head(all_data_lux_combined_filtered))





