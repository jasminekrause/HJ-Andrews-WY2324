## Script to process water level and air pressure data
# Step 1. Load data  
# Step 2. Filter out flagged data from sensor downloads and adjust site names accordingly
# Step 3. Create flagging for diel variation in temperature and pressure
# Step 4. 

library(ggplot2)
library(tidyverse)
library(viridis)
library(dplyr)


#### Step 1. ####
setwd("/Users/jasminekrause/Documents/GitHub/HJ-Andrews-WY2324/Sensor_downloads")

# Define the base directory containing the data
base_dir_wl <- 'WL/Raw_data'

# List all CSV files in the base directory
filepaths <- list.files(path = base_dir_wl, pattern = "\\.csv$", full.names = TRUE)

# Initialize an empty list to store data frames
data_frames_wl <- list()

# Loop through the file paths, read data, and store in the list
for (filepath in filepaths) {
  tryCatch({
    # Extract site, data type, and serial number from the file name
    filename <- basename(filepath)
    parts <- unlist(strsplit(filename, "_"))
    site <- parts[1]  # Assumes the first part is the site, e.g., 'miniSS6'
    data_type <- parts[2]  # Assumes the second part is the data type, e.g., 'WL'
    serial_number <- parts[3]  # Assumes the third part is the serial number, e.g., '21836724'
    date <- substr(parts[4], 1, 8)  # Assumes the fourth part is the date, e.g., '20241109'
    
    print(paste("Site:", site, "Data Type:", data_type, "Serial Number:", serial_number, "Date:", date))
    
    # Read the data into a data frame, skipping the first two rows to avoid the plot title and metadata
    df <- read.csv(filepath, skip = 2)
    
    # Store the raw data frame and associated metadata for cleaning later
    data_frames_wl[[paste0(site, "_", data_type, "_", serial_number)]] <- list(df = df, site = site, data_type = data_type, serial_number = serial_number, date = date)
    print(paste("Successfully read", filepath))
  }, error = function(e) {
    print(paste("Error reading", filepath, ":", e$message))
  })
}

# Data cleaning loop: process each data frame stored in the list
for (key in names(data_frames_wl)) {
  # Access the data frame and associated metadata
  data_info <- data_frames_wl[[key]]
  df <- data_info$df
  site <- data_info$site
  data_type <- data_info$data_type
  serial_number <- data_info$serial_number
  date <- data_info$date
  
  # Remove the first row (assuming the first row is not part of the actual data)
  df <- df[-1, ]
  
  # Dynamically handle the number of columns (8 or 9)
  if (ncol(df) == 8) {
    colnames(df) <- c("Row", "DateTime", "Abs_Pressure_psi", "Temp_F", "Coupler_Detached", 
                      "Coupler_Attached", "Host_Connected", "End_Of_File")
  } else if (ncol(df) == 9) {
    colnames(df) <- c("Row", "DateTime", "Abs_Pressure_psi", "Temp_F", "Coupler_Detached", 
                      "Coupler_Attached", "Host_Connected", "Stopped", "End_Of_File")
  } else {
    stop("Unexpected number of columns in file:", key)
  }
  
  # Remove unwanted columns
  unwanted_cols <- c("Coupler_Detached", "Coupler_Attached", "Host_Connected", "Stopped", "End_Of_File")
  df <- df[, !(colnames(df) %in% unwanted_cols)]
  
  # Rename 'DateTime' to 'PCT'
  colnames(df)[colnames(df) == "DateTime"] <- "PCT"
  
  # Add a new 'Temp_C' column (converting Temp_F to Celsius)
  df$Temp_C <- (df$Temp_F - 32) * 5 / 9
  
  # Add site, data type, serial number, and download_date to the data frame
  df$site <- site
  df$data_type <- data_type
  df$serial_number <- serial_number
  df$download_date <- date  # Renaming 'date' to 'download_date'
  
  # Convert 'PCT' column to DateTime type
  df$PCT <- as.POSIXct(df$PCT, format = "%m/%d/%y %I:%M:%S %p", tz = "UTC")
  
  # Store the cleaned data frame back in the list with the same key
  data_frames_wl[[key]] <- df
  print(paste("Successfully cleaned", key))
}

# Check if any data frames were created and cleaned
if (length(data_frames_wl) == 0) {
  print("No data frames were created. Please check the file contents and format.")
} else {
  # Combine all cleaned data frames into a single data frame
  all_data_wl_combined <- do.call(rbind, data_frames_wl)
  
  # Ensure the 'PCT' column is correctly parsed as DateTime
  all_data_wl_combined$PCT <- as.POSIXct(all_data_wl_combined$PCT, format = "%m/%d/%y %I:%M:%S %p", tz = "UTC")
}

# Plotting Absolute Pressure (Abs Pressure) from all sites over time
ggplot(all_data_wl_combined, aes(x = PCT, y = Abs_Pressure_psi, color = site)) +
  geom_line() +
  labs(
    x = "DateTime",
    y = "Abs Pressure (psi)",
    title = "Absolute Pressure from All Sites Over Time",
    color = "Site"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "top") +
  theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_color_discrete(name = "Site") +
  theme(panel.grid.major = element_line(color = "gray", size = 0.5), panel.grid.minor = element_blank())



#### STEP 2 ####
# Read in the manual flagging CSV for pressure data
manual_flag <- read.csv("WL/Sensor cleaning and filtering WY23-24 - pressure.csv")

# Ensure the date columns are in Date format
manual_flag$Visual_flagdate <- as.Date(manual_flag$Visual_flagdate, format = "%Y-%m-%d")
all_data_wl_combined$PCT <- as.Date(all_data_wl_combined$PCT)

# Strip suffix from manual_flag$Site (everything after the underscore)
manual_flag$Site_base <- sub("_.*", "", manual_flag$site)  # Removes everything after the first underscore

# Iterate through each site change (Code == "SM") for pressure data
for (i in 1:nrow(manual_flag)) {
  # Only handle rows where Code is "SM"
  if (manual_flag$Code[i] == "SM") {
    # Extract relevant values
    site_to_change <- manual_flag$Site_base[i]  # Base site without suffix
    site_name_change <- manual_flag$New_site[i]  # New site
    flag_date <- manual_flag$Visual_flagdate[i]  # Visual flag date
    
    # Debugging: Print the values we're working with
    print(paste("Processing site:", site_to_change, "->", site_name_change))
    print(paste("Flag date:", flag_date))
    
    # Match rows in all_data_wl_combined that contain the base site name
    matching_rows <- grepl(site_to_change, all_data_wl_combined$site)  # Match base site name
    
    # Update the site name based on date and match condition
    all_data_wl_combined$site[matching_rows & all_data_wl_combined$PCT < flag_date] <- paste0(site_to_change, "_1")  # Before flag date
    all_data_wl_combined$site[matching_rows & all_data_wl_combined$PCT >= flag_date] <- paste0(site_name_change, "_2")  # After flag date
  }
}

# Adjust site names
# Create a new column to rename sites to be more intuitive
# First, create a vector for site renaming
site_to_position <- c(
  "bedrock" = "bedrock", 
  "bridge" = "bridge", 
  "minimeso" = "S1", 
  "miniSS1" = "S4", 
  "miniSS2" = "S5", 
  "miniSS3" = "S6", 
  "SS2" = "SS7", 
  "miniSS4" = "S8", 
  "miniSS5" = "S9", 
  "miniSS6" = "S10", 
  "SS3" = "SS12", 
  "miniSS8" = "S13", 
  "miniSS9" = "S14", 
  "SS4" = "SS15"
)

# Now, mutate to create the site_position column
all_data_wl_combined <- all_data_wl_combined %>%
  mutate(site_position = site_to_position[site])  # Match the site names with the mapping

# Check the updated data
print(head(all_data_wl_combined))

# Generate a color scale with a sufficient number of colors
num_sites <- length(unique(all_data_wl_combined$site_position))
color_values <- viridis(num_sites)  # Use the viridis color palette

# Plot the data with the updated site names (Pressure over time)
ggplot(all_data_wl_combined, aes(x = PCT, y = Abs_Pressure_psi, color = site_position)) +
  geom_line() +
  labs(
    x = "DateTime", 
    y = "Pressure (psi)", 
    title = "Pressure Over Time with Updated Site Names"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = color_values)

# Plot Temperature Data with the updated site names
ggplot(all_data_wl_combined, aes(x = PCT, y = Temp_F, color = site_position)) +
  geom_line() +
  labs(
    x = "DateTime", 
    y = "Temperature (°F)", 
    title = "Temperature Over Time with Updated Site Names"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = color_values)



#### STEP 3. ####
sensor_metadata <- read.csv("Lux/Sensor downloads, stream geom + sample metadata WS1 23-24 - Sensor download dates.csv")

# Step 2: Filter for Lux sensors deployed in water
lux_sensors <- sensor_metadata %>%
  filter(Air.Water == "Water" & grepl("Lux", Sensor))  # Filter Lux sensors deployed in water

# Display the Lux sensors to confirm
print(lux_sensors)

# Step 3: Define the Lux sensors deployed in water for filtering
# You can manually input the serial numbers or filter from the dataset
lux_sensors_serials <- lux_sensors$SN

# Step 4: Assuming `all_data_lux_combined` is your light data frame
# Filter the light data based on SN from the metadata
all_data_lux_combined_filtered <- all_data_lux_combined %>%
  filter(serial_number %in% lux_sensors_serials)

# Step 5: Now that we have filtered data, we can calculate daily temperature variations and flag outliers
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

# Step 6: Check the filtered and flagged data
print(head(all_data_lux_combined_filtered))




# Calculate Daily Temperature Variation 

# Convert 'PCT' to Date format (if not already)
all_data_wl_combined$Date <- as.Date(all_data_wl_combined$PCT)

# Calculate daily temperature variation for each site
daily_temp_variation <- all_data_wl_combined %>%
  group_by(site, Date) %>%
  summarise(daily_temp_variation = max(Temp_F, na.rm = TRUE) - min(Temp_F, na.rm = TRUE)) %>%
  ungroup()

# Flag days with a temperature variation greater than 10°F (Flag 1)
all_data_wl_combined <- left_join(all_data_wl_combined, daily_temp_variation, by = c("site", "Date")) %>%
  mutate(flag1 = ifelse(daily_temp_variation > 10, "Temp Variation > 10°C", NA))


# Flag 2 Outliers Based on IQR for Pressure 

# Calculate the IQR for the 'Abs_Pressure_psi' column
Q1_pressure <- quantile(all_data_wl_combined$Abs_Pressure_psi, 0.25, na.rm = TRUE)
Q3_pressure <- quantile(all_data_wl_combined$Abs_Pressure_psi, 0.75, na.rm = TRUE)
IQR_pressure <- Q3_pressure - Q1_pressure

# Calculate lower and upper bounds for outliers in pressure data
lower_bound_pressure <- Q1_pressure - 1.5 * IQR_pressure
upper_bound_pressure <- Q3_pressure + 1.5 * IQR_pressure

# Flag outliers for 'Abs_Pressure_psi' (Flag 2)
all_data_wl_combined <- all_data_wl_combined %>%
  mutate(flag2 = ifelse(Abs_Pressure_psi < lower_bound_pressure | Abs_Pressure_psi > upper_bound_pressure, "Outlier", NA))

# Check the updated data
print(head(all_data_wl_combined))


# Visualize Flagged Data Points 

# Highlight the points with flag1 and flag2
ggplot(all_data_wl_combined, aes(x = PCT, y = Abs_Pressure_psi)) +
  geom_line(aes(color = site), size = 1) +  # Plot all data as lines
  geom_point(data = all_data_wl_combined %>% filter(!is.na(flag1)), 
             aes(x = PCT, y = Abs_Pressure_psi), 
             color = 'red', 
             shape = 16, 
             size = 2, 
             inherit.aes = FALSE, 
             label = 'flag1') +  # Highlight flag1 data points in red
  geom_point(data = all_data_wl_combined %>% filter(flag2 == "Outlier"), 
             aes(x = PCT, y = Abs_Pressure_psi), 
             color = 'blue', 
             shape = 17, 
             size = 2, 
             inherit.aes = FALSE, 
             label = 'flag2') +  # Highlight flag2 outliers in blue
  labs(x = "DateTime", y = "Pressure (psi)", 
       title = "Pressure Over Time with Flagged Data Points") +
  theme_minimal() +
  theme(legend.position = "bottom")


#### Step 4: Filter Flagged Outliers (Flag 2) ####

# Replace 'Abs_Pressure_psi' values with NA for outliers (flag2)
all_data_wl_combined <- all_data_wl_combined %>%
  mutate(Abs_Pressure_psi_filtered = case_when(
    flag2 == "Outlier" ~ NA_real_,  # Replace Abs_Pressure_psi with NA for outliers
    TRUE ~ Abs_Pressure_psi  # Keep the original Abs_Pressure_psi if not an outlier
  ))

# Check the updated data
print(head(all_data_wl_combined))












# First, let's add a column to check the max daily variation in pressure and temp
# Convert 'PCT' to Date format (extract the date without time)
# Convert 'PCT' to Date format (extract the date without time)
all_data_wl_combined$Date <- as.Date(all_data_wl_combined$PCT)

# Calculate the daily variation for pressure (Abs_Pressure_psi)
daily_variation_pressure <- all_data_wl_combined %>%
  group_by(site, Date) %>%
  summarise(daily_variation_pressure = max(Abs_Pressure_psi, na.rm = TRUE) - min(Abs_Pressure_psi, na.rm = TRUE)) %>%
  ungroup()

# Merge the daily pressure variation back into the original data frame based on site and date
all_data_wl_combined <- left_join(all_data_wl_combined, daily_variation_pressure, by = c("site", "Date"))

# Calculate the daily variation for temperature (Temp_F)
daily_variation_temp <- all_data_wl_combined %>%
  group_by(site, Date) %>%
  summarise(daily_variation_temp = max(Temp_F, na.rm = TRUE) - min(Temp_F, na.rm = TRUE)) %>%
  ungroup()

# Merge the daily temperature variation back into the original data frame based on site and date
all_data_wl_combined <- left_join(all_data_wl_combined, daily_variation_temp, by = c("site", "Date"))

# Check the result
print(head(all_data_wl_combined))







