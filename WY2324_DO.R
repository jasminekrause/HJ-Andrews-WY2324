## Script to process dissolved oxygen data
# Step 1. Load data
# Step 2. Adjust site names accordingly
# Step 3. Filter out flagged data from sensor downloads and create additional flagging for outliers
# Step 3. Apply sensor calibration to adjust for sensor drift and evaluate
# Step 4. 

library(ggplot2)
library(tidyverse)
library(viridis)
library(dplyr)


#### Step 1. ####
setwd("/Users/jasminekrause/Documents/GitHub/HJ-Andrews-WY2324/Sensor_downloads")

# Define the function to read and preprocess the data
read_and_preprocess <- function(filepath, site_name) {
  # Read in the data, skipping the first 9 rows and selecting the relevant columns
  df <- read.csv(filepath, skip = 8)[, 3:7]
  # Rename columns
  colnames(df) <- c("PCT", "Q", "Temp", "DO", "DOT_saturation")
  # Add site column
  df$site <- site_name
  # Parse the PCT column to datetime
  df$PCT <- as.POSIXct(df$PCT, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  return(df)
}

# UPDATE Download date here
base_dir_PMEDO <- 'DO/20241110'

# Define the pattern to match files (assuming all files are in the base directory)
pattern <- paste0(base_dir_PMEDO, "/*.TXT")

# Use list.files to find all files that match the pattern
filepaths <- list.files(path = base_dir_PMEDO, pattern = "*.TXT", full.names = TRUE)

# Dictionary to store DataFrames
data_frames_PMEDO <- list()

# Loop through the file paths, read data, and store in the dictionary
for (filepath in filepaths) {
  tryCatch({
    # Extract site and data type from the file path
    parts <- strsplit(basename(filepath), "_")[[1]]
    site <- parts[1]  # Assumes the first part is the site
    
    # Read and preprocess the data
    df <- read_and_preprocess(filepath, site)
    
    # Filter out rows with NA in PCT or DO
    df <- df %>% drop_na(PCT, DO)
    
    # Store the DataFrame in the list
    data_frames_PMEDO[[site]] <- df
    print(paste("Successfully read", filepath))
  }, error = function(e) {
    print(paste("Error reading", filepath, ":", e$message))
  })
}

# Concatenate all DataFrames into one for plotting
all_data_PMEDO <- bind_rows(data_frames_PMEDO)
print(all_data_PMEDO)

# Plotting
ggplot(all_data_PMEDO, aes(x = PCT, y = DO, color = site)) +
  geom_line() +
  labs(x = "DateTime", y = "DO, mg/L") +
  theme_minimal() +
  theme(legend.position = "bottom")


#### Step 2. ####
# Read in manual flagging .csv
manual_flag <- read.csv("Sensor cleaning and filtering WY23-24 - DO.csv")

# Ensure the date columns are in Date format
manual_flag$Visual_flagdate <- as.Date(manual_flag$Visual_flagdate, format = "%Y-%m-%d")
all_data_PMEDO$PCT <- as.Date(all_data_PMEDO$PCT)

# Strip suffix from manual_flag$Site (everything after the underscore)
manual_flag$Site_base <- sub("_.*", "", manual_flag$site)  # Removes everything after the first underscore

# Iterate through each site change (Code == "SM")
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
    
    # Match rows in all_data_PMEDO that contain the base site name
    matching_rows <- grepl(site_to_change, all_data_PMEDO$site)  # Match base site name
    
    # Update the site name based on date and match condition
    all_data_PMEDO$site[matching_rows & all_data_PMEDO$PCT < flag_date] <- paste0(site_to_change, "_1")  # Before flag date
    all_data_PMEDO$site[matching_rows & all_data_PMEDO$PCT >= flag_date] <- paste0(site_name_change, "_2")  # After flag date
  }
}

# Check the updated data
print(head(all_data_PMEDO))


#### Step 3. ####
# Flag 1 will be the manual flag from sensor downloads
# Ensure the date columns are in Date format
manual_flag$Visual_flagdate <- as.Date(manual_flag$Visual_flagdate, format = "%Y-%m-%d")
all_data_PMEDO$PCT <- as.Date(all_data_PMEDO$PCT)

# Create a new column 'flag1' in all_data_PMEDO and flag questionable data from manual_flag
all_data_PMEDO <- all_data_PMEDO %>%
  mutate(flag1 = ifelse(PCT %in% manual_flag$Visual_flagdate, 
                        manual_flag$Code[match(PCT, manual_flag$Visual_flagdate)], 
                        NA))

# Check the updated data
print(head(all_data_PMEDO))

# Flag 2 are outliers 
# Calculate the IQR for the 'DO' column
Q1 <- quantile(all_data_PMEDO$DO, 0.25)
Q3 <- quantile(all_data_PMEDO$DO, 0.75)
IQR <- Q3 - Q1

# Calculate lower and upper bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Flag outliers
all_data_PMEDO <- all_data_PMEDO %>%
  mutate(flag2 = ifelse(DO < lower_bound | DO > upper_bound, "Outlier", NA))

# Check the updated data
print(head(all_data_PMEDO))


# Highlight the points with flag1 and flag2
ggplot(all_data_PMEDO, aes(x = PCT, y = DO)) +
  geom_line(aes(color = site), size = 1) +  # Plot all data as lines
  geom_point(data = all_data_PMEDO %>% filter(!is.na(flag1)), 
             aes(x = PCT, y = DO), 
             color = 'red', 
             shape = 16, 
             size = 2, 
             inherit.aes = FALSE, 
             label = 'flag1') +  # Highlight flag1 data points in red
  geom_point(data = all_data_PMEDO %>% filter(flag2 == "Outlier"), 
             aes(x = PCT, y = DO), 
             color = 'blue', 
             shape = 17, 
             size = 2, 
             inherit.aes = FALSE, 
             label = 'flag2') +  # Highlight flag2 outliers in blue
  labs(x = "DateTime", y = "DO, mg/L", 
       title = "Dissolved Oxygen (DO) Over Time with Flagged Data Points") +
  theme_minimal() +
  theme(legend.position = "bottom")


# We are just going to filter flag2 for now.
all_data_PMEDO <- all_data_PMEDO %>%
  mutate(DO_filtered = case_when(
    flag2 == "Outlier" ~ NA_real_,  # Replace DO with NA for Outliers
    TRUE ~ DO  # Keep the original DO if not an Outlier
  ))


# Create a new column to rename sites to be more intutitive
# First, create a vector
site_to_position <- c(
  "minimeso" = "S1", 
  "miniSS0" = "S2", 
  "SS1" = "SS3", 
  "miniSS1" = "S4", 
  "miniSS2" = "S5", 
  "miniSS3" = "S6", 
  "SS2" = "SS7", 
  "miniSS4" = "S8", 
  "miniSS5" = "S9", 
  "miniSS6" = "S10", 
  "miniSS7_1" = "S11.a", 
  "miniSS7_2_2" = "S11.b", 
  "SS3_1" = "SS12.a", 
  "SS3_2" = "SS12.b", 
  "miniSS8" = "S13", 
  "miniSS9" = "S14", 
  "SS4_1" = "SS15.a", 
  "SS4_2_2" = "SS15.b"
)

# Now, mutate to create the site_position column
all_data_PMEDO <- all_data_PMEDO %>%
  mutate(site_position = site_to_position[site])  # Match the Site_base with the mapping

# Check the updated data
print(head(all_data_PMEDO))


# Generate a color scale with a sufficient number of colors
num_sites <- length(unique(all_data_PMEDO$site_position))
color_values <- viridis(num_sites)  # Use the viridis color palette

# Plot the data with the updated site names
ggplot(all_data_PMEDO, aes(x = PCT, y = DO_filtered, color = site_position)) +
  geom_line() +
  labs(x = "DateTime", y = "DO, mg/L", title = "Dissolved Oxygen (DO) Over Time with Updated Site Names") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = color_values)







