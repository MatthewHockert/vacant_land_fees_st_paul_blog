
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(beepr)
# Define the range of years
years <- 2005:2022

# Define the base file path and layer name pattern
base_file_path <- "/Users/matthewhockert/Desktop/HCRA/shp_plan_regonal_parcels_"
layer_name_pattern <- "Parcels%YEAR%Ramsey"

# Define column names to keep
column_names_to_keep <- c("COUNTY_ID", "PIN", "CITY","CTU_NAME", "ZIP", "USE1_DESC", "DWELL_TYPE", 
                          "ACRES_POLY", "NUM_UNITS", "EMV_LAND", "EMV_BLDG", "EMV_TOTAL", 
                          "TOTAL_TAX", "TAX_EXEMPT", "YEAR_BUILT", "OWNER_NAME", "HOMESTEAD", 
                          "geometry")

# Function to generate file path and layer name based on year
get_file_path <- function(year) {
  paste0(base_file_path, year)
}

get_layer_name <- function(year) {
  sub("%YEAR%", year, layer_name_pattern)
}

# Function to load and process each shapefile
process_shapefile <- function(file_path, layer, column_names,year) {
  df <- st_read(file_path, layer = layer)
  df$year <- year
  column_names <- c(column_names, "year")
  column_names <- column_names[column_names %in% names(df)]
  subset(df, select = column_names)
}

# Load, process, and combine all data frames
processed_data_frames <- lapply(years, function(year) {
  file_path <- get_file_path(year)
  layer_name <- get_layer_name(year)
  process_shapefile(file_path, layer_name, column_names_to_keep,year)
})

all_columns <- unique(unlist(lapply(processed_data_frames, names)))

# Function to ensure all data frames have the same columns
ensure_columns <- function(df, all_columns) {
  for (col in all_columns) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }
  df <- subset(df, select = all_columns)
  return(df)
}

# Process all data frames to ensure they have the same columns
processed_data_frames <- lapply(processed_data_frames, ensure_columns, all_columns = all_columns)

# Subset the processed data frames to keep only the desired columns
# subset_data_frames <- lapply(processed_data_frames, function(df) {
#   subset(df, select = column_names_to_keep)
# })

# Combine all processed data frames into one
ramsey_data <- do.call(rbind, processed_data_frames)
rm(processed_data_frames)
rm(subset_data_frames)
ramsey_data <- ramsey_data[grep("-\\d+$", ramsey_data$PIN),]
ramsey_data$PIN_2 <- sub("^[^-]*-", "", ramsey_data$PIN)