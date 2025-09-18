# Packages ----

# Set the packages to read in
packages <- c("tidyverse", "tidycensus", "ggmap", "sf", "openxlsx", "arcgisbinding", "conflicted", "rmapshaper")

# Function to check and install missing packages
install_if_missing <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
}

# Apply the function to each package
invisible(sapply(packages, install_if_missing))

# Load the packages
library(tidyverse)
library(tidycensus)
library(ggmap)
library(sf)
library(openxlsx)
library(arcgisbinding)
library(conflicted)

# Prefer certain packages for certain functions
conflicts_prefer(dplyr::filter, dplyr::lag, lubridate::year, base::`||`, base::is.character, base::`&&`, stats::cor, base::as.numeric)

rm(install_if_missing, packages)

# Setting file paths / environment variables ----

state_or_metro <- 'cbsa' # Define the geography for the ACS data download. Other options include 'state', 'cbsa' (for metro), 'county', 'tract', 'block group', etc.
                        # See https://walker-data.com/tidycensus/articles/basic-usage.html#geography-in-tidycensus for a comprehensive list of geography options.

census_api_key <- 'f8d6fbb724ef6f8e8004220898ac5ed24324b814' # Provide the Census API Key, if others are running this you will need to get a Census API key here: https://api.census.gov/data/key_signup.html

acs_year <- 2024
acs_data_type <- 'acs1' # Define the survey to pull data from, 'acs5' for 5-year estimates, 'acs1' for 1 year estimates
geo_level_for_data_pull <- state_or_metro 
read_in_geometry <- FALSE # Change this to TRUE to pull in spatial data along with the data download 
                          # Geometry will take A LOT longer to read in.The more granular the geography, the longer the read-in time if TRUE.
show_api_call = TRUE # Show the call made to the Census API in the console, this will help if an error is thrown

state_shapefile_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2024/States/cb_2024_us_state_20m.shp" # Input the file path for the shape file that you would like to read in. 
metro_shapefile_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2024/CBSAs/cb_2024_us_cbsa_500k.shp" # Input the file path for the shape file that you would like to read in. 

output_filepath_for_cleaned_data <- paste0("household-counts/outputs/household_counts_by_", state_or_metro, "_2024.xlsx")
output_filepath_for_wide_shapefile <- paste0("household-counts/outputs/household_counts_by_", state_or_metro, ".shp")
output_filepath_for_long_shapefile <- paste0("household-counts/outputs/household_counts_by_", state_or_metro, "_long.shp")

# Create a variable list to read in ----

# Load the variables for the year / dataset selected above
acs_variables <- load_variables(year = acs_year, dataset = acs_data_type)

## Optional code to output the ACS variables. There is already a version of the 2023 variables available here: R:/ADHOC-JBREC/Ian-K/API Template Scripts/ACS/Summary Tables/acs_variables_2023_acs1.xlsx
# write.xlsx(acs_variables, paste0("R:/ADHOC-JBREC/Ian-K/API Template Scripts/ACS/Summary Tables/acs_variables_", acs_year, "_", acs_data_type, ".xlsx"))

# Read in the preferred variable spreadsheet (create your own within this file: R:/ADHOC-JBREC/Ian-K/API Template Scripts/ACS/Summary Tables/acs_variables_2023_acs1.xlsx)
variables <- read.xlsx("acs-variables/acs_variables_2024_acs1.xlsx", 
                       sheet = 'Household Counts')

# Select 'name' and 'amended_label' (and rename 'name' to code')
variables <- variables %>%
  select(name, amended_label) %>%
  rename(code = name)

# Create Codes, containing all of the preferred variable codes
variable_codes <- variables$code
# Create Labels, containing all of the amended labels
variable_labels <- variables$amended_label

# Read in the ACS data ----

data <- get_acs(
          geography = geo_level_for_data_pull,
          variables = variable_codes,
          year = acs_year,
          geometry = read_in_geometry,
          key = census_api_key,
          survey = acs_data_type,
          show_call = show_api_call
          )

data <- data %>%
  # Rename 'variable' to 'Code'
  rename(code = variable) %>%
  # Join the variable spreadsheet to the ACS data by 'Code'
  left_join(variables, by = 'code') %>%
  # Rename the listed 'Variable' with the 'AmendedLabel' from the variable spreadsheet
  rename(variable = amended_label) %>%
  # Drop the 'Code' column
  select(-code)

# Pivot the ACS data to a wide format, with columns named by variable. Each geography unit will have one row with one column per variable.
data <- data %>%
  pivot_wider(names_from = 'variable', values_from = 'estimate', id_cols = c('GEOID', 'NAME'))

# Your code to clean/analyze ACS data ----

if(state_or_metro == 'state') {
  
  data_summarized <- data %>%
    filter(!is.na(units_total)) %>%
    # Group by CBSA (NAME, GEOID in this case as they are both unique identifiers)
    group_by(NAME, GEOID) %>%
    # To find the total number of housing units, SF-detached units, and SF-attached units
    summarize(across(pop:renter_units_boat_van_rv, ~sum(., na.rm = T))) %>%
    # Ungroup!
    ungroup()
  
} else if(state_or_metro == 'cbsa'){
  
  data_summarized <- data %>%
    filter(!is.na(units_total) & !str_detect(NAME, pattern = "PR Metro Area")) %>%
    mutate(NAME = str_remove(NAME, " Metro Area"),
           NAME = str_remove(NAME, " Micro Area")) %>%
    # Group by CBSA (NAME, GEOID in this case as they are both unique identifiers)
    group_by(NAME, GEOID) %>%
    # To find the total number of housing units, SF-detached units, and SF-attached units
    summarize(across(pop:renter_units_boat_van_rv, ~sum(., na.rm = T))) %>%
    # Ungroup!
    ungroup()
  
} else{
  
  print("Check state_or_metro value!")
  
}

# Output tabular data ----

write.xlsx(data_summarized, output_filepath_for_cleaned_data)

# Read in spatial files ----

# Note, these files will contain geographies from US Territories (i.e. Puerto Rico, Guam, etc.). Remove them if need be!

state_shapefile <- st_read(state_shapefile_file_path)
metro_shapefile <- st_read(metro_shapefile_file_path)

state_shapefile <- state_shapefile %>%
  select(GEOID, geometry)

metro_shapefile <- metro_shapefile %>%
  select(GEOID, geometry)

# Create a spatial file and plot it! ----

if(state_or_metro == 'state') {
  
  # Join the shapefile geometry to the summarized data by GEOID:
  spatial_data <- data_summarized %>%
    left_join(state_shapefile, by = 'GEOID') %>%
    st_as_sf()
  
} else if(state_or_metro == 'cbsa'){
  
  # Join the shapefile geometry to the summarized data by GEOID:
  spatial_data <- data_summarized %>%
    left_join(metro_shapefile, by = 'GEOID') %>%
    st_as_sf()
  
} else{
  
  print("Check state_or_metro value!")
  
}

# Plot the data:
spatial_data %>%
  filter(!NAME %in% c('Alaska', 'Hawaii')) %>%
  filter(
    !str_detect(NAME, pattern = ', AK') & !str_detect(NAME, pattern = ', HI')
    ) %>%
  ggplot(aes(fill = owner_units_2 / owner_units_total)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(option = 'D') +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# Output spatial data ----

spatial_data_long <- spatial_data %>%
  select(-pop) %>%
  pivot_longer(names_to = 'category', values_to = 'hh_count', cols = units_total:renter_units_boat_van_rv) %>%
  st_simplify(dTolerance = 50, preserveTopology = T)


spatial_data_long <- spatial_data_long %>%
  mutate(category = case_when(
    category == 'units_total' ~ 'Total Housing Units',
    category == 'owner_units_total' ~ 'Total Owner-Occupied Housing Units',
    category == 'owner_units_sf_det' ~ 'Owner-Occupied Housing Units: Single-Family Detached',
    category == 'owner_units_sf_att' ~ 'Owner-Occupied Housing Units: Single-Family Attached',
    category == 'owner_units_2' ~ 'Owner-Occupied Housing Units in 2 Unit Properties',
    category == 'owner_units_3_4' ~ 'Owner-Occupied Housing Units in 3-to-4 Unit Properties',
    category == 'owner_units_5_9' ~ 'Owner-Occupied Housing Units in 5-to-9 Unit Properties',
    category == 'owner_units_10_19' ~ 'Owner-Occupied Housing Units in 10-to-19 Unit Properties',
    category == 'owner_units_20_49' ~ 'Owner-Occupied Housing Units in 20-to-49 Unit Properties',
    category == 'owner_units_50_plus' ~ 'Owner-Occupied Housing Units in 50+ Unit Properties',
    category == 'owner_units_mobile' ~ 'Owner-Occupied Housing Units: Mobile Homes',
    category == 'owner_units_boat_van_rv' ~ 'Owner-Occupied Housing Units: Boats, RVS, Vans, etc.',
    category == 'renter_units' ~ 'Total Renter-Occupied Housing Units',
    category == 'renter_units_sf_det' ~ 'Renter-Occupied Housing Units: Single-Family Detached',
    category == 'renter_units_sf_att' ~ 'Renter-Occupied Housing Units: Single-Family Attached',
    category == 'renter_units_2' ~ 'Renter-Occupied Housing Units in 2 Unit Properties',
    category == 'renter_units_3_4' ~ 'Renter-Occupied Housing Units in 3-to-4 Unit Properties',
    category == 'renter_units_5_9' ~ 'Renter-Occupied Housing Units in 5-to-9 Unit Properties',
    category == 'renter_units_10_19' ~ 'Renter-Occupied Housing Units in 10-to-19 Unit Properties',
    category == 'renter_units_20_49' ~ 'Renter-Occupied Housing Units in 20-to-49 Unit Properties',
    category == 'renter_units_50_plus' ~ 'Renter-Occupied Housing Units in 50+ Unit Properties',
    category == 'renter_units_mobile' ~ 'Renter-Occupied Housing Units: Mobile Homes',
    category == 'renter_units_boat_van_rv' ~ 'Renter-Occupied Housing Units: Boats, RVS, Vans, etc.',
    T ~ category
  ))

spatial_data_long <- spatial_data_long %>%
  group_by(NAME, GEOID) %>%
  mutate(shr_tot = (hh_count / hh_count[category == 'Total Housing Units']) * 100,
         shr_own = (hh_count / hh_count[category == 'Total Owner-Occupied Housing Units']) * 100,
         shr_rnt = (hh_count / hh_count[category == 'Total Renter-Occupied Housing Units']) * 100) %>%
  ungroup()

names(spatial_data) <- str_replace(names(spatial_data), pattern = "owner_units", replacement = 'o')
names(spatial_data) <- str_replace(names(spatial_data), pattern = "renter_units", replacement = 'r')


# Check to make sure there is an Active ArcGIS Installation
arc.check_product()

# Output the ACS zip code data to the path specified
arc.write(path = output_filepath_for_wide_shapefile, data = spatial_data, overwrite = TRUE, validate = TRUE)
arc.write(path = output_filepath_for_long_shapefile, data = spatial_data_long, overwrite = TRUE, validate = TRUE)
