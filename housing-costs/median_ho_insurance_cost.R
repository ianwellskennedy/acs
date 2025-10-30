# Packages ----

# Set the packages to read in
packages <- c("tidyverse", "tidycensus", "ggmap", "sf", "openxlsx", "arcgisbinding", "conflicted", "rmapshaper", "spatstat")

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

geo_level <- 'cbsa' # Define the geography for the ACS data download. Other options include 'state', 'cbsa' (for metro), 'county', 'tract', 'block group', etc.
# See https://walker-data.com/tidycensus/articles/basic-usage.html#geography-in-tidycensus for a comprehensive list of geography options.

census_api_key <- 'f8d6fbb724ef6f8e8004220898ac5ed24324b814' # Provide the Census API Key, if others are running this you will need to get a Census API key here: https://api.census.gov/data/key_signup.html

acs_year <- 2024
acs_data_type <- 'acs1' # Define the survey to pull data from, 'acs5' for 5-year estimates, 'acs1' for 1 year estimates
geo_level_for_data_pull <- geo_level 
read_in_geometry <- FALSE # Change this to TRUE to pull in spatial data along with the data download 
# Geometry will take A LOT longer to read in.The more granular the geography, the longer the read-in time if TRUE.
show_api_call = TRUE # Show the call made to the Census API in the console, this will help if an error is thrown

state_shapefile_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2024/States/cb_2024_us_state_20m.shp" # Input the file path for the shape file that you would like to read in. 
metro_shapefile_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2024/CBSAs/cb_2024_us_cbsa_500k.shp" # Input the file path for the shape file that you would like to read in. 

output_filepath_for_cleaned_data <- paste0("housing-costs/outputs/median_homeowner_ins_cost_by_", geo_level, "_", acs_year,".xlsx")
output_filepath_for_shapefile <- paste0("housing-costs/outputs/median_homeowner_ins_cost_by_", geo_level, "_", acs_year, ".shp")

# Create a variable list to read in ----

# Load the variables for the year / dataset selected above
#acs_variables <- load_variables(year = acs_year, dataset = acs_data_type)

# Read in the preferred variable spreadsheet (create your own within this file: R:/ADHOC-JBREC/Ian-K/API Template Scripts/ACS/Summary Tables/acs_variables_2023_acs1.xlsx)
variables <- read.xlsx("acs-variables/acs_variables_2024_acs1.xlsx", 
                       sheet = 'Insurance')

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
  geography = geo_level,
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
  select(-c(code, moe))

med_value <- data %>%
  filter(variable == 'med_home_val') %>%
  select(-c(NAME, variable)) %>%
  rename(med_home_val = estimate)

# Your code to clean/analyze ACS data ----

data_summarized <- data %>%
  mutate(ins_midpoint = case_when(
    str_detect(variable, "under_100") ~ 50,
    str_detect(variable, "100_to_299") ~ 200,
    str_detect(variable, "300_to_499") ~ 400,
    str_detect(variable, "500_to_799") ~ 650,
    str_detect(variable, "800_to_999") ~ 900,
    str_detect(variable, "1000_to_1499") ~ 1250,
    str_detect(variable, "1500_to_1999") ~ 1750,
    str_detect(variable, "2000_to_2499") ~ 2250,
    str_detect(variable, "2500_to_2999") ~ 2750,
    str_detect(variable, "3000_to_3499") ~ 3250,
    str_detect(variable, "3500_to_3999") ~ 3750,
    str_detect(variable, "over_4000") ~ 4500,
    T ~ NA
  )) %>%
  rename(metro_name = NAME)

data_summarized <- data_summarized %>% 
  filter(!is.na(ins_midpoint)) %>%
  group_by(metro_name, GEOID) %>%
  summarize(med_ins = weighted.median(ins_midpoint, w = estimate),
            avg_ins = weighted.mean(ins_midpoint, w = estimate)) %>%
  ungroup() 

if(geo_level == 'state') {
  
  data_summarized <- data_summarized %>%
    filter(!is.na(avg_ins)) 
  
} else if(geo_level == 'cbsa'){
  
  data_summarized <- data_summarized %>%
    filter(!is.na(avg_ins) & !str_detect(metro_name, pattern = "PR Metro Area")) %>%
    mutate(metro_name = str_remove(metro_name, " Metro Area"),
           metro_name = str_remove(metro_name, " Micro Area")) 
  
} else if(geo_level == 'county'){
  
  data_summarized <- data_summarized %>%
    filter(!is.na(avg_ins) & !str_detect(NAME, pattern = "Puerto Rico")) %>%
    mutate(NAME = str_remove(NAME, " County.*")) 
} else{
  
  print("Check geo_level value!")
  
}


# Join home value data ----

data_summarized <- data_summarized %>%
  left_join(med_value, by = 'GEOID') 

data_summarized <- data_summarized %>%
  mutate(shr_of_val = (med_ins/med_home_val)*100) 


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

if(geo_level == 'state') {
  
  # Join the shapefile geometry to the summarized data by GEOID:
  spatial_data <- data_summarized %>%
    left_join(state_shapefile, by = 'GEOID') %>%
    st_as_sf()
  
} else if(geo_level == 'cbsa'){
  
  # Join the shapefile geometry to the summarized data by GEOID:
  spatial_data <- data_summarized %>%
    left_join(metro_shapefile, by = c('GEOID')) %>%
    st_as_sf()
  
} else{
  
  print("Check geo_level value!")
  
}

# Plot the data:
spatial_data %>%
  filter(!metro_name %in% c('Alaska', 'Hawaii')) %>%
  filter(
    !str_detect(metro_name, pattern = ', AK') & !str_detect(metro_name, pattern = ', HI')
  ) %>%
  ggplot(aes(fill = shr_of_val)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(option = 'D') +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# Output spatial data ----

# Check to make sure there is an Active ArcGIS Installation
arc.check_product()

# Output the ACS zip code data to the path specified
arc.write(path = output_filepath_for_shapefile, data = spatial_data, overwrite = TRUE, validate = TRUE)
