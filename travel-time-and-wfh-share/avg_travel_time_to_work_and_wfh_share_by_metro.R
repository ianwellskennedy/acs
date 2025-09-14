# Packages ----

# Set the packages to read in
packages <- c("tidyverse", "tidycensus", "ggmap", "sf", "openxlsx", "arcgisbinding", "conflicted", "readxl")

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
library(readxl)

# Prefer certain packages for certain functions
conflicts_prefer(dplyr::filter, dplyr::lag, lubridate::year, base::`||`, base::is.character, base::`&&`, stats::cor, base::as.numeric)

rm(install_if_missing, packages)

# Setting file paths / environment variables ----

census_api_key <- "f8d6fbb724ef6f8e8004220898ac5ed24324b814" # Enter your Census API Key, obtain one here if need be: https://api.census.gov/data/key_signup.html

acs_years <- seq(2023, 2024, 1)
acs_data_type <- 'acs1' # Define the survey to pull data from, 'acs5' for 5-year estimates, 'acs1' for 1 year estimates
geo_level_for_data_pull <- "cbsa" # Define the geography for the ACS data download. Other options include 'state', 'county', 'tract', 'block group', etc.
# See https://walker-data.com/tidycensus/articles/basic-usage.html#geography-in-tidycensus for a comprehensive list of geography options.
read_in_geometry <- FALSE # Change this to TRUE to pull in spatial data along with the data download 
show_api_call = TRUE # Show the call made to the Census API in the console, this will help if an error is thrown

metro_shp_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2024/CBSAs/cb_2024_us_cbsa_5m.shp"
output_filepath_for_cleaned_data <- "travel-time-and-wfh-share/outputs/avg_travel_time_to_work_and_wfh_share_by_metro.xlsx"
output_filepath_for_shp <- "travel-time-and-wfh-share/outputs/avg_travel_time_to_work_and_wfh_share_by_metro.shp"

# Create a variable list to read in ----

# Load the variables for the year / dataset selected above
#acs_variables <- load_variables(year = acs_year, dataset = acs_data_type)

## Optional code to output the ACS variables. There is already a version of the 2023 variables available here: R:/ADHOC-JBREC/Ian-K/API Template Scripts/ACS/Summary Tables/acs_variables_2023_acs1.xlsx
#write.xlsx(acs_variables, paste0("acs-variables/acs_variables_", acs_year, "_", acs_data_type, ".xlsx"))

# Read in the preferred variable spreadsheet (create your own within this file: R:/ADHOC-JBREC/Ian-K/API Template Scripts/ACS/Summary Tables/acs_variables_2023_acs1.xlsx)
variables <- read.xlsx(paste0("acs-variables/acs_variables_", 2024, "_", acs_data_type, ".xlsx"), sheet = 'Travel Time')

# Select 'name' and 'amended_label' (and rename 'name' to code')
variables <- variables %>%
  select(name, amended_label) %>%
  rename(code = name)

# Create Codes, containing all of the preferred variable codes
variable_codes <- variables$code
# Create Labels, containing all of the amended labels
variable_labels <- variables$amended_label

# Read  in the metro shape file ----

metro_shp <- st_read(metro_shp_file_path)

metro_shp <- metro_shp %>%
  rename(metro_name = NAME, metro_code = GEOID) %>%
  select(metro_name, metro_code, geometry)

# Read in the ACS data ----

for (year in acs_years) {
  data <- get_acs(
    geography = geo_level_for_data_pull,
    variables = variable_codes,
    year = year,
    geometry = read_in_geometry,
    key = census_api_key,
    survey = acs_data_type,
    show_call = show_api_call
  )
  
  data <- data %>%
    rename(code = variable) %>%
    left_join(variables, by = 'code') %>%
    rename(variable = amended_label) %>%
    select(-code) %>%
    pivot_wider(names_from = 'variable', values_from = 'estimate', id_cols = c('GEOID', 'NAME'))
  
  assign(paste0("data_", year), data)
}

# Your code to clean/analyze ACS data ----

clean_travel_time_data <- function(data) {
  data <- data %>%
    mutate(
      avg_tt = (travel_time / labor_force),
      avg_tt_c = (travel_time_car / labor_force_car),
      avg_tt_p = (travel_time_public_transit / labor_force_public_transit),
      avg_tt_w = (travel_time_walked / labor_force_walked),
      wfh_sh = (labor_force_wfh / labor_force) * 100
    ) %>%
    filter(!str_detect(NAME, pattern = "PR Metro Area")) %>%
    select(GEOID, pop, wfh_sh,starts_with('avg_tt')) %>%
    rename(metro_code = GEOID)
}

data_2024 <- data_2024 %>%
  clean_travel_time_data()
names(data_2024)[-1] <- paste0(names(data_2024)[-1], "_24")

data_2023 <- data_2023 %>%
  clean_travel_time_data()
names(data_2023)[-1] <- paste0(names(data_2023)[-1], "_23")

data_final <- data_2024 %>%
  left_join(data_2023, by = 'metro_code') 

data_final <- data_final %>%
  mutate(tt_ch = avg_tt_24 - avg_tt_23,
         wfh_change = wfh_sh_24 - wfh_sh_23)

data_final <- data_final %>%
  left_join(metro_shp, by = 'metro_code') 

data_final <- data_final %>%
  select(metro_name, metro_code, starts_with('pop'), starts_with('wfh') , everything())

data_final_tabular <- data_final %>%
  select(-geometry)

# Output tabular data ----

write.xlsx(data_final_tabular, output_filepath_for_cleaned_data)

# Output spatial data ----

data_final <- data_final %>%
  st_as_sf()

arc.check_product()

arc.write(data_final, path = output_filepath_for_shp, overwrite = T, validate = T)
