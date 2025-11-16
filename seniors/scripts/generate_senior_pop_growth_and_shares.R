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
invisible(sapply(packages, function(p) library(p, character.only = TRUE)))

# Prefer certain packages for certain functions
conflicts_prefer(dplyr::filter, dplyr::lag, lubridate::year, base::`||`, base::is.character, base::`&&`, stats::cor, base::as.numeric)

rm(install_if_missing, packages)

# Setting file paths / environment variables ----

# Read in a pre-packaged set of variables to analyze
acs_variables_file_path <- "C:/Users/ianwe/Downloads/github/acs/acs-variables/acs_variables_2024_acs1.xlsx"

county_shp_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2024/Counties/cb_2024_us_county_5m.shp" # Se  the file path to the county shp, this file can be downloaded here: https://www2.census.gov/geo/tiger/GENZ2024/shp/cb_2024_us_county_5m.zip
census_api_key <- '6dd2c4143fc5f308c1120021fb663c15409f3757' # Provide the Census API Key, if others are running this you will need to get a Census API key here: https://api.census.gov/data/key_signup.html

acs_years <- c(2023, 2024) # Specify the years to read in ACS data for
acs_data_type <- 'acs1' # Define the survey to pull data from, 'acs5' for 5-year estimates, 'acs1' for 1 year estimates
geo_level_for_data_pull <- "county" # Define the geography for the ACS data download. Other options include 'state', 'county', 'tract', 'block group', etc.
# See https://walker-data.com/tidycensus/articles/basic-usage.html#geography-in-tidycensus for a comprehensive list of geography options.
read_in_geometry <- FALSE # Change this to TRUE to pull in spatial data along with the data download 
show_api_call = TRUE # Show the call made to the Census API in the console, this will help if an error is thrown

# Specify output file paths for tabular and spatial data files
output_filepath_for_cleaned_data <- "seniors/outputs/senior_pop_growth_by_county.xlsx"
output_filepath_for_shp <- "seniors/outputs/senior_pop_growth_by_county.shp"

# Read in and clean .shp file ----

# Read in the county .shp file
county_shp <- st_read(county_shp_file_path)

# Select only the GEOID, and rename it to county_fips_code
county_shp <- county_shp %>%
  select(GEOID) %>%
  rename(county_fips_code = GEOID)

# Create a variable list to read in ----

# Load the variables for the year / dataset selected above
#acs_variables <- load_variables(year = 2024, dataset = acs_data_type)

# Read in the preferred variable spreadsheet (create your own within this file: R:/ADHOC-JBREC/Ian-K/API Template Scripts/ACS/Summary Tables/acs_variables_2023_acs1.xlsx)
variables <- read.xlsx(acs_variables_file_path, sheet = 'Seniors')

# Select 'name' and 'amended_label' (and rename 'name' to code')
variables <- variables %>%
  select(name, amended_label) %>%
  rename(code = name)

# Create Codes, containing all of the preferred variable codes
variable_codes <- variables$code
# Create Labels, containing all of the amended labels
variable_labels <- variables$amended_label

# Read in the ACS data ----

# For each year in acs_years, read in the data based on the parameters previously established
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
  
  # Name each output data frame "data_[year]"
  assign(paste0("data_", year), data)
}

# Analysis ----

# Create a custom function to clean the data / perform calculations
clean_data <- function(data){
  data <- data %>%
    rename(county_fips_code = GEOID, county_name = NAME) %>%
    mutate(pop_65_plus = rowSums(across(m_65_66:f_85_plus), na.rm = T),
           shr_pop_65_plus = pop_65_plus/pop) %>%
    select(county_name, county_fips_code, pop, pop_65_plus, shr_pop_65_plus, median_age) 
}

# Use the custom function on the 2023/2024 data frames, and rename relevant names to include "_[year]"
data_2024 <- clean_data(data_2024)
names(data_2024)[3:6] <- paste0(names(data_2024)[3:6], "_2024")

data_2023 <- clean_data(data_2023)
names(data_2023)[3:6] <- paste0(names(data_2023)[3:6], "_2023")

# Join the 2023/2024 data frames together
data_final <- data_2024 %>%
  left_join(data_2023, by = c('county_name', 'county_fips_code'))

# Calculate values in which 2023 and 2024 data is necessary
data_final <- data_final %>%
  mutate(
    snr_share_growth_23_to_24 = shr_pop_65_plus_2024 - shr_pop_65_plus_2023,
    snr_growth_23_to_24 = pop_65_plus_2024 - pop_65_plus_2023,
    snr_growth_rate_23_to_24 = (pop_65_plus_2024 - pop_65_plus_2023) / pop_65_plus_2023,
    state = str_extract(county_name, "(?<=, ).*"),
    county_name = str_remove(county_name, ", .*")
         )

# Output tabular data ----

write.xlsx(data_final, output_filepath_for_cleaned_data)

# Output spatial data ----

# Join the data to the county .shp file
data_final <- data_final %>%
  left_join(county_shp, by = 'county_fips_code') %>%
  st_as_sf()

# Check to make sure there is an active ArcGIS Pro installation
arc.check_product()

# Output the .shp file
arc.write(data_final, path = output_filepath_for_shp, overwrite = T, validate = T)
