#### Preamble ####
# Purpose: Download the Toronto Shelter System Flow data from Open Data Toronto.
# Author: Maria Mangru
# Date: 19th January 2024
# Contact: maria.mangru@mail.utoronto.ca
# License: none
# Pre-requisites: opendatatoronto package, dplyr package
# Note: code provided by Open Data Toronto with additional comments added by author


#### Workspace setup ####
install.packages("opendatatoronto")
install.packages("dplyr")
library(opendatatoronto)
library(dplyr)


#### Download Data ####

# Get package information 
package_id <- "ac77f532-f18b-427c-905c-4ae87ce69c93"  # unique identifier for the data set
package <- show_package(package_id)
print(package)

# Get package resources
resources <- list_package_resources(package_id)

# identify datastore resources; by default, Toronto Open Data sets datastore resource 
# format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# Load the first datastore resource as a sample
data <- filter(datastore_resources, row_number() == 1) %>% get_resource()

# View the first few rows of the data
print(head(data))
