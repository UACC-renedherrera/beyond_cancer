# GIS stuff for beyond cancer 
# René Dario Herrera 
# University of Arizona Cancer Center 
# 12 Oct 2021 

# which cities did people register from?

# setup 
# load packages 
library(here)
library(tidyverse)
library(zipcodeR)
library(tigris)

# read data 
bcs_3_zip_codes <- read_rds("data/tidy/bcs_3_registration_zip_codes.rds")

# az zip codes 
az_zip_codes <- search_state("AZ")

# inspect
class(az_zip_codes)
glimpse(az_zip_codes)

# join BCS3 zip codes to my az zip code list 
bcs_3_zip_code_cities <- inner_join(x = bcs_3_zip_codes,
                                   y = az_zip_codes)

# produce a table
bcs_3_zip_code_cities %>%
  select(in_person, zipcode, major_city, county, state) %>%
  knitr::kable()
