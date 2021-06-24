# Data tidy for
# Beyond Cancer Event Registration
# and Survey 

# set up
# load package libraries 
library(here)
library(tidyverse)
library(ggthemes)
library(qualtRics)
library(zipcodeR)
library(sf)
library(tigris)
library(geojson)
library(knitr)
library(lubridate)
library(zipcodeR)

options(tigris_use_cache = TRUE)

# get az county spatial data
az_counties <- counties(state = 04)

# get az zip code spatial data 
zip_db <- zip_code_db

bc_1 <- read_rds("data/raw/20210426_mindy_griffith_registration.rds")
bc_2 <- read_rds("data/raw/20210617_positive_psychology_registration.rds")

