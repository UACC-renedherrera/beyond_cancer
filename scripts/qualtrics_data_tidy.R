# Data tidy for
# Beyond Cancer Event Registration
# and Survey 

# set up #### 
# load package libraries 
library(here)
library(tidyverse)
# library(ggthemes)
library(qualtRics)
library(zipcodeR)
library(sf)
library(tigris)
library(geojson)
# library(knitr)
library(lubridate)
# library(zipcodeR)
library(janitor)

options(tigris_use_cache = TRUE)

# get az county spatial data
az_counties <- counties(state = 04)

# get az zip code spatial data 
zip_db <- zip_code_db

# qualtrics ####
# collect data from qualtrics for survey
# load list of surveys
surveys <- all_surveys()
# display
surveys

# select zoom registration survey & inspect
# Beyond Cancer 26 April 2021 Mindy Griffith Registration ####
beyond_1_reg <- fetch_survey(surveys$id[1]) %>%
  clean_names()

# inspect
glimpse(beyond_1_reg)

# new variables to identify and
# select
beyond_1_reg <- beyond_1_reg %>%
  mutate(event = "Beyond 1",
         type = "Registration") %>%
  select(response_id,
         event,
         type,
         recorded_date,
         location_latitude,
         location_longitude,
         distribution_channel,
         email = q1_2,
         zip_code = q1_3)

# all lowercase email address 
beyond_1_reg$email <- str_to_lower(beyond_1_reg$email)

# extract email domain 
beyond_1_reg <- beyond_1_reg %>%
  mutate(email_domain = str_extract(email, "@.+"),
         zip_code = as.character(zip_code)) # zip code to character 

# Beyond Cancer 26 April 2021 Mindy Griffith Pre-Event Survey ####
beyond_1_pre_survey <- fetch_survey(surveys$id[6]) %>%
  clean_names()

# inspect
glimpse(beyond_1_pre_survey)

# new variables to identify and
# select
# beyond_1_pre_survey <- 
beyond_1_pre_survey %>%
  mutate(event = "Beyond 1",
         type = "pre_survey") %>%
  select(response_id,
         event,
         type,
         recorded_date,
         location_latitude,
         location_longitude,
         distribution_channel,
         email = q24,
         = q3_2_1,
         )

# all lowercase email address 
beyond_1_pre_survey$email <- str_to_lower(beyond_1_pre_survey$email)

# extract email domain 
beyond_1_pre_survey <- beyond_1_pre_survey %>%
  mutate(email_domain = str_extract(email, "@.+"),
         zip_code = as.character(zip_code)) # zip code to character 

# Beyond Cancer 17 June 2021 Positive Psychology Registration and Survey
beyond_2_reg <- fetch_survey(surveys$id[10]) %>%
  clean_names()

# Beyond Cancer 17 June 2021 Positive Psychology Evaluation
beyond_2_eval <- fetch_survey(surveys$id[9]) %>%
  clean_names()


glimpse(beyond_1_reg)
glimpse(beyond_2_reg)



beyond_1_reg <- beyond_1_reg %>%
  select(8,9,14,15,19,20)

write_rds(beyond_1_reg, "data/raw/20210426_mindy_griffith_registration.rds")

# select zoom registration survey & inspect
beyond_2_reg <- fetch_survey(surveys$id[12])
glimpse(beyond_2_reg)

beyond_2_reg <- beyond_2_reg %>%
  select(8,9,14,15,"Q1.3_1":"Q4.3")

# write_rds(beyond_2_reg, "data/raw/20210617_positive_psychology_registration.rds")
# 
# # select zoom registration survey & inspect
# beyond_2_post <- fetch_survey(surveys$id[11])
# glimpse(beyond_2_reg)
# 
# beyond_2_reg <- beyond_2_reg %>%
#   select(8,9,14,15,"Q1.3_1":"Q4.3")
# 
# write_rds(beyond_2_reg, "data/raw/20210617_positive_psychology_registration.rds")
# 
# # select zoom registration survey & inspect
# beyond_survey <- fetch_survey(surveys$id[7])
# glimpse(beyond_survey)
# 
# # change character to date time 
# beyond_survey$Q4.3 <- as_date(beyond_survey$Q4.3, format = "%m/%d/%Y")
# 
# beyond_survey <- beyond_survey %>%
#   mutate(q4.3_year_dx = year(Q4.3)) 
# 
# # change zip code to character
# 
# beyond_registration <- beyond_registration %>%
#   mutate(zipcode = as.character(Q1.3))
# 
# # list of email addresses registered
# beyond_reg_email <- beyond_registration %>% 
#   select(RecordedDate, Q1.2) %>%
#   arrange(Q1.2) 
# 
# # beyond_reg_email <- str_to_lower(beyond_reg_email$Q1.2) %>%
# #   as_tibble()
# 
# # save list to disk for calendar invitation   
# write_csv(beyond_reg_email, file = "data/tidy/beyond_cancer_registration_email_list.csv")
# 
# # zip code location for beyond cancer registration 
# registration_spatial <- inner_join(beyond_registration, zip_db, by = "zipcode") %>%
#   filter(lat <= 37) %>%
#   filter(lat >= 30) %>%
#   filter(lng <= -109) %>%
#   filter(lng >= -115) %>%
#   select(lng, lat)
# 
# # generate a table of zip code frequencies 
# inner_join(beyond_registration, zip_db, by = "zipcode") %>%
#   filter(lat <= 37) %>%
#   filter(lat >= 30) %>%
#   filter(lng <= -109) %>%
#   filter(lng >= -115) %>%
#   group_by(zipcode) %>%
#   summarize(count = n()) %>%
#   arrange(zipcode) %>%
#   kable(
#     col.names = c("Zipcode", "Count"),
#     caption = "Self-reported AZ zipcode of respondents"
#   )
# 
# # generate a table of county  frequencies 
# inner_join(beyond_registration, zip_db, by = "zipcode") %>%
#   filter(lat <= 37) %>%
#   filter(lat >= 30) %>%
#   filter(lng <= -109) %>%
#   filter(lng >= -115) %>%
#   group_by(county) %>%
#   summarize(count = n()) %>%
#   arrange(desc(count)) %>%
#   kable(
#     col.names = c("AZ County", "Count"),
#     caption = "County containing zip code"
#   )
# 
# registration_spatial <- as.matrix(registration_spatial)
# registration_spatial <- st_multipoint(x = registration_spatial)
# registration_spatial <- st_sfc(registration_spatial, crs = 4269)
# 
# # map of IP address location for beyond cancer registration 
# beyond_reg_mapping <- beyond_registration %>%
#   select(lng = LocationLongitude,
#          lat = LocationLatitude
#   ) %>%
#   filter(lat <= 37) %>%
#   filter(lat >= 30) %>%
#   filter(lng <= -109) %>%
#   filter(lng >= -115)
# 
# beyond_reg_mapping <- as.matrix(beyond_reg_mapping)
# 
# beyond_reg_mapping <- st_multipoint(x = beyond_reg_mapping)
# 
# beyond_reg_mapping <- st_sfc(beyond_reg_mapping, crs = 4269)
# 
# ggplot() +
#   geom_sf(data = az_counties, fill = "#E2E9EB") +
#   geom_sf(data = beyond_reg_mapping, color = "#0C234B", alpha = 0.5, size = 2) +
#   geom_sf(data = registration_spatial, color = "#AB0520", alpha = 0.5, size = 2) +
#   theme_map() +
#   labs(title = "IP Location of Beyond Cancer Registration from Qualtrics",
#        subtitle = "Red = IP address \nBlue = self report zip code",
#        caption = "n = 27")
# 
# st_write(
#   obj = az_counties,
#   dsn = "data/spatial/tigris/az_counties/az_counties.shp",
#   append = FALSE
# )
# 
# st_write(
#   obj = beyond_reg_mapping,
#   dsn = "data/spatial/beyond_cancer_registrations/beyond_cancer_registrations.shp",
#   append = FALSE
# )
# 
# # survey responses 
# glimpse(beyond_survey)
# 
# beyond_survey %>%
#   select(starts_with("Q3.2")) %>%
#   pivot_longer(
#     cols = c(1,3,6,7),
#     names_to = "name",
#     values_to = "value"
#   ) %>%
#   drop_na(value) %>%
#   count(value) %>%
#   arrange(desc(n)) %>%
#   kable(
#     col.names = c("Impact statement", "Number"),
#     caption = "How has cancer impacted your life?"
#   )
# 
# beyond_survey %>%
#   select(starts_with("Q3.3")) %>%
#   pivot_longer(
#     cols = c(1:5,7),
#     names_to = "name",
#     values_to = "value"
#   ) %>%
#   drop_na(value) %>%
#   count(value) %>%
#   arrange(desc(n)) %>%
#   kable(
#     col.names = c("Topic", "Number"),
#     caption = "Which topics are you hoping to hear more about?"
#   )
# 
# beyond_survey %>%
#   select(starts_with("Q3.4")) %>%
#   drop_na() %>%
#   kable(
#     col.names = c("Expectation response"),
#     caption = "What are your expectations?"
#   )
# 
# beyond_survey %>%
#   select(starts_with("Q5.2")) %>%
#   count(Q5.2) %>%
#   arrange(desc(n)) %>%
#   kable(
#     col.names = c("Response", "Number"),
#     caption = "Have you joined a support group?"
#   )  
# 
# beyond_survey %>%
#   select(starts_with("Q5.3")) %>%
#   drop_na() %>%
#   kable(
#     col.names = c("Support Group"),
#     caption = "What is the name of the support group?"
#   )
# 
# beyond_survey %>%
#   select(starts_with("Q5.4")) %>%
#   drop_na() %>%
#   kable(
#     col.names = c("Source / Referral"),
#     caption = "How did you find the support group?"
#   )
# 
# beyond_survey %>%
#   select(starts_with("Q4.6")) %>%
#   drop_na() %>%
#   kable(
#     col.names = c("Resources"),
#     caption = "What other survivorship resources have you accessed or found useful?"
#   )
# 
# beyond_survey %>%
#   select(starts_with("Q4.2")) %>%
#   pivot_longer(
#     cols = c(1,11),
#     names_to = "name",
#     values_to = "value"
#   ) %>%
#   drop_na(value) %>%
#   count(value) %>%
#   arrange(desc(n)) %>%
#   kable(
#     col.names = c("Cancer site", "Number"),
#     caption = "Which cancer are you diagnosed with?"
#   )
# 
# beyond_survey %>%
#   select(q4.3_year_dx) %>%
#   drop_na() %>%
#   arrange(desc(q4.3_year_dx)) %>%
#   kable(
#     col.names = c("Year"),
#     caption = "When were you first diagnosed with cancer?"
#   )
# 
# beyond_survey %>%
#   select(starts_with("Q4.4")) %>%
#   pivot_longer(
#     cols = c(1,4,5,6),
#     names_to = "name",
#     values_to = "value"
#   ) %>%
#   drop_na(value) %>%
#   count(value) %>%
#   arrange(desc(n)) %>%
#   kable(
#     col.names = c("Survivorship statement", "Number"),
#     caption = "Where are you in cancer survivorship journey?"
#   )
# 
# beyond_survey %>%
#   select(starts_with("Q4.5")) %>%
#   count(Q4.5) %>%
#   arrange(desc(n)) %>%
#   kable(
#     col.names = c("Response", "Number"),
#     caption = "Have you been given a survivorship care plan?"
#   )  
# 
