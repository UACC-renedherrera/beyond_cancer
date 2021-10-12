# Data tidy for
# Beyond Cancer Event Registration
# and Survey 

# set up #### 
# load package libraries 
library(here)
library(tidyverse)
library(qualtRics) #connect to qualtrics
library(lubridate) #dates and times
library(janitor) #clean up variable names

# read data from qualtrics ####
# collect data from qualtrics for survey
# load list of surveys
surveys <- all_surveys()
# display
surveys

# Beyond Cancer 26 April 2021 Mindy Griffith #### 
# Registration 
bcs_1_reg <- fetch_survey(surveys$id[1]) %>%
  clean_names()

# inspect
glimpse(bcs_1_reg)

# new variables to identify and
# select
bcs_1_reg <- bcs_1_reg %>%
  mutate(event = "BCS 1",
         type = "Registration") %>%
  select(response_id,
         event,
         type,
         recorded_date,
         location_latitude,
         location_longitude,
         email = q1_2,
         zip_code = q1_3)

# all lowercase email address 
bcs_1_reg$email <- str_to_lower(bcs_1_reg$email)

# extract email domain 
bcs_1_reg <- bcs_1_reg %>%
  mutate(email_domain = str_extract(email, "@.+"),
         zip_code = as.character(zip_code)) # zip code to character 

# Beyond Cancer 26 April 2021 Mindy Griffith Pre-Event Survey ####
bcs_1_pre_survey <- fetch_survey(surveys$id[8]) %>%
  clean_names()

# inspect
glimpse(bcs_1_pre_survey)

# new variables to identify and
# select
bcs_1_pre_survey <- bcs_1_pre_survey %>%
  mutate(event = "BCS 1",
         type = "pre_survey") %>%
  select(response_id,
         event,
         type,
         recorded_date,
         location_latitude,
         location_longitude,
         email = q24,
         "impact_survivor" = q3_2_1,
         "impact_friend" = q3_2_2,
         "impact_family" = q3_2_3,
         "impact_recent cancer diagnosis" = q3_2_4,
         "impact_caregiver" = q3_2_5,
         "impact_professional" = q3_2_6,
         cancer_impact = q3_2_7,
         cancer_impact_text = q3_2_7_text,
         "needs_topics_Caregiver well-being" = q3_3_1,
         "needs_topics_Positive psychology" = q3_3_2,
         "needs_topics_financial hardships" = q3_3_3,
         "needs_topics_Active lifestyle and healthy diet" = q3_3_4,
         "needs_topics_Self-advocacy" = q3_3_5,
         "needs_topics_Support and community" = q3_3_6,
         "needs_topics_Survivorship care plans" = q3_3_8 ,
         needs_topics = q3_3_7,
         needs_topics_text = q3_3_7_text,
         event_expectations = q3_4,
         joined_support_group = q5_2,
         support_group_name = q5_3,
         support_group_found = q5_4,
         survivor_resources = q4_6 ,
         "cancer_site_Breast (female)" = q4_2_1,
         cancer_site_prostate = q4_2_2,
         "cancer_site_lung and bronchus" = q4_2_3,
         cancer_site_colorectum = q4_2_4,
         "cancer_site_melanoma of the skin" = q4_2_5,
         "cancer_site_urinary bladder" = q4_2_6,
         cancer_site_pancreas = q4_2_7,
         "cancer_site_liver and intrahepatic bile duct" = q4_2_8,
         "no cancer diagnosis" = q4_2_9,
         "other cancer diagnosis" = q4_2_10,
         "other cancer diagnosis text" = q4_2_10_text,
         "diagnosis date" = q4_3,
         "cancer_survivorship_prevention" = q4_4_1,
         "cancer_survivorship_detection and screening" = q4_4_2,
         "cancer_survivorship_recent diagnosis" = q4_4_3,
         "cancer_survivorship_treatment" = q4_4_4,
         "cancer_survivorship_survivorship" = q4_4_5,
         "cancer_survivorship_self advocacy" = q4_4_6,
         "cancer_survivorship_other" = q4_4_7,
         "cancer_survivorship_other_text" = q4_4_7_text,
         "survivorship care plan" = q4_5,
         "email" = q24
  )

# all lowercase email address 
bcs_1_pre_survey$email <- str_to_lower(bcs_1_pre_survey$email)

# extract email domain 
bcs_1_pre_survey <- bcs_1_pre_survey %>%
  mutate(email_domain = str_extract(email, "@.+"))

# inspect
glimpse(bcs_1_pre_survey)

# Beyond Cancer 17 June 2021 Positive Psychology Registration and Survey ####
bcs_2_reg <- fetch_survey(surveys$id[12]) %>%
  clean_names()

# inspect
glimpse(bcs_2_reg)

# new variables to identify and
# select
bcs_2_reg <- bcs_2_reg %>%
  mutate(event = "BCS 2",
         type = "Registration") %>%
  select(response_id,
         event,
         type,
         recorded_date,
         location_latitude,
         location_longitude,
         email = q1_3_3,
         zip_code = q1_3_4,
         survey_participation = q2_2,
         preferred_language = q3_1,
         preferred_language_text = q3_1_3_text,
         "impact_Cancer patient" = q3_2_1,
         "impact_survivor" = q3_2_2,
         "impact_Navigator" = q3_2_3,
         "impact_Physician Provider" = q3_2_4,
         "impact_Nurse" = q3_2_5,
         "impact_Researcher" = q3_2_6,
         "impact_Family Caretaker" = q3_2_7,
         cancer_impact = q3_2_8,
         cancer_impact_text = q3_2_8_text,
         most_valuable_resource = q3_3,
         "cancer_survivorship_prevention" = q4_1_1,
         "cancer_survivorship_detection and screening" = q4_1_2,
         "cancer_survivorship_recent diagnosis" = q4_1_3,
         "cancer_survivorship_treatment" = q4_1_4,
         "cancer_survivorship_survivorship" = q4_1_5,
         "cancer_survivorship_self advocacy" = q4_1_6,
         "cancer_survivorship_other" = q4_1_7,
         "cancer_survivorship_other_text" = q4_1_7_text,
         "cancer_survivorship_does not apply" = q4_1_8,
         "mental health treatment" = q4_2,
         "mental health intent" = q4_3
  )

# all lowercase email address 
bcs_2_reg$email <- str_to_lower(bcs_2_reg$email)

# extract email domain 
bcs_2_reg <- bcs_2_reg %>%
  mutate(email_domain = str_extract(email, "@.+"),
         zip_code = as.character(zip_code)) # zip code to character 

# inspect
glimpse(bcs_2_reg)

# Beyond Cancer 17 June 2021 Positive Psychology Evaluation ####
bcs_2_eval <- fetch_survey(surveys$id[11]) %>%
  clean_names()

# inspect
glimpse(bcs_2_eval)

# new variables to identify and
# select
bcs_2_eval <- bcs_2_eval %>%
  mutate(event = "BCS 2",
         type = "post_eval") %>%
  select(response_id,
         event,
         type,
         recorded_date,
         location_latitude,
         location_longitude,
         email = recipient_email,
         zip_code = q1_2_1,
         exceeded_expectations = q2_2_1,
         knowledgeable_panelists = q2_2_2,
         culturally_appropriate = q2_2_3,
         learned_something = q2_2_4,
         register_again = q2_2_5,
         personal_connection = q3_2,
         info_new_to_me = q3_3,
         event_takeaway_resources = q3_4,
         suggested_changes_culture = q3_5,
         other_notes = q3_6,
         suggested_presenter = q4_2,
         prioritize_topic_caregiver = q4_3_1,
         prioritize_topic_positive_psych = q4_3_2,
         prioritize_topic_finance = q4_3_3,
         prioritize_topic_self_advocacy = q4_3_4,
         prioritize_topic_support = q4_3_5,
         prioritize_topic_survivorship = q4_3_6,
         prioritize_topic_other = q4_3_7,
         prioritize_topic_other_text = q4_3_7_text
  )

# all lowercase email address 
bcs_2_eval$email <- str_to_lower(bcs_2_eval$email)

# extract email domain 
bcs_2_eval <- bcs_2_eval %>%
  mutate(email_domain = str_extract(email, "@.+"),
         zip_code = as.character(zip_code)) # zip code to character 

# inspect
glimpse(bcs_2_eval)

# views and engagement 
# build tibble 
bcs_views <- tribble(
  ~event, ~zoom, ~twitter_live, ~twitter_replay, ~facebook_live, ~fb_reach, ~fb_engagements,
  "Mindy Griffith", 21, 21, 28, 2, 204, 41,
  "Positive Psychology", 34, NA, NA, 3, 73, 17
)

# replace NA with 0 for row sum purposes
bcs_views <- bcs_views %>%
  replace(is.na(.), 0)

# make the calculations 
bcs_views$total <- rowSums(bcs_views[,c(-1)])

# inspect 
glimpse(bcs_views)

# save as RDS 
write_rds(bcs_1_reg, file = "data/tidy/bcs_1_registration.rds")
write_rds(bcs_1_pre_survey, file = "data/tidy/bcs_1_pre_event_survey.rds")
write_rds(bcs_2_reg, file = "data/tidy/bcs_2_registration.rds")
write_rds(bcs_views, file = "data/tidy/bcs_views.rds")





library(zipcodeR)
# library(sf)
# library(tigris)
# library(geojson)
# library(knitr)

options(tigris_use_cache = TRUE)

# get az county spatial data
az_counties <- counties(state = 04)

write_rds(az_counties, "data/spatial/az_counties.rds")

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
bcs_1_reg <- fetch_survey(surveys$id[1]) %>%
  clean_names()

# inspect
glimpse(bcs_1_reg)

# new variables to identify and
# select
bcs_1_reg <- bcs_1_reg %>%
  mutate(event = "BCS 1",
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
bcs_1_reg$email <- str_to_lower(bcs_1_reg$email)

# extract email domain 
bcs_1_reg <- bcs_1_reg %>%
  mutate(email_domain = str_extract(email, "@.+"),
         zip_code = as.character(zip_code)) # zip code to character 

write_rds(bcs_1_reg, "data/tidy/bcs_1_reg.rds")

# Beyond Cancer 26 April 2021 Mindy Griffith Pre-Event Survey ####
bcs_1_pre_survey <- fetch_survey(surveys$id[6]) %>%
  clean_names()

# inspect
glimpse(bcs_1_pre_survey)

# new variables to identify and
# select
bcs_1_pre_survey <- bcs_1_pre_survey %>%
  mutate(event = "BCS 1",
         type = "pre_survey") %>%
  select(response_id,
         event,
         type,
         recorded_date,
         location_latitude,
         location_longitude,
         distribution_channel,
         email = q24,
         "impact_survivor" = q3_2_1,
         "impact_friend" = q3_2_2,
         "impact_family" = q3_2_3,
         "impact_recent cancer diagnosis" = q3_2_4,
         "impact_caregiver" = q3_2_5,
         "impact_professional" = q3_2_6,
         cancer_impact = q3_2_7,
         cancer_impact_text = q3_2_7_text,
         "needs_topics_Caregiver well-being" = q3_3_1,
         "needs_topics_Positive psychology" = q3_3_2,
         "needs_topics_financial hardships" = q3_3_3,
         "needs_topics_Active lifestyle and healthy diet" = q3_3_4,
         "needs_topics_Self-advocacy" = q3_3_5,
         "needs_topics_Support and community" = q3_3_6,
         "needs_topics_Survivorship care plans" = q3_3_8 ,
         needs_topics = q3_3_7,
         needs_topics_text = q3_3_7_text,
         event_expectations = q3_4,
         joined_support_group = q5_2,
         support_group_name = q5_3,
         support_group_found = q5_4,
         survivor_resources = q4_6 ,
         "cancer_site_Breast (female)" = q4_2_1,
         cancer_site_prostate = q4_2_2,
         "cancer_site_lung and bronchus" = q4_2_3,
         cancer_site_colorectum = q4_2_4,
         "cancer_site_melanoma of the skin" = q4_2_5,
         "cancer_site_urinary bladder" = q4_2_6,
         cancer_site_pancreas = q4_2_7,
         "cancer_site_liver and intrahepatic bile duct" = q4_2_8,
         "no cancer diagnosis" = q4_2_9,
         "other cancer diagnosis" = q4_2_10,
         "other cancer diagnosis text" = q4_2_10_text,
         "diagnosis date" = q4_3,
         "cancer_survivorship_prevention" = q4_4_1,
         "cancer_survivorship_detection and screening" = q4_4_2,
         "cancer_survivorship_recent diagnosis" = q4_4_3,
         "cancer_survivorship_treatment" = q4_4_4,
         "cancer_survivorship_survivorship" = q4_4_5,
         "cancer_survivorship_self advocacy" = q4_4_6,
         "cancer_survivorship_other" = q4_4_7,
         "cancer_survivorship_other_text" = q4_4_7_text,
         "survivorship care plan" = q4_5,
         "email" = q24
         )

# all lowercase email address 
bcs_1_pre_survey$email <- str_to_lower(bcs_1_pre_survey$email)

# extract email domain 
bcs_1_pre_survey <- bcs_1_pre_survey %>%
  mutate(email_domain = str_extract(email, "@.+"))

write_rds(bcs_1_pre_survey, "data/tidy/bcs_1_pre_survey.rds")

# Beyond Cancer 17 June 2021 Positive Psychology Registration and Survey ####
bcs_2_reg <- fetch_survey(surveys$id[10]) %>%
  clean_names()

# inspect
glimpse(bcs_2_reg)

# new variables to identify and
# select
bcs_2_reg <- bcs_2_reg %>%
  mutate(event = "BCS 2",
         type = "Registration") %>%
  select(response_id,
         event,
         type,
         recorded_date,
         location_latitude,
         location_longitude,
         distribution_channel,
         email = q1_3_3,
         zip_code = q1_3_4,
         survey_participation = q2_2,
         preferred_language = q3_1,
         preferred_language_text = q3_1_3_text,
         "impact_Cancer patient" = q3_2_1,
         "impact_survivor" = q3_2_2,
         "impact_Navigator" = q3_2_3,
         "impact_Physician Provider" = q3_2_4,
         "impact_Nurse" = q3_2_5,
         "impact_Researcher" = q3_2_6,
         "impact_Family Caretaker" = q3_2_7,
         cancer_impact = q3_2_8,
         cancer_impact_text = q3_2_8_text,
         most_valuable_resource = q3_3,
         "cancer_survivorship_prevention" = q4_1_1,
         "cancer_survivorship_detection and screening" = q4_1_2,
         "cancer_survivorship_recent diagnosis" = q4_1_3,
         "cancer_survivorship_treatment" = q4_1_4,
         "cancer_survivorship_survivorship" = q4_1_5,
         "cancer_survivorship_self advocacy" = q4_1_6,
         "cancer_survivorship_other" = q4_1_7,
         "cancer_survivorship_other_text" = q4_1_7_text,
         "cancer_survivorship_does not apply" = q4_1_8,
         "mental health treatment" = q4_2,
         "mental health intent" = q4_3
         )

# all lowercase email address 
bcs_2_reg$email <- str_to_lower(bcs_2_reg$email)

# extract email domain 
bcs_2_reg <- bcs_2_reg %>%
  mutate(email_domain = str_extract(email, "@.+"),
         zip_code = as.character(zip_code)) # zip code to character 

write_rds(bcs_2_reg, "data/tidy/bcs_2_reg.rds")

# Beyond Cancer 17 June 2021 Positive Psychology Evaluation ####
bcs_2_eval <- fetch_survey(surveys$id[9]) %>%
  clean_names()

# inspect
glimpse(bcs_2_eval)

# new variables to identify and
# select
bcs_2_eval <- bcs_2_eval %>%
  mutate(event = "BCS 2",
         type = "post_eval") %>%
  select(response_id,
         event,
         type,
         recorded_date,
         location_latitude,
         location_longitude,
         distribution_channel,
         email = recipient_email,
         zip_code = q1_2_1,
         exceeded_expectations = q2_2_1,
         knowledgeable_panelists = q2_2_2,
         culturally_appropriate = q2_2_3,
         learned_something = q2_2_4,
         register_again = q2_2_5,
         personal_connection = q3_2,
         info_new_to_me = q3_3,
         event_takeaway_resources = q3_4,
         suggested_changes_culture = q3_5,
         other_notes = q3_6,
         suggested_presenter = q4_2,
         prioritize_topic_caregiver = q4_3_1,
         prioritize_topic_positive_psych = q4_3_2,
         prioritize_topic_finance = q4_3_3,
         prioritize_topic_self_advocacy = q4_3_4,
         prioritize_topic_support = q4_3_5,
         prioritize_topic_survivorship = q4_3_6,
         prioritize_topic_other = q4_3_7,
         prioritize_topic_other_text = q4_3_7_text
  )

# all lowercase email address 
bcs_2_eval$email <- str_to_lower(bcs_2_eval$email)

# extract email domain 
bcs_2_eval <- bcs_2_eval %>%
  mutate(email_domain = str_extract(email, "@.+"),
         zip_code = as.character(zip_code)) # zip code to character 

write_rds(bcs_2_eval, "data/tidy/bcs_2_eval.rds")


# stop here #### 


# mapping 
b1_spatial <- bcs_1_reg %>%
  select(response_id,
         location_latitude,
         location_longitude) %>%
  filter(location_latitude > 30,
         location_latitude < 40,
         location_longitude > -116,
         location_longitude < -108)

b1_sf <- st_as_sf(b1_spatial,
                  coords = c("location_longitude", "location_latitude"),
                  crs = 4326)

b2_spatial <- bcs_2_reg %>%
  select(response_id,
         location_latitude,
         location_longitude) %>%
  filter(location_latitude > 30,
         location_latitude < 40,
         location_longitude > -116,
         location_longitude < -108)

b2_sf <- st_as_sf(b2_spatial,
                  coords = c("location_longitude", "location_latitude"),
                  crs = 4326)

ggplot() +
  geom_sf(data = az_counties) +
  geom_sf(data = b1_sf, color = "blue", alpha = .60) +
  geom_sf(data = b2_sf, color = "red", alpha = .60) +
  theme_void()

  


bcs_views <- tribble(
  ~event, ~zoom, ~twitter_live, ~twitter_replay, ~facebook_live, ~fb_reach, ~fb_engagements,
  "Mindy Griffith", 21, 21, 28, 2, 204, 41,
  "Positive Psychology", 34, NA, NA, 3, 73, 17
)

bcs_views <- bcs_views %>%
  replace(is.na(.), 0)

bcs_views$total <- rowSums(bcs_views[,c(-1)])


# zip codes for beyond cancer 3 16 Oct Registration 
bcs_3_zip_codes <- fetch_survey(surveys$id[8]) %>%
  clean_names() %>%
  select(in_person = q1_3,
         zipcode = q1_2_4) %>%
  arrange(zipcode) %>%
  mutate(zipcode = as.character(zipcode))

glimpse(bcs_3_zip_codes)

write_rds(bcs_3_zip_codes, "data/tidy/bcs_3_registration_zip_codes.rds")

# # 
# 
# glimpse(bcs_1_reg)
# glimpse(bcs_2_reg)
# 
# 
# 
# bcs_1_reg <- bcs_1_reg %>%
#   select(8,9,14,15,19,20)
# 
# write_rds(bcs_1_reg, "data/raw/20210426_mindy_griffith_registration.rds")
# 
# # select zoom registration survey & inspect
# bcs_2_reg <- fetch_survey(surveys$id[12])
# glimpse(bcs_2_reg)
# 
# bcs_2_reg <- bcs_2_reg %>%
#   select(8,9,14,15,"Q1.3_1":"Q4.3")

# write_rds(bcs_2_reg, "data/raw/20210617_positive_psychology_registration.rds")
# 
# # select zoom registration survey & inspect
# bcs_2_post <- fetch_survey(surveys$id[11])
# glimpse(bcs_2_reg)
# 
# bcs_2_reg <- bcs_2_reg %>%
#   select(8,9,14,15,"Q1.3_1":"Q4.3")
# 
# write_rds(bcs_2_reg, "data/raw/20210617_positive_psychology_registration.rds")
# 
# # select zoom registration survey & inspect
# bcs_survey <- fetch_survey(surveys$id[7])
# glimpse(bcs_survey)
# 
# # change character to date time 
# bcs_survey$Q4.3 <- as_date(bcs_survey$Q4.3, format = "%m/%d/%Y")
# 
# bcs_survey <- bcs_survey %>%
#   mutate(q4.3_year_dx = year(Q4.3)) 
# 
# # change zip code to character
# 
# bcs_registration <- bcs_registration %>%
#   mutate(zipcode = as.character(Q1.3))
# 
# # list of email addresses registered
# bcs_reg_email <- bcs_registration %>% 
#   select(RecordedDate, Q1.2) %>%
#   arrange(Q1.2) 
# 
# # bcs_reg_email <- str_to_lower(bcs_reg_email$Q1.2) %>%
# #   as_tibble()
# 
# # save list to disk for calendar invitation   
# write_csv(bcs_reg_email, file = "data/tidy/bcs_cancer_registration_email_list.csv")
# 
# # zip code location for beyond cancer registration 
# registration_spatial <- inner_join(bcs_registration, zip_db, by = "zipcode") %>%
#   filter(lat <= 37) %>%
#   filter(lat >= 30) %>%
#   filter(lng <= -109) %>%
#   filter(lng >= -115) %>%
#   select(lng, lat)
# 
# # generate a table of zip code frequencies 
# inner_join(bcs_registration, zip_db, by = "zipcode") %>%
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
# inner_join(bcs_registration, zip_db, by = "zipcode") %>%
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
# bcs_reg_mapping <- bcs_registration %>%
#   select(lng = LocationLongitude,
#          lat = LocationLatitude
#   ) %>%
#   filter(lat <= 37) %>%
#   filter(lat >= 30) %>%
#   filter(lng <= -109) %>%
#   filter(lng >= -115)
# 
# bcs_reg_mapping <- as.matrix(bcs_reg_mapping)
# 
# bcs_reg_mapping <- st_multipoint(x = bcs_reg_mapping)
# 
# bcs_reg_mapping <- st_sfc(bcs_reg_mapping, crs = 4269)
# 
# ggplot() +
#   geom_sf(data = az_counties, fill = "#E2E9EB") +
#   geom_sf(data = bcs_reg_mapping, color = "#0C234B", alpha = 0.5, size = 2) +
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
#   obj = bcs_reg_mapping,
#   dsn = "data/spatial/bcs_cancer_registrations/bcs_cancer_registrations.shp",
#   append = FALSE
# )
# 
# # survey responses 
# glimpse(bcs_survey)
# 
# bcs_survey %>%
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
# bcs_survey %>%
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
# bcs_survey %>%
#   select(starts_with("Q3.4")) %>%
#   drop_na() %>%
#   kable(
#     col.names = c("Expectation response"),
#     caption = "What are your expectations?"
#   )
# 
# bcs_survey %>%
#   select(starts_with("Q5.2")) %>%
#   count(Q5.2) %>%
#   arrange(desc(n)) %>%
#   kable(
#     col.names = c("Response", "Number"),
#     caption = "Have you joined a support group?"
#   )  
# 
# bcs_survey %>%
#   select(starts_with("Q5.3")) %>%
#   drop_na() %>%
#   kable(
#     col.names = c("Support Group"),
#     caption = "What is the name of the support group?"
#   )
# 
# bcs_survey %>%
#   select(starts_with("Q5.4")) %>%
#   drop_na() %>%
#   kable(
#     col.names = c("Source / Referral"),
#     caption = "How did you find the support group?"
#   )
# 
# bcs_survey %>%
#   select(starts_with("Q4.6")) %>%
#   drop_na() %>%
#   kable(
#     col.names = c("Resources"),
#     caption = "What other survivorship resources have you accessed or found useful?"
#   )
# 
# bcs_survey %>%
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
# bcs_survey %>%
#   select(q4.3_year_dx) %>%
#   drop_na() %>%
#   arrange(desc(q4.3_year_dx)) %>%
#   kable(
#     col.names = c("Year"),
#     caption = "When were you first diagnosed with cancer?"
#   )
# 
# bcs_survey %>%
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
# bcs_survey %>%
#   select(starts_with("Q4.5")) %>%
#   count(Q4.5) %>%
#   arrange(desc(n)) %>%
#   kable(
#     col.names = c("Response", "Number"),
#     caption = "Have you been given a survivorship care plan?"
#   )  
# 
