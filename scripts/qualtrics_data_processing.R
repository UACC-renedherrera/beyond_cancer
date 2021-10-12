# Data tidy for
# Beyond Cancer Event Registration
# and Survey 

# set up
# load package libraries 
library(here)
library(tidyverse)
library(ggthemes)

# library(zipcodeR)
# library(sf)
# library(tigris)
# library(geojson)
# library(knitr)
# library(lubridate)
# 
# options(tigris_use_cache = TRUE)
# 
# # get az county spatial data
# az_counties <- counties(state = 04)
# 
# # get az zip code spatial data 
# zip_db <- zip_code_db

# read saved data from disk 
# registration and audience #### 
bcs_1_reg <- read_rds(file = "data/tidy/bcs_1_registration.rds")
bcs_1_pre_survey <- read_rds(file = "data/tidy/bcs_1_pre_event_survey.rds")
bcs_2_reg <- read_rds(file = "data/tidy/bcs_2_registration.rds")
bcs_views <- read_rds(file = "data/tidy/bcs_views.rds")

# views 
bcs_views

# 26 April 2021: mindy griffith ####
glimpse(bcs_1_pre_survey)

# How has cancer impacted your life? 
bcs_1_impact <- bcs_1_pre_survey %>%
  select(starts_with("impact_")) %>%
  mutate(survivor = if_else(impact_survivor == "I am a survivor", 1, 0),
         friend =  if_else(impact_friend == "My friend has or has had cancer", 1, 0),
         family =  if_else(impact_family == "People in my family have or have had cancer", 1, 0),
         patient =  if_else(`impact_recent cancer diagnosis` == "I have a recent cancer diagnosis", 1, 0),
         caregiver =  if_else(impact_caregiver == "I am the caregiver of a person with cancer", 1, 0),
         professional =  if_else(impact_professional == "I am a professional who works in cancer control or care", 1, 0)
         ) %>%
  select(!(starts_with("impact_"))) %>%
  colSums(na.rm = TRUE) %>%
  enframe() %>%
  arrange(desc(value))

# Which cancer are you diagnosed with? 
bcs_1_cancer_dx <- bcs_1_pre_survey %>%
  select(starts_with("cancer_site")) %>%
  mutate(breast = if_else(`cancer_site_Breast (female)` == "Breast (female)", 1, 0),
         prostate = if_else(cancer_site_prostate == "Prostate", 1, 0),
         `lung and bronchus` = if_else(`cancer_site_lung and bronchus` == "Lung and bronchus", 1, 0),
         colorectum = if_else(cancer_site_colorectum == "Colorectum", 1, 0),
         melanoma = if_else(`cancer_site_melanoma of the skin` == "Melanoma of the skin", 1, 0),
         `urinary bladder` = if_else(`cancer_site_urinary bladder` == "Urinary bladder", 1, 0),
         pancreas = if_else(cancer_site_pancreas == "Pancreas", 1, 0),
         `liver and intrahepatic bile duct` = if_else(`cancer_site_liver and intrahepatic bile duct` == "Liver and intrahepatic bile duct", 1, 0)
         ) %>%
  select(!(starts_with("cancer_"))) %>%
  colSums(na.rm = TRUE) %>%
  enframe() %>%
  arrange(desc(value))

# Cancer Support Group 
bcs_1_support_group <- bcs_1_pre_survey %>%
  filter(joined_support_group == "Yes") %>%
  select(support_group_name, support_group_found) %>%
  arrange(support_group_name)

# Cancer survivor resources
bcs_1_survivor_resources <- bcs_1_pre_survey %>%
  filter(survivor_resources != is.na(survivor_resources)) %>%
  select(survivor_resources) %>%
  arrange(survivor_resources)

# Where are you in your cancer survivorship journey?
bcs_1_survivor_journey <- bcs_1_pre_survey %>%
  select(starts_with("cancer_survivorship_")) %>%
  mutate(
    prevention = if_else(cancer_survivorship_prevention == "Prevention", 1, 0),
    `detection screening` = if_else(`cancer_survivorship_detection and screening` == "Detection and Screening", 1, 0),
    `recent diagnosis` = if_else(`cancer_survivorship_recent diagnosis` == "Recent diagnosis (acute survivorship)", 1, 0),
    treatment = if_else(cancer_survivorship_treatment == "Treatment (acute survivorship / extended survivorship)", 1, 0),
    survivorship = if_else(cancer_survivorship_survivorship == "Survivorship (extended survivorship / permanent survivorship)", 1, 0),
    `self advocacy` = if_else(`cancer_survivorship_self advocacy` == "Self-advocacy", 1, 0)
  ) %>%
  select(!(starts_with("cancer_"))) %>%
  colSums(na.rm = TRUE) %>%
  enframe() %>%
  arrange(desc(value))

# Have you been given a survivorship care plan?
bcs_1_survivor_care_plan <- bcs_1_pre_survey %>%
  select(`survivorship care plan`) %>%
  group_by(`survivorship care plan`) %>%
  count()

# 17 June 2021: positive psychology ####
glimpse(bcs_2_reg)

# How has cancer impacted your life? 
bcs_2_impact <- bcs_2_reg %>%
  select(starts_with("impact_")) %>%
  mutate(patient =  if_else(`impact_Cancer patient` == "Cancer patient", 1, 0),
         survivor = if_else(impact_survivor == "Cancer survivor", 1, 0),
         navigator = if_else(impact_Navigator == "Navigator", 1, 0),
         provider = if_else(`impact_Physician Provider` == "Physician", 1, 0),
         nurse = if_else(impact_Nurse == "Nurse", 1, 0),
         researcher = if_else(impact_Researcher == "Researcher", 1, 0),
         `family caretaker` =  if_else(`impact_Family Caretaker` == "Family/Caretaker", 1, 0)
  ) %>%
  select(!(starts_with("impact_"))) %>%
  colSums(na.rm = TRUE) %>%
  enframe() %>%
  arrange(desc(value))

# preferred language 
bcs_2_lang <- bcs_2_reg %>%
  select(preferred_language) %>%
  group_by(preferred_language) %>%
  count()

# what is the most valuable resource for cancer patients and survivors?
bcs_2_survivor_resources <- bcs_2_reg %>%
  select(most_valuable_resource) %>%
  drop_na() %>%
  arrange(most_valuable_resource)

# Where are you in your cancer survivorship journey?
bcs_2_survivor_journey <- bcs_2_reg %>%
  select(starts_with("cancer_survivorship_")) %>%
  mutate(
    prevention = if_else(cancer_survivorship_prevention == "Prevention", 1, 0),
    `detection and screening` = if_else(`cancer_survivorship_detection and screening` == "Detection and Screening", 1, 0),
    `recent diagnosis` = if_else(`cancer_survivorship_recent diagnosis` == "Recent diagnosis (acute survivorship)", 1, 0),
    treatment = if_else(cancer_survivorship_treatment == "Treatment (acute survivorship / extended survivorship)", 1, 0),
    survivorship = if_else(cancer_survivorship_survivorship == "Survivorship (extended survivorship / permanent survivorship)", 1, 0),
    `self advocacy` = if_else(`cancer_survivorship_self advocacy` == "Self-advocacy", 1, 0)
  ) %>%
  select(!(starts_with("cancer_"))) %>%
  colSums(na.rm = TRUE) %>%
  enframe() %>%
  arrange(desc(value))

# have you sought out mental health services?
bcs_2_mental_health_hx <- bcs_2_reg %>%
  select(`mental health treatment`) %>%
  group_by(`mental health treatment`) %>%
  count()

# do you plan to seek out mental health services?
bcs_2_mental_health_serv <- bcs_2_reg %>%
  select(`mental health intent`) %>%
  group_by(`mental health intent`) %>%
  count()


# save email list from previous registration to disc
email_list <- full_join(bcs_1_reg, bcs_2_reg) %>%
  select(email) %>%
  arrange(email) %>%
  distinct(email)

email_list %>%
  write_csv("data/tidy/beyond_cancer_email_list.csv")
