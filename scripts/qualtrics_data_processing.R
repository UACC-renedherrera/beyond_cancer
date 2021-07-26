# Data tidy for
# Beyond Cancer Event Registration
# and Survey 

# set up
# load package libraries 
library(here)
library(tidyverse)
library(ggthemes)
library(zipcodeR)
library(sf)
library(tigris)
library(geojson)
library(knitr)
library(lubridate)

options(tigris_use_cache = TRUE)

# get az county spatial data
az_counties <- counties(state = 04)

# get az zip code spatial data 
zip_db <- zip_code_db

# registration and audience #### 
bc_1 <- read_rds("data/tidy/beyond_1_pre_survey.rds")
bc_2 <- read_rds("data/tidy/beyond_2_reg.rds")

glimpse(bc_1)

# How has cancer impacted your life? 
bc_1 %>%
  select(starts_with("impact_")) %>%
  mutate(survivor = if_else(impact_survivor == "I am a survivor", 1, 0),
         friend =  if_else(impact_friend == "My friend has or has had cancer", 1, 0),
         family =  if_else(impact_family == "People in my family have or have had cancer", 1, 0),
         patient =  if_else(`impact_recent cancer diagnosis` == "I have a recent cancer diagnosis", 1, 0),
         caregiver =  if_else(impact_caregiver == "I am the caregiver of a person with cancer", 1, 0),
         professional =  if_else(impact_professional == "I am a professional who works in cancer control or care", 1, 0)
         ) %>%
  select(!(starts_with("impact_"))) %>%
  colSums(na.rm = TRUE)

# Cancer Support Group 
bc_1 %>%
  filter(joined_support_group == "Yes") %>%
  select(support_group_name, support_group_found)

# Cancer Support Group 
bc_1 %>%
  filter(survivor_resources != is.na(survivor_resources)) %>%
  select(survivor_resources)

# Where are you in your cancer survivorship journey?
bc_1 %>%
  select(starts_with("cancer_survivorship_")) %>%
  mutate(
    prevention = if_else(cancer_survivorship_prevention == "Prevention", 1, 0),
    detection_screening = if_else(`cancer_survivorship_detection and screening` == "Detection and Screening", 1, 0),
    recent_dx = if_else(`cancer_survivorship_recent diagnosis` == "Recent diagnosis (acute survivorship)", 1, 0),
    tx = if_else(cancer_survivorship_treatment == "Treatment (acute survivorship / extended survivorship)", 1, 0),
    survivorship = if_else(cancer_survivorship_survivorship == "Survivorship (extended survivorship / permanent survivorship)", 1, 0),
    self_advocacy = if_else(`cancer_survivorship_self advocacy` == "Self-advocacy", 1, 0)
  ) %>%
  select(!(starts_with("cancer_"))) %>%
  colSums(na.rm = TRUE)

# Have you been given a survivorship care plan?
bc_1 %>%
  select(`survivorship care plan`) %>%
  group_by(`survivorship care plan`) %>%
  count()

glimpse(bc_2)

# How has cancer impacted your life? 
bc_2 %>%
  select(starts_with("impact_")) %>%
  mutate(patient =  if_else(`impact_Cancer patient` == "Cancer patient", 1, 0),
         survivor = if_else(impact_survivor == "Cancer survivor", 1, 0),
         navigator = if_else(impact_Navigator == "Navigator", 1, 0),
         provider = if_else(`impact_Physician Provider` == "Physician", 1, 0),
         nurse = if_else(impact_Nurse == "Nurse", 1, 0),
         researcher = if_else(impact_Researcher == "Researcher", 1, 0),
         family_caretaker =  if_else(`impact_Family Caretaker` == "Family/Caretaker", 1, 0)
  ) %>%
  select(!(starts_with("impact_"))) %>%
  colSums(na.rm = TRUE)

# preferred language 
bc_2 %>%
  select(preferred_language) %>%
  group_by(preferred_language) %>%
  count()

# what is the most valuable resource for cancer patients and survivors?
bc_2 %>%
  select(most_valuable_resource) %>%
  drop_na()

# Where are you in your cancer survivorship journey?
bc_2 %>%
  select(starts_with("cancer_survivorship_")) %>%
  mutate(
    prevention = if_else(cancer_survivorship_prevention == "Prevention", 1, 0),
    detection_screening = if_else(`cancer_survivorship_detection and screening` == "Detection and Screening", 1, 0),
    recent_dx = if_else(`cancer_survivorship_recent diagnosis` == "Recent diagnosis (acute survivorship)", 1, 0),
    tx = if_else(cancer_survivorship_treatment == "Treatment (acute survivorship / extended survivorship)", 1, 0),
    survivorship = if_else(cancer_survivorship_survivorship == "Survivorship (extended survivorship / permanent survivorship)", 1, 0),
    self_advocacy = if_else(`cancer_survivorship_self advocacy` == "Self-advocacy", 1, 0)
  ) %>%
  select(!(starts_with("cancer_"))) %>%
  colSums(na.rm = TRUE)

# have you sought out mental health services?
bc_2 %>%
  select(`mental health treatment`) %>%
  group_by(`mental health treatment`) %>%
  count()

# do you plan to seek out mental health services?
bc_2 %>%
  select(`mental health intent`) %>%
  group_by(`mental health intent`) %>%
  count()

