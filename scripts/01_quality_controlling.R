# 01_data_cleaning.R
# Data preparation for DSM analysis
# Note: segmented data (segs and obs) were provided by Angus Henderson and are used as starting point.


# Load libraries
library(tidyverse)
library(basf)
library(sf)
library(dplyr)

# load/Import raw data (example, replace with your own)
load(segmentdata.Rdata)

# Quick checks
summary(segs)
summary(obs)

# making a table to understand the survey effort across the Seaons and Vessels before quality controlling the data 
survey_effort_summary <- obs %>%
  st_drop_geometry() %>%
  mutate(vessel = str_extract(voy_id, "(?<=_)[A-Za-z]+_[A-Za-z]+")) %>%
  group_by(Season, vessel, voy_id) %>%
  summarise(
    total_whales = sum(BestNumber, na.rm = TRUE),
    sightings = n(),
    .groups = "drop") %>%
  group_by(Season, vessel) %>%
  summarise(
    voyages = n_distinct(voy_id),
    total_whales = sum(total_whales),
    total_sightings = sum(sightings),
    .groups = "drop") %>%
 arrange(Season, vessel)

survey_effort_summary
# should output a table with total whales sightings and counts per season and vessel with a total number of voyages from each voyage


# ================================
# Quality controlling data
# ===============================

# filtering the obs and segs

obs_f <- obs %>% 
  st_drop_geometry() %>% 
  mutate(Angle_shift = abs(ifelse(Angle >180, Angle-360, Angle))) %>% 
  mutate(BestNumber = cut(BestNumber, breaks = c(0,1,2,3,4,10,Inf), 
                          labels = c("1","2", "3", "4","5+", "10+"))) %>% 
  filter(Sightability %in% c("3","4","5")) %>% 
  filter(Platform == "BR", SeaState < 6) %>% 
  mutate(dist_type2 = case_when( dist_type == "ret"~ "ret",
                                 dist_type %in% c( "land", "retland") ~ "land",
                                 dist_type == "close" ~ "close",
                                 TRUE ~ "other")
  ) %>% 
  filter(Angle > 270 | Angle < 90) %>% 
  mutate(Speedbined = case_when((Speed*0.54) < 8 ~ "slow",
                                (Speed*0.54) > 8 & (Speed*0.54) <12 ~ "good",
                                (Speed*0.54) >12 ~"fast")) %>% 
  filter(Angle <361)

segs_f <- segs %>% 
  st_drop_geometry() %>% 
  filter(Sightability %in% c("3","4","5")) %>% 
  mutate(Effort=Effort/2)


