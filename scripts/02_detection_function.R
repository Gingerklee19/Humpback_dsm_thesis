# 02_detection_function.R

# Load required libraries
library(Distance)

# Load quality controled data set
load("1.filtered_segs_obs.Rdata")

# set a truncation distance
truncdist <- 15%

# creating the detection funciton in both hr nd hn and plotting 
ds_hr <- ds(
  data = obs_rp, 
  truncation = truncdist, 
  key = "hr", 
  adjustment = NULL)

plot(ds_hr)

ds_hn <- ds(
  data = obs_f, 
  truncation = truncdist, 
  key = "hn", 
  adjustment = NULL)

plot(ds_hn)

# testing the effet of covaraites in the detection function

ds_hr_seastate <- ds(
   data = obs_f,
   truncation = truncdist,
   key = "hr",
   formula = ~ SeaState
 )

ds_hn_seastate <- ds(
   data = obs_f,
   truncation = truncdist,
   key = "hn",
   formula = ~ SeaState
 )

ds_hr_sight <- ds(
   data = obs_f,
   truncation = truncdist,
   key = "hr",
   formula = ~ Sightability
 )

ds_hn_sight <- ds(
   data = obs_f,
   truncation = truncdist,
   key = "hn",
   formula = ~ Sightability
 )

# Analysing the detection functions

Distance::summarize_ds_models(ds_hr, ds_hn, ds_hr_seastate, ds_hn_seastate, ds_hr_sight, ds_hn_sight)
