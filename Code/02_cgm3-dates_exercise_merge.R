---
title: "02_cgm3-dates_exercise_merge"
author: "Gwen Goodenbour"
date: "2026-07-05"
---
# merging cgm3 (cgm sas day/night level data with subject dates) and exercise

library(dplyr)
library(lubridate)
library(hms)

# cleaned cgm data
cgm <- read.csv(here("DataProcessed", "cgm3_dated"))

# cleaned/long exercise data
ex <- read.csv(here("DataProcessed","exercise_long"))
  
# pre-merge check
class(cgm$id) # character
class(ex$id) # character

class(cgm$date) # character
class(ex$date) # character

unique(cgm$condition)
unique(ex$condition)


# summarize exercise data to one row per participant per condition
ex_summary <- ex %>%
  group_by(id, condition) %>%
  
  # exercise data to one row per condition first
  summarise( 
    n_exercise_days = n(),
    mean_hr = mean(hr, na.rm = TRUE),
    mean_rpe = mean(rpe, na.rm = TRUE),
    mean_tread_grade = mean(tread_grade, na.rm = TRUE),
    mean_tread_speed = mean(tread_speed, na.rm = TRUE),
    .groups = "drop"  #removes default grouping
  )

# join summarized exercise data to cgm3
merged <- cgm %>%
  left_join(ex_summary, by = c("id", "condition"))

# verify
dim(merged)
names(merged)

# making sure there is only missing exercise data for BL condition
merged %>%
  filter(condition != "BL") %>%
  summarise(
    total_rows = n(),
    missing_exercise = sum(is.na(mean_hr)),
    pct_missing = round(missing_exercise/total_rows * 100, 1)
  )  
