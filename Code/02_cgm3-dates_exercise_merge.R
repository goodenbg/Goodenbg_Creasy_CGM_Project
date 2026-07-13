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
cgm <- read.csv(here("DataProcessed", "cgm_dated"))

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
  
  summarise(
    n_exercise_days  = n(),
    mean_hr          = mean(hr, na.rm = TRUE),
    mean_rpe         = mean(rpe, na.rm = TRUE),
    mean_tread_grade = mean(tread_grade, na.rm = TRUE),
    mean_tread_speed = mean(tread_speed, na.rm = TRUE),
    mean_time        = mean(time, na.rm = TRUE),
    
    # food_date and wake are datetime/character - take first non-NA 
    n_wake_recorded  = sum(!is.na(wake)),
    n_food_recorded  = sum(!is.na(food_date)),
    .groups = "drop"
  )


# join summarized exercise data to cgm3
merged <- cgm %>%
  left_join(ex_summary, by = c("id", "condition"))

# verify
dim(merged)
names(merged)

# notes column is feeling cluttered now - making new dataset and removing from merge

# save column as its own table
notes <- merged %>%
  select(id, notes)

# remove it from merged
merged <- merged %>%
  select(-notes)


# making sure there is only missing exercise data for BL condition
merged %>%
  filter(condition != "BL") %>%
  summarise(
    total_rows = n(),
    missing_exercise = sum(is.na(mean_hr)),
    pct_missing = round(missing_exercise/total_rows * 100, 1)
  )  

# check which columns are dates stored as character
merged %>%
  select(where(is.character)) %>%
  names()

# convert date columns from character to Date
merged <- merged %>%
  mutate(across(c(date, cond_start, cond_end), as.Date))

#write.csv(merged,"clean_merge_triple_dataset", row.names = FALSE)
