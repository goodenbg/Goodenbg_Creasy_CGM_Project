---
title: "01_Cleaning_ CGM_Dates"
author: "Gwen Goodenbour"
date: "2026-07-13"
---
  
# This R script cleans and merges dates for cgm data and subject dates

library(haven)
library(readxl)
library(here)
library(dplyr)
library(tidyr)
library(lubridate)


##########################################################
#-----------FIXING MISSING DATES IN CGM2------------------
##########################################################

# READ IN CGM DATA
cgm <- read_sas(here("DataProcessed", "analysis_v20260609_REVISED"))
# condition dates
cond_long <- read.csv(here("DataProcessed", "cond_dates_long"))

#add dash in cgm id 
cgm <- cgm %>%
  mutate(id = sub("(TAN)(\\d+)", "\\1-\\2", id))

# convert cond_start and cond_end to Date
cond_long <- cond_long %>%
  mutate(
    cond_start = as.Date(cond_start, format = "%Y-%m-%d"),
    cond_end   = as.Date(cond_end, format = "%Y-%m-%d")
  )

cgm2 <- cgm %>%
  left_join(
    cond_long %>% select(id, condition, cond_num, cond_start, cond_end, notes),
    by = join_by(id, between(date, cond_start, cond_end))
  )

#---- Check for missing condition dates between populated ones ----

n_distinct(cgm2$condition, na.rm = TRUE) # shoudl be 5 :)

# flag and separate NAs: leading/trailing (remove) vs floating (keep, verify with PI)

#cgm3 <- cgm3 %>% 
#group_by(id) %>% 
#arrange(id, date) %>% 
#mutate(
# find first and last date with known condition for each participant
#first = min(date[!is.na(condition)], na.rm = TRUE), 
#last = max(date[!is.na(condition)], na.rm = TRUE),

# flag rows to remove if they are outside condition range
#remove_flag = is.na(condition) &
#(date < first | date > last)
#) %>% 
#ungroup()

#check how many would be removed
#cgm3 %>% count(remove_flag)
# this approach is both too specific and too nonspecific for what I want: 
# different approach below --------------------------------------------


# manually assigning the ids that I know have suspicious missing dates
# will remove all NAs except for these ids:
keep_na_ids <- c("TAN-022", "TAN-027")

# Testing with a flag before deleting
cgm2 <- cgm2%>%
  mutate(
    remove = !(id %in% keep_na_ids) &
      (is.na(cond_start) | is.na(cond_end)))

# ^ looks good lets remove those unnecessary NAs:
cgm2_clean <- cgm2 %>%
  filter(!remove) 

#remove unneeded column
cgm2_clean <- cgm2_clean[, !names(cgm2_clean) %in% c("remove")]

# ----- JOIN OTHER TWO DATES TO CGM & Condition Dates -----

ex_long <- read.csv(here("DataProcessed", "ex_dates_long"))

diet_long <- read.csv(here("DataProcessed", "diet_dates_long"))

#convert character type to dates
ex_long <- ex_long %>%
  mutate(date = as.Date(date))

diet_long <- diet_long %>%
  mutate(date = as.Date(date))
# join exercise day flags onto cgm3
cgm_dated <- cgm2_clean %>%
  full_join(
    ex_long %>% select(id, condition, date, exercise_day),
    by = c("id", "condition", "date")
  ) %>%
  full_join(
    diet_long %>% select(id, condition, date, diet_day_flag),
    by = c("id", "condition", "date")
  ) %>%
  mutate(
    exercise_day  = replace_na(exercise_day, 0),
    diet_day_flag = replace_na(diet_day_flag, 0)
  )

#write clean dataset to new file
#write.csv(cgm_dated,"cgm_dated", row.names = FALSE)

# ------------------------------------------
# which participants have unmatched exercise dates?
