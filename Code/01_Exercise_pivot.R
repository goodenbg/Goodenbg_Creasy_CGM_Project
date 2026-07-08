---
title: "02_exercise"
author: "Gwen Goodenbour"
date: "2026-06-18"
---
# This file pivots exercise data from wide to long format 
  
# PIVOT EXERCISE WIDE-> LONG
library(readxl)
library(dplyr)

# read in file
ex_wide <- read_excel(here("DataRaw", "23-1388 Exercise Data.xlsx"))
# NOTE -exercise data has updated any repeated conditions to correct condition 
# dates already :)

#pivot to long data format
ex_long <- ex_wide %>%
  rename(ID = record_ida) %>%
  mutate(condition = recode(redcap_event_name,
                            "fastedam_arm_1" = "Fasted-AM",
                            "fedam_arm_1"    = "Fed-AM",
                            "fastedpm_arm_1" = "Fasted-PM",
                            "fedpm_arm_1"    = "Fed-PM"
  )) %>%
  select(-redcap_event_name) %>%
  pivot_longer(
    cols = -c(ID, condition),
    names_to = c(".value", "day_of_week"),
    names_pattern = "(.+)_(sat|sun|mon|tues|wed|thurs|fri)"
  ) %>%
  filter(!is.na(date)) %>%
  mutate(date = as.Date(date))

#check
names(ex_long)
head(ex_long)
nrow(ex_long)

# standardizing naming convention and time
ex_long <- ex_long %>% 
  rename(id = `ID`) 

ex_long <- ex_long %>%
  mutate(wake = format(wake, "%H:%M"))

ex_long <- ex_long %>%
  mutate(start = format(start, "%H:%M"))

#checking distinct IDs
# How many unique participants?
n_distinct(ex_long$id) 

#check for duplicates
sum(duplicated(ex_long)) # = 0

class(ex_long$food_date)
# standardize food_date to consistent datetime format
ex_long <- ex_long %>%
  mutate(food_date = format(food_date, "%Y-%m-%d %H:%M:%S"))

#write clean dataset to new file
write.csv(ex_long,"exercise_long", row.names = FALSE)


# exclude this step to maintain dropped/missing participants until modeling stage
#counting how many conditions each participant completed to filter out those with less than 4
#ex_long %>% 
  #count(id, condition) %>% 
  #count(id, name = "n_conditions") %>% 
  #arrange(n_conditions) %>% 
  #print(n = Inf)

#filtering out IDs with less than four completed conditions
#ex_long <- ex_long %>% 
  #group_by(ID) %>% 
  #filter(n_distinct(condition) == 4) %>% 
  #ungroup()

# verify we now have  16 participants
#n_distinct(ex_long$ID)


