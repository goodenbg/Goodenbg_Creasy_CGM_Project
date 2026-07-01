---
title: "02_exercise"
author: "Gwen Goodenbour"
date: "2026-06-18"
---
  
# PIVOT EXERCISE WIDE-> LONG

ex_wide <- read_excel(here("DataRaw", "23-1388 Exercise Data.xlsx"))

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

#checking distinct IDs
# How many unique participants?
n_distinct(ex_long$ID) #too many IDs (should be 16) 

#counting how many conditions each participant completed to filte rout those with less than 4
ex_long %>% 
  count(ID, condition) %>% 
  count(ID, name = "n_conditions") %>% 
  arrange(n_conditions) %>% 
  print(n = Inf)

#filtering out IDs with less than four completed conditions
ex_long <- ex_long %>% 
  group_by(ID) %>% 
  filter(n_distinct(condition) == 4) %>% 
  ungroup()

# verify we now have  16 participants
n_distinct(ex_long$ID)

#considering repeated conditions
dates <- read_excel(here("DataRaw","23-1388 Subject Dates.xlsx")) %>% 
  rename(ID = `Study ID`)
repeats <- dates %>% 
  filter(!is.na(`Repeated Condition`)) %>% 
  pull(ID) #pull IDs of participants that had repeated conditions

# see the repeated condition label for each repeater
dates %>%
  filter(ID %in% repeats) %>%
  select(ID, `Repeated Condition`)

# confirm exactly 16 unique participants
n_distinct(ex_long$ID)

# confirm how many days of data each participant has per condition
ex_long %>%
  count(ID, condition) %>%
  print(n = Inf)

# look at the 2 repeaters' conditions in exercise_long
# alongside what their repeated condition was supposed to be
ex_long %>%
  filter(ID %in% repeats) %>%
  distinct(ID, condition) %>%
  left_join(
    dates %>% select(ID, `Repeated Condition`),
    by = "ID"
  )
write_xlsx(ex_long, path = here("DataProcessed","Exercise_data_long.xlsx"))
# TODO: verify repeaters (TAN-002, TAN-012) don't have duplicate condition 
# data once all raw files are merged - check for duplicate ID + condition + date rows
