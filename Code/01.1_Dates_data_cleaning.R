---
title: "01.1_Dates_data_cleaning"
author: "Gwen Goodenbour"
date: "2026-06-10- 2026-07-10"
---

# This R script cleans all three sheets of the Subject dates file in prep for 
  # pivot long and merge with cgm
  
library(haven)
library(readxl)
library(here)
library(dplyr)
library(tidyr)
library(lubridate)

################################################################################
# ----- CLEANING SUBJECT DATES FILE ----- 
##############################################################################
# multiple sheets
# read in all three sheets
cond_dates <- read_excel(here("DataRaw", "23-1388 Subject Dates REVISED.xlsx"), sheet = 1)
ex_dates <- read_excel(here("DataRaw", "23-1388 Subject Dates REVISED.xlsx"), sheet = 2)
diet_dates <- read_excel(here("DataRaw", "23-1388 Subject Dates REVISED.xlsx"), sheet = 3)

# ----- 1. clean condition dates first: ----------------------------------------

#CLEAN ID
cond_dates$id <- gsub("-", "", cond_dates$`Study ID`)

#CLEAN DATE RANGE
cond_dates <- cond_dates %>%
  separate(`Dates...4`, into = c("start_date", "end_date"), sep = "-") %>%
  mutate(
    start_date = as.Date(start_date, format = "%m/%d/%y"),
    end_date   = as.Date(end_date, format = "%m/%d/%y")
  )

#---- Further Subject Dates cleaning
#names(dates)

cond_dates <- cond_dates %>% # split condition date range into start and end dates
  separate(`Dates...6`, into = c("cond1_start", "cond1_end"), sep = "-") %>%
  separate(`Dates...8`, into = c("cond2_start", "cond2_end"), sep = "-") %>%
  separate(`Dates...10`, into = c("cond3_start", "cond3_end"), sep = "-") %>%
  separate(`Dates...12`, into = c("cond4_start", "cond4_end"), sep = "-") %>%
  separate(`Dates...14`, into = c("rep_start", "rep_end"), sep = "-")  %>% 
  # convert all new date columns to Date format
  mutate(across(ends_with("_start") | ends_with("_end"), 
                ~ as.Date(.x, format = "%m/%d/%y")))
#head(dates)

#dates %>% select(`Study ID`, id) %>% head() # two id columns but structured differently
# remove duplicate id column and rename Study ID to id
cond_dates <- cond_dates %>%
  select(-id) %>%
  rename(id = `Study ID`)

# standardize naming convention further
#ames(dates)
cond_dates <- cond_dates %>% 
  rename(chamber_y_n = `Chamber (Y/N)`) %>% 
  rename(baseline = `Baseline`) %>% 
  rename(cond_1 = `Condition 1`) %>% 
  rename(cond_2 = `Condition 2`) %>% 
  rename(cond_3 = `Condition 3`) %>% 
  rename(cond_4 = `Condition 4`) %>% 
  rename(rep_cond = `Repeated Condition`) %>% 
  rename(notes = `NOTES`)



# ----- 2. clean exercise dates ----------------------------------------------

#CLEAN ID
ex_dates$id <- gsub("-", "", ex_dates$`Study ID`)

#CLEAN DATE RANGE
ex_dates <- ex_dates %>%
  # separate date ranges for each condition
  separate(`Dates...4`,  into = c("cond1_start", "cond1_end"),   sep = "-") %>%
  separate(`Dates...10`, into = c("cond2_start", "cond2_end"), sep = "-") %>%
  separate(`Dates...16`, into = c("cond3_start", "cond3_end"), sep = "-") %>%
  separate(`Dates...22`, into = c("cond4_start", "cond4_end"), sep = "-") %>%
  separate(`Dates...28`, into = c("rep_start",   "rep_end"),   sep = "-") %>%
  # convert to Date format
  mutate(across(ends_with("_start") | ends_with("_end"),
                ~ as.Date(.x, format = "%m/%d/%y")))

#dates %>% select(`Study ID`, id) %>% head() # two id columns but structured differently
# remove duplicate id column and rename Study ID to id
ex_dates <- ex_dates %>%
  select(-id) %>%
  rename(id = `Study ID`)

names(ex_dates)
ex_dates <- ex_dates %>% 
  rename(chamber_y_n = `Chamber (Y/N)`) %>% 
  rename(cond_1 = `Condition 1`) %>% 
  rename(cond_2 = `Condition 2`) %>% 
  rename(cond_3 = `Condition 3`) %>% 
  rename(cond_4 = `Condition 4`) %>% 
  rename(rep_cond = `Repeated Condition`) %>% 
  rename(notes = `NOTES`)


# ----- 3. clean diet dates ----------------------------------------------
#CLEAN ID
diet_dates$id <- gsub("-", "", diet_dates$`Study ID`)

#CLEAN DATE RANGE
diet_dates <- diet_dates %>%
  # separate date ranges for each condition
  separate(`Dates...4`,  into = c("cond1_start", "cond1_end"),   sep = "-") %>%
  separate(`Dates...9`, into = c("cond2_start", "cond2_end"), sep = "-") %>%
  separate(`Dates...14`, into = c("cond3_start", "cond3_end"), sep = "-") %>%
  separate(`Dates...19`, into = c("cond4_start", "cond4_end"), sep = "-") %>%
  separate(`Dates...24`, into = c("rep_start",   "rep_end"),   sep = "-") %>%
  # convert to Date format
  mutate(across(ends_with("_start") | ends_with("_end"),
                ~ as.Date(.x, format = "%m/%d/%y")))

#dates %>% select(`Study ID`, id) %>% head() # two id columns but structured differently
# remove duplicate id column and rename Study ID to id
diet_dates <- diet_dates %>%
  select(-id) %>%
  rename(id = `Study ID`)

names(diet_dates)
diet_dates <- diet_dates %>% 
  rename(chamber_y_n = `Chamber (Y/N)`) %>% 
  rename(cond_1 = `Condition 1`) %>% 
  rename(cond_2 = `Condition 2`) %>% 
  rename(cond_3 = `Condition 3`) %>% 
  rename(cond_4 = `Condition 4`) %>% 
  rename(rep_cond = `Repeated Condition`) %>% 
  rename(notes = `NOTES`)


# ---- REPLACE REPEATED CONDITIONS -----
# function to replace repeated condition dates
rep_repeated_cond <- function(df) {
  df %>%
    mutate(
      # replace cond1 dates if cond_1 matches the repeated condition
      cond1_start = if_else(!is.na(rep_cond) & cond_1 == rep_cond, rep_start, cond1_start),
      cond1_end   = if_else(!is.na(rep_cond) & cond_1 == rep_cond, rep_end,   cond1_end),
      # replace cond2 dates if cond_2 matches the repeated condition
      cond2_start = if_else(!is.na(rep_cond) & cond_2 == rep_cond, rep_start, cond2_start),
      cond2_end   = if_else(!is.na(rep_cond) & cond_2 == rep_cond, rep_end,   cond2_end),
      # replace cond3 dates if cond_3 matches the repeated condition
      cond3_start = if_else(!is.na(rep_cond) & cond_3 == rep_cond, rep_start, cond3_start),
      cond3_end   = if_else(!is.na(rep_cond) & cond_3 == rep_cond, rep_end,   cond3_end),
      # replace cond4 dates if cond_4 matches the repeated condition
      cond4_start = if_else(!is.na(rep_cond) & cond_4 == rep_cond, rep_start, cond4_start),
      cond4_end   = if_else(!is.na(rep_cond) & cond_4 == rep_cond, rep_end,   cond4_end)
    )
}

# apply to each sheet
cond_dates  <- rep_repeated_cond(cond_dates)
ex_dates    <- rep_repeated_cond(ex_dates)
diet_dates  <- rep_repeated_cond(diet_dates)
##############################################################################

# Making naming convention just a little more intuitive
# (tried clean_names with janitor but was still more confusing than desired)
names(ex_dates)
ex_dates <- ex_dates %>% 
  rename(ex1_1 = `Exercise 1...5`) %>% 
  rename(ex1_2 = `Exercise 2...6`) %>%  
  rename(ex1_3 = `Exercise 3...7`) %>% 
  rename(ex1_4 = `Exercise 4...8`) %>%
  
  rename(ex2_1 = `Exercise 1...11`) %>% 
  rename(ex2_2 = `Exercise 2...12`) %>%  
  rename(ex2_3 = `Exercise 3...13`) %>% 
  rename(ex2_4 = `Exercise 4...14`) %>% 
  
  rename(ex3_1 = `Exercise 1...17`) %>% 
  rename(ex3_2 = `Exercise 2...18`) %>%  
  rename(ex3_3 = `Exercise 3...19`) %>% 
  rename(ex3_4 = `Exercise 4...20`) %>%
  
  rename(ex4_1 = `Exercise 1...23`) %>% 
  rename(ex4_2 = `Exercise 2...24`) %>%  
  rename(ex4_3 = `Exercise 3...25`) %>% 
  rename(ex4_4 = `Exercise 4...26`) %>% 

  rename(exrep_1 = `Exercise 1...29`) %>% 
  rename(exrep_2 = `Exercise 2...30`) %>%  
  rename(exrep_3 = `Exercise 3...31`) %>% 
  rename(exrep_4 = `Exercise 4...32`) 
  
names(diet_dates)
diet_dates <- diet_dates %>% 
  rename(diet1_1 = `Diet Day 1...5`) %>% 
  rename(diet1_2 = `Diet Day 2...6`) %>%  
  rename(diet1_3 = `Diet Day 3...7`) %>% 
  
  rename(diet2_1 = `Diet Day 1...10`) %>% 
  rename(diet2_2 = `Diet Day 2...11`) %>%  
  rename(diet2_3 = `Diet Day 3...12`) %>% 
  
  rename(diet3_1 = `Diet Day 1...15`) %>% 
  rename(diet3_2 = `Diet Day 2...16`) %>%  
  rename(diet3_3 = `Diet Day 3...17`) %>% 
  
  rename(diet4_1 = `Diet Day 1...20`) %>% 
  rename(diet4_2 = `Diet Day 2...21`) %>%  
  rename(diet4_3 = `Diet Day 3...22`) %>%
  
  rename(dietrep4_1 = `Diet Day 1...25`) %>% 
  rename(dietrep4_2 = `Diet Day 2...26`) %>%  
  rename(dietrep4_3 = `Diet Day 3...27`) 

# ----- Replace repeated diet and exercise dates -----
# check the class of each ex1 column
ex_dates %>% select(matches("^ex1")) %>% sapply(class)

# fixing Excel dates issue
ex_dates <- ex_dates %>%
  mutate(
    ex1_1 = as.Date(ex1_1),
    ex1_2 = as.Date(as.numeric(ex1_2), origin = "1899-12-30"),
    ex1_3 = as.Date(as.numeric(ex1_3), origin = "1899-12-30"),
    ex1_4 = as.Date(as.numeric(ex1_4), origin = "1899-12-30")
  )


# replacing repeated exercise dates
ex_dates <- ex_dates %>%
  mutate(
    ex1_1 = if_else(id == "TAN-002", as.Date(NA), ex1_1),
    ex1_2 = if_else(id == "TAN-002", as.Date("2025-01-13"), ex1_2),
    ex1_3 = if_else(id == "TAN-002", as.Date("2025-01-14"), ex1_3),
    ex1_4 = if_else(id == "TAN-002", as.Date("2025-01-15"), ex1_4)
  )

# verify
ex_dates %>% 
  filter(id == "TAN-002") %>% 
  select(id, ex1_1, ex1_2, ex1_3, ex1_4) 

ex_dates <- ex_dates %>%
  mutate(
    ex1_1 = if_else(id == "TAN-012", as.Date("2025-03-10"), ex1_1),
    ex1_2 = if_else(id == "TAN-012", as.Date("2025-03-11"), ex1_2),
    ex1_3 = if_else(id == "TAN-012", as.Date("2025-03-12"), ex1_3),
    ex1_4 = if_else(id == "TAN-012", as.Date("2025-03-13"), ex1_4)
  )

# verify
ex_dates %>% 
  filter(id == "TAN-024") %>% 
  select(id, ex1_1, ex1_2, ex1_3, ex1_4)

ex_dates <- ex_dates %>%
  mutate(
    ex1_1 = if_else(id == "TAN-024", as.Date("2025-09-08"), ex1_1),
    ex1_2 = if_else(id == "TAN-024", as.Date("2025-09-09"), ex1_2),
    ex1_3 = if_else(id == "TAN-024", as.Date("2025-09-10"), ex1_3),
    ex1_4 = if_else(id == "TAN-024", as.Date("2025-09-11"), ex1_4)
  )

# verify
ex_dates %>% 
  filter(id == "TAN-024") %>% 
  select(id, ex1_1, ex1_2, ex1_3, ex1_4)

# replacing repeated diet dates -----

# check the class of each diet1 column
diet_dates %>% select(matches("^diet1")) %>% sapply(class)

# fixing Excel date issue
diet_dates <- diet_dates %>%
  mutate(
    diet1_1 = as.Date(diet1_1),
    diet1_2 = as.Date(diet1_2),
    diet1_3 = as.Date(as.numeric(diet1_3), origin = "1899-12-30")
  )

# replacing repeated cond diet dates

diet_dates <- diet_dates %>%
  mutate(
    diet1_1 = if_else(id == "TAN-002", as.Date("2025-01-14"), diet1_1),
    diet1_2 = if_else(id == "TAN-002", as.Date("2025-01-15"), diet1_2),
    diet1_3 = if_else(id == "TAN-002", as.Date("2025-01-16"), diet1_3)
  )

# verify
diet_dates %>% 
  filter(id == "TAN-002") %>% 
  select(id, diet1_1, diet1_2, diet1_3)

diet_dates <- diet_dates %>%
  mutate(
    diet1_1 = if_else(id == "TAN-012", as.Date("2025-04-15"), diet1_1),
    diet1_2 = if_else(id == "TAN-012", as.Date("2025-04-16"), diet1_2),
    diet1_3 = if_else(id == "TAN-012", as.Date("2025-04-17"), diet1_3)
  )

# verify
diet_dates %>% 
  filter(id == "TAN-012") %>% 
  select(id, diet1_1, diet1_2, diet1_3)

diet_dates <- diet_dates %>%
  mutate(
    diet1_1 = if_else(id == "TAN-024", as.Date("2025-09-10"), diet1_1),
    diet1_2 = if_else(id == "TAN-024", as.Date("2025-09-11"), diet1_2),
    diet1_3 = if_else(id == "TAN-024", as.Date("2025-09-12"), diet1_3)
  )

# verify
diet_dates %>% 
  filter(id == "TAN-024") %>% 
  select(id, diet1_1, diet1_2, diet1_3)

#write clean datasets to new file -> read into pivot file
# Write as csv's - will need
#write.csv(cond_dates,"pre_pivot_cond_dates", row.names = FALSE)
#write.csv(ex_dates,"pre_pivot_ex_dates", row.names = FALSE)
#write.csv(diet_dates,"pre_pivot_diet_dates", row.names = FALSE)
