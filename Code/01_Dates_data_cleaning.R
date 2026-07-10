---
title: "01_Dates_data_cleaning"
author: "Gwen Goodenbour"
date: "2026-06-10- 2026-07-10"
---
# This R script cleans and merges dates for cgm data and subject dates
  
    
library(haven)
library(readxl)
library(here)
library(dplyr)
library(tidyr)

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
#------------------------------------------------------------------------------



    #PICK UP NEXT: Pivot each sheet long and then bind



#pivot dates to long format - one row per participant per condition
# pivot dates to long format including baseline and all 4 conditions
dates_long <- dates %>%
  pivot_longer(
    cols = c(baseline, cond_1, cond_2, cond_3, cond_4),
    names_to = "cond_num",
    values_to = "condition"
  ) %>%
  # match each condition to its correct start and end dates
  mutate(
    cond_start = case_when(
      cond_num == "baseline" ~ start_date,
      cond_num == "cond_1"   ~ cond1_start,
      cond_num == "cond_2"   ~ cond2_start,
      cond_num == "cond_3"   ~ cond3_start,
      cond_num == "cond_4"   ~ cond4_start,
      cond_num == "rep_cond" ~ rep_start
    ),
    cond_end = case_when(
      cond_num == "baseline" ~ end_date,
      cond_num == "cond_1"   ~ cond1_end,
      cond_num == "cond_2"   ~ cond2_end,
      cond_num == "cond_3"   ~ cond3_end,
      cond_num == "cond_4"   ~ cond4_end,
      cond_num == "rep_cond" ~ rep_end
    )
  ) %>%
  # keep only relevant columns
  select(id, chamber_y_n,condition, cond_num, cond_start, cond_end, cond_num, notes)

#write clean dataset to new file
#library(writexl)
#write.csv(dates,"Subject_dates_clean", row.names = FALSE)

##########################################################
#-----------FIXING MISSING DATES IN CGM2------------------

#################################################################################

# READ IN CGM DATA
cgm<-read_sas(here("DataProcessed", "analysis_v20260609_REVISED"))


# LEFT JOIN
# WHEN JOINING, NEED TO HAVE COMMON VARIABLES
cgm2 <- cgm %>%
  left_join(
    dates %>% select(id, start_date, end_date, baseline),
    by = join_by(id, between(date, start_date, end_date))
  )
# add dash to cgm2 id to match other files format
cgm2 <- cgm2 %>%
  mutate(id = sub("(TAN)(\\d+)", "\\1-\\2", id))
#View(cgm2)

#write to new file++++
#write.csv(cgm2, "cgm_dates_clean", row.names = FALSE)

# check TAN-001 cgm dates vs condition date ranges
cgm2 %>% 
  filter(id == "TAN-004") %>% 
  select(id, date) %>% 
  print(n = Inf)

dates_long %>% 
  filter(id == "TAN-004")

# remove old start_date and end_date from previous join
cgm2 <- cgm2 %>%
  select(-start_date, -end_date, -baseline)

# retry the join
cgm3 <- cgm2 %>%
  left_join(
    dates_long,
    by = join_by(id, between(date, cond_start, cond_end))
  )

# verify
head(cgm3)



#---- Check for missing condition dates between populated ones ----

n_distinct(cgm3$condition, na.rm = TRUE) # shoudl be 5 :)

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
# this approach is both too specifci and too nonspecific for what I want: 
# different approach below --------------------------------------------


# manually assigning the ids that I know have suspicious missing dates
# will remove all NAs except for these ids:
keep_na_ids <- c("TAN-020", "TAN-022", "TAN-027")

# Testing with a flag before deleting
cgm3 <- cgm3 %>%
  mutate(
    remove = !(id %in% keep_na_ids) &
      (is.na(cond_start) | is.na(cond_end)))

# ^ looks good lets remove those unnecessary NAs:
cgm3_clean <- cgm3 %>%
  filter(!remove) 

#manually remove a few leftover from keep_na_ids
# remove specific rows by row number
cgm3_clean <- cgm3_clean %>%
  slice(-c(262, 263,268, 269, 274,275, 280, 281, 292:294, 302:304, 307:309))

#remove unneeded column
cgm3_clean <- cgm3_clean[, !names(cgm3_clean) %in% c("remove")]



#write clean dataset to new file
#write.csv(cgm3_clean,"cgm3_dated", row.names = FALSE)

# ------------------------------------------
