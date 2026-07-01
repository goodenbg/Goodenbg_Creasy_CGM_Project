---
title: "01_Dates_data_cleaning"
author: "Gwen Goodenbour"
date: "2026-06-10- 2026-06-26"
---
  
library(haven)
library(readxl)
library(here)
library(dplyr)
library(tidyr)

################################################################################
# ----- CLEANING SUBJECT DATES FILE -----

dates<-read_excel(here("DataRaw", "23-1388 Subject Dates REVISED.xlsx"))

#CLEAN ID
dates$id <- gsub("-", "", dates$`Study ID`)

#CLEAN DATE RANGE
dates <- dates %>%
  separate(`Dates...4`, into = c("start_date", "end_date"), sep = "-") %>%
  mutate(
    start_date = as.Date(start_date, format = "%m/%d/%y"),
    end_date   = as.Date(end_date, format = "%m/%d/%y")
  )

#---- Further Subject Dates cleaning
#names(dates)

dates <- dates %>% # split condition date range into start and end dates
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
dates <- dates %>%
  select(-id) %>%
  rename(id = `Study ID`)

########################################################################
# ---- Determine whether or not to drop when modelling ------
# remove dropout participants manually - replace with actual dropout IDs
#drop <- c("TAN-009", "TAN-024", 'TAN-025',"TAN-027", "TAN-029")

# filter out dropouts
#dates <- dates %>%
 # filter(!id %in% drop)
# --------------------------------------------------------------
########################################################################

# standardize naming convention further
#ames(dates)
dates <- dates %>% 
  rename(chamber_y_n = `Chamber (Y/N)`) %>% 
  rename(baseline = `Baseline`) %>% 
  rename(cond_1 = `Condition 1`) %>% 
  rename(cond_2 = `Condition 2`) %>% 
  rename(cond_3 = `Condition 3`) %>% 
  rename(cond_4 = `Condition 4`) %>% 
  rename(rep_cond = `Repeated Condition`) %>% 
  rename(notes = `NOTES`)
# ---- REPLACING REPEATED CONDITIONS ----------------------------------

# replace original condition dates with repeat dates for repeaters
dates <- dates %>%
  mutate(
    # replace cond1 dates if cond_1 matches the repeated condition
    cond1_start = if_else(!is.na(`rep_cond`) & cond_1 == `rep_cond`, 
                          rep_start, cond1_start),
    cond1_end   = if_else(!is.na(`rep_cond`) & cond_1 == `rep_cond`, 
                          rep_end, cond1_end),
    # replace cond2 dates if cond_2 matches the repeated condition
    cond2_start = if_else(!is.na(`rep_cond`) & cond_2 == `rep_cond`, 
                          rep_start, cond2_start),
    cond2_end   = if_else(!is.na(`rep_cond`) & cond_2 == `rep_cond`, 
                          rep_end, cond2_end),
    # replace cond3 dates if cond_3 matches the repeated condition
    cond3_start = if_else(!is.na(`rep_cond`) & cond_3 == `rep_cond`, 
                          rep_start, cond3_start),
    cond3_end   = if_else(!is.na(`rep_cond`) & cond_3 == `rep_cond`, 
                          rep_end, cond3_end),
    # replace cond4 dates if cond_4 matches the repeated condition
    cond4_start = if_else(!is.na(`rep_cond`) & cond_4 == `rep_cond`, 
                          rep_start, cond4_start),
    cond4_end   = if_else(!is.na(`rep_cond`) & cond_4 == `rep_cond`, 
                          rep_end, cond4_end)
  )

# verify the replacement worked for the 3 repeaters
dates %>%
  filter(!is.na(`rep_cond`)) %>%
  select(id, `rep_cond`, rep_start, rep_end,
         cond_1, cond1_start, cond1_end,
         cond_2, cond2_start, cond2_end,
         cond_3, cond3_start, cond3_end,
         cond_4, cond4_start, cond4_end) #YESSS

#---------------------------

#write clean dataset to new file
#library(writexl)
#write.csv(dates,"Subject_dates_clean", row.names = FALSE)

##########################################################
#-----------FIXING MISSING DATES IN CGM2------------------

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

#############################################################################################################

#something is not going right with the merge!


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
keep_na_ids <- c("TAN-002", "TAN-012", "TAN-020", "TAN-022", "TAN-027")

# Testing with a flag before deleting
cgm3 <- cgm3 %>%
  mutate(
    remove = !(id %in% keep_na_ids) &
      (is.na(cond_start) | is.na(cond_end)))

# ^ looks good lets remove those unnecessary NAs:
cgm3_clean <- cgm3 %>%
  filter(!remove) 

#manually remove a few leftover from keep_na_ids
cgm3_clean <- cgm3_clean[-c(37, 56, 57, 161, 171, 177, 182:184,
                            290, 291, 296, 297, 302, 303,
                            308, 309, 320:322), ]

#remove unneeded column
cgm3_clean <- cgm3_clean[, !names(cgm3_clean) %in% c("remove")]

#write clean dataset to new file
#library(writexl)
#write.csv(dates,"cgm3_clean", row.names = FALSE)

# ------------------------------------------



# -look for any outliers for start end 24 hr wear- does everyone have ~24 hours of wear?

cgm3 %>%
  mutate(
    # Create a new column called wear_duration:
    # difftime(end, start) calculates the time difference for each row.
    wear_duration = as.numeric(difftime(end, start, units = "hours"))
  ) %>%
  
  summarise(
    n = n(),
    min = min(wear_duration, na.rm = TRUE),
    max = max(wear_duration, na.rm = TRUE),
    mean = mean(wear_duration, na.rm = TRUE),
    median = median(wear_duration, na.rm = TRUE),
  )

#flag difference of more than 1 hour
cgm_flag <- cgm3 %>%
  mutate(
    wear_duration = as.numeric(difftime(end, start, units = "hours")),
    wear_flag = abs(wear_duration - 24) > 1 
  ) %>%
  filter(wear_flag) %>%
  select(everything(), wear_duration, wear_flag)

#more than 24 hrs?

