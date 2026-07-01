---
  title: "03_MergeAttepmt"
author: "Gwen Goodenbour"
date: "2026-06-11"
output: 
  html_document:
  toc: 
  
  ---
  
  
  #paused... not quite here yet...
library(tidyverse)
library(readxl)
library(lubridate)

library(here)
demog <- read_excel(here("DataProcessed","23-1388 Demographics_cleaned.xlsx"))
#not cleaned yet-----
ex_long <- read_excel(here("DataProcessed","Exercise_data_long.xlsx")) %>% 
  janitor :: clean_names() #check this!
#-------
sub_dates <- read_excel("23-1388 Subject Dates_cleaned")

cgm2 <- read_excel(here("DataProcessed","clean_23-1388 Subject Dates.xlsx"))

#--- Preparing/ standardizing files ----
head(cgm2)
 
# standardize ID column to match other files
cgm2 <- cgm2 %>%
  rename(ID = id)

# check date class
class(cgm2$date)

# convert date from datetime to date to match exercise_long
cgm2 <- cgm2 %>%
  mutate(date = as.Date(date))
ex_long <- ex_long %>% 
  mutate(date = as.Date(date))

# verify
class(cgm2$date)
class(ex_long$date)
#-----------------------------

#---- JOINING EXERCISE DATA WITH CGM DATA BY ID AND DATE-----

merged <- ex_long %>% 
  left_join(cgm2, by = c("ID","date"))

# check
dim(merged)
names(merged)


# next--- or before--- check dates
# find exercise rows that didn't match any cgm data
# check how many unique participants and conditions in exercise_long
ex_long %>%
  count(ID, condition) %>%
  print(n = Inf)

# check if exercise_long still has 16 participants
n_distinct(ex_long$ID)

names(demog)
