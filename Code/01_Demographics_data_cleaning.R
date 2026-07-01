---
title: "01_Demographics_data_cleaning"
author: "Gwen Goodenbour"
date: "2026-06-11"
  ---


library(tidyverse)
library(knitr)
library(kableExtra)
library(table1)
library(ggplot2)
library(readxl)
library(here)

#load in data
dem_data <- read_excel(here("DataRaw","23-1388 Demographics.xlsx"))
# checking variables and structure---
head(dem_data)
names(dem_data)
nrow(dem_data)

dem_data %>% 
  select(`Record ID`) %>% 
  print(n = Inf)
#------

#check for missing data
colSums(is.na(dem_data))

#remove missing IDs

dem_data1 <- dem_data %>% 
  filter(!is.na(`Record ID`))

#verifying no more missing ID 
#dem_data1 %>%
#count(`Record ID`) #--- looks good (removed two rows with missing data)

#standardize naming convention-----
dem_data1 <- dem_data1 %>%
  rename(id = `Record ID`) %>% 
  rename(cond_1 = `Condition 1`) %>% 
  rename(sex = `Please indicate your sex at birth`) %>% 
  rename(race = `Please indicate your race:`) %>% 
  rename(ethnicity = `Ethnicity`)  %>% 
  rename(bmi = `Screening BMI`)
  
#-----
# remove dropped participants
drop <- c("TAN-009", "TAN-024", 'TAN-025',"TAN-027", "TAN-029")

dem_data1 <- dem_data1 %>%
  filter(!`id` %in% drop)

# verify 16 remain
nrow(dem_data1)

# remove unwanted column
dem_data1 <- dem_data1 %>%
  select(-`Race (Other)`)

#write clean dataset to new file
#write.csv(dem_data1, "Demographics_clean", row.names = FALSE)
