---
title: "01.2_Dates_pivots"
author: "Gwen Goodenbour"
date: "2026-07-13"
---
# This R script pivots cleaned dates from wide to long format and binds them 
  # into one dates datset
library(haven)
library(readxl)
library(here)
library(dplyr)
library(tidyr)
library(lubridate)

  
cond_dates <- read.csv(here("DataProcessed", "pre_pivot_cond_dates"))
ex_dates <- read.csv(here("DataProcessed", "pre_pivot_ex_dates"))
diet_dates <- read.csv(here("DataProcessed", "pre_pivot_diet_dates"))

#------------------------------------------------------------------------------
names(cond_dates)
names(ex_dates)
names(diet_dates)


#############################################################################
# pivoting condition dates from wide to long
cond_long <- cond_dates %>%
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


# fully long exercise dates - one row per id per condition per exercise date
ex_long <- ex_dates %>%
  pivot_longer(
    cols = c(cond_1, cond_2, cond_3, cond_4),
    names_to = "cond_num",
    values_to = "condition"
  ) %>%
  mutate(
    ex_date1 = case_when(
      cond_num == "cond_1" ~ ex1_1, cond_num == "cond_2" ~ ex2_1,
      cond_num == "cond_3" ~ ex3_1, cond_num == "cond_4" ~ ex4_1),
    ex_date2 = case_when(
      cond_num == "cond_1" ~ ex1_2, cond_num == "cond_2" ~ ex2_2,
      cond_num == "cond_3" ~ ex3_2, cond_num == "cond_4" ~ ex4_2),
    ex_date3 = case_when(
      cond_num == "cond_1" ~ ex1_3, cond_num == "cond_2" ~ ex2_3,
      cond_num == "cond_3" ~ ex3_3, cond_num == "cond_4" ~ ex4_3),
    ex_date4 = case_when(
      cond_num == "cond_1" ~ ex1_4, cond_num == "cond_2" ~ ex2_4,
      cond_num == "cond_3" ~ ex3_4, cond_num == "cond_4" ~ ex4_4)
  ) %>%
  select(id, condition, cond_num, ex_date1:ex_date4) %>%
  
  # pivot again so each exercise date is its own row
  pivot_longer(
    cols = ex_date1:ex_date4,
    names_to = "ex_session",
    values_to = "date"
  ) %>%
  filter(!is.na(date)) %>%
  mutate(exercise_day = 1)  # flag for joining

# same for diet dates
diet_long <- diet_dates %>%
  pivot_longer(
    cols = c(cond_1, cond_2, cond_3, cond_4),
    names_to = "cond_num",
    values_to = "condition"
  ) %>%
  
  # match each condition to its diet dates
  mutate(
    diet_date1 = case_when(
      cond_num == "cond_1" ~ diet1_1, cond_num == "cond_2" ~ diet2_1,
      cond_num == "cond_3" ~ diet3_1, cond_num == "cond_4" ~ diet4_1),
    diet_date2 = case_when(
      cond_num == "cond_1" ~ diet1_2, cond_num == "cond_2" ~ diet2_2,
      cond_num == "cond_3" ~ diet3_2, cond_num == "cond_4" ~ diet4_2),
    diet_date3 = case_when(
      cond_num == "cond_1" ~ diet1_3, cond_num == "cond_2" ~ diet2_3,
      cond_num == "cond_3" ~ diet3_3, cond_num == "cond_4" ~ diet4_3)
  ) %>%
  select(id, condition, cond_num, diet_date1, diet_date2, diet_date3) %>%
  
  # pivot diet dates long so each diet day is its own row
  pivot_longer(
    cols = diet_date1:diet_date3,
    names_to = "diet_day_num",
    values_to = "date"
  ) %>%
  filter(!is.na(date)) %>%
  mutate(diet_day_flag = 1)

# convert date handling both excel serials and standard dates
diet_long <- diet_long %>%
  mutate(date = case_when(
    grepl("^\\d{5}$", date) ~ as.Date(as.numeric(date), origin = "1899-12-30"),
    TRUE ~ as.Date(date)
  ))


# where are the 10 missing diet days concentrated?
diet_long %>%
  filter(is.na(date)) %>%
  count(id, condition) %>%
  as.data.frame()
# seems scattered/ incidental

# verify
class(diet_long$date)
summary(diet_long$date)

# fixing excel date issue/ inconsistent format
ex_long <- ex_long %>%
  mutate(date = case_when(
    # if it looks like an excel serial number, convert with origin
    !is.na(as.numeric(date)) ~ as.Date(as.numeric(date), origin = "1899-12-30"),
    # otherwise-  standard date
    TRUE ~ as.Date(date, format = "%Y-%m-%d")
  ))

#write clean dataset to new file
#library(writexl)
#write.csv(cond_long,"cond_dates_long", row.names = FALSE)
#write.csv(ex_long,"ex_dates_long", row.names = FALSE)
#write.csv(diet_long,"diet_dates_long", row.names = FALSE)
