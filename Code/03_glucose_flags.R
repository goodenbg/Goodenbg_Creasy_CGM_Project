---
title: "03_glucose_flags"
author: "Gwen Goodenbour"
date: "2026-07-06"
---
  
# for cgm flags- artifacts from pressure on wear: 
# need to check the minute level data to see if it persisted-
# flaged the days but need to check the raw associated day

library(here)
library(dplyr)
library(lubridate)
library(hms)

cgm <- read.csv(here("DataProcessed", "cgm_dated"))
  
# using range of 70-140 to account for range of time between meals
summary(cgm$avg_glucose)
summary(cgm$night_glucose)

glucose_flag <- cgm %>% 
  mutate(
    avg_g_flag = avg_glucose < 70 | avg_glucose > 140,
    night_flag = night_glucose < 70 | night_glucose > 140
  ) %>%
  filter(avg_g_flag, night_flag) %>%
  select(everything(), avg_glucose, night_glucose,avg_g_flag, night_flag)

# pretty much only TAN-018
# check what the first few rows look like, skipping first two rows
tan18 <- read.csv(here("DataRaw/Raw Glucose Data/TAN018_glucose_8-11-2025.csv"), skip = 2)
names(tan18)

#rename columns 
tan18 <- tan18 %>% 
  rename(glucose = `Historic.Glucose.mg.dL`) %>% 
  select(Device,Serial.Number, Device.Timestamp, glucose) 

#perform same glucose flag but on raw data:
tan18_flag <- tan18 %>% 
  mutate(
    g_flag = glucose < 70 | glucose > 140) %>% 
  filter(g_flag) %>% 
  select(everything(), glucose, g_flag)

# percentage of flags for tan-018
tan18_flag %>% count(g_flag)
nrow(tan18)

flagged_q <- tan18_flag %>%
  count(g_flag) %>%
  mutate(pct = round(n / nrow(tan18) * 100, 1)) #~34%

