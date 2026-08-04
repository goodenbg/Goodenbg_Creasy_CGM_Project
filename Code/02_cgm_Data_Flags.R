---
title: "02_cgm_Data_Flags"
author: "Gwen Goodenbour"
date: "2026-07-01"
---
# This R script's goal is to flag any outliers in wearables data
  # more or less than 24 hours wear
  # outliers in cgm averages and standard deviations
  # also checking for nobs = 96


library(dplyr)
library(lubridate)
library(hms)

cgm <- read.csv(here("DataProcessed", "cgm_dated"))
# -look for any outliers for start end 24 hr wear- does everyone have ~24 hours of wear?

class(cgm$start)
class(cgm$end)

cgm_summary <- cgm %>%
  mutate(
    start = as_hms(start),
    end = as_hms(end),
    wear_duration = as.numeric(end - start) / 3600
  ) %>%
  summarise(
    n = n(),
    min = min(wear_duration, na.rm = TRUE),
    max = max(wear_duration, na.rm = TRUE),
    mean = mean(wear_duration, na.rm = TRUE),
    median = median(wear_duration, na.rm = TRUE)
  )

#flag difference of more than 1 hour from 24 hours
cgm_flag <- cgm %>%
  mutate(
    start = as_hms(start),
    end = as_hms(end),
    wear_duration = as.numeric(end - start) / 3600,
    wear_duration = ifelse(wear_duration < 0, wear_duration + 24, wear_duration),
    wear_flag = abs(wear_duration - 24) > 2
  ) %>%
  filter(wear_flag) %>%
  select(everything(), wear_duration, wear_flag)

# flag for more or less than 96 nobs
obs_flag <- cgm %>% 
  mutate(
    obs_flag = n_obs < 96 | n_obs > 96
    ) %>%
  filter(obs_flag) %>%
  select(everything(), n_obs, obs_flag)
  

# glucose outliers
names(cgm)

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


# standard deviation flags - more nuanced (CV?)
# flag for 75th quartile
summary(cgm$sd_glucose)
summary(cgm$night_sd_glucose)
summary(cgm$day_sd_glucose)

sd_glucose_flag <- cgm %>% 
  mutate(
    sd_g_flag = sd_glucose > 25,
    sd_day_g_flag = day_sd_glucose > 25,
    sd_night_g_flag = night_sd_glucose > 25
  ) %>%
  filter(sd_g_flag, sd_day_g_flag, sd_night_g_flag) %>%
  select(everything(), sd_glucose, night_sd_glucose,day_sd_glucose, sd_g_flag, sd_day_g_flag, sd_night_g_flag)

