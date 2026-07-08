---
title: "04_mock_presentation"
author: "Gwen Goodenbour"
date: "2026-07-07"
---

library(haven)
library(readxl)
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

#----- Visualizing missing data before and after clean -----
# first - make a merge file of uncleaned data
# load raw cgm, dates(repeats not replaced), and exercise
cgm<-read_sas(here("DataProcessed","analysis_v20260430.sas7bdat"))

dates<-read_excel(here("DataRaw", "23-1388 Subject Dates.xlsx"))

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


# standardize naming convention further
dates <- dates %>% 
  rename(chamber_y_n = `Chamber (Y/N)`) %>% 
  rename(baseline = `Baseline`) %>% 
  rename(cond_1 = `Condition 1`) %>% 
  rename(cond_2 = `Condition 2`) %>% 
  rename(cond_3 = `Condition 3`) %>% 
  rename(cond_4 = `Condition 4`) %>% 
  rename(rep_cond = `Repeated Condition`) %>% 
  rename(notes = `NOTES`)
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

# summarize exercise data to one row per participant per condition
ex_summary <- ex_long %>%
  group_by(id, condition) %>%
  
  summarise(
    n_exercise_days  = n(),
    mean_hr          = mean(hr, na.rm = TRUE),
    mean_rpe         = mean(rpe, na.rm = TRUE),
    mean_tread_grade = mean(tread_grade, na.rm = TRUE),
    mean_tread_speed = mean(tread_speed, na.rm = TRUE),
    mean_time        = mean(time, na.rm = TRUE),
    
    # food_date and wake are datetime/character - take first non-NA 
    n_wake_recorded  = sum(!is.na(wake)),
    n_food_recorded  = sum(!is.na(food_date)),
    .groups = "drop"
  )

# join summarized exercise data to cgm3
merged1 <- cgm3 %>%
  left_join(ex_summary, by = c("id", "condition"))

#remove unneeded column
merged1 <- merged1[, !names(merged1) %in% c("notes")]
                         
# LOADING IN CLEANED + MERGED DATASET
merged_clean <- read.csv(here("DataProcessed", "clean_merge_triple_dataset"))

# ----- Comparing missing data pre and post merge! -----
#install.packages("visdat")
library(visdat)
vis_dat(merged1)
vis_dat(merged_clean)

# fix cut off labels in vis_dat

vis_dat(merged1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        theme(plot.margin = margin(l = 20)))

# check which columns are dates stored as character
merged_clean %>%
  select(where(is.character)) %>%
  names()

# convert date columns from character to Date
merged_clean <- merged_clean %>%
  mutate(across(c(date, cond_start, cond_end), as.Date))

vis_dat(merged_clean) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

#install.packages("naniar")
library(naniar)

gg_miss_var(merged1)
gg_miss_var(merged_clean)


# predict which variables and their values are important for-
# predicting the proportion of missingness:

#install.packages("rpart")
library(rpart)
#install.packages("rpart.part")
library(rpart.plot)

merged1 %>%
  add_prop_miss() %>%
  rpart(prop_miss_all ~ ., data = .) %>%
  prp(type = 5, extra = 101, prefix = "Prop. Miss = ")

merged_clean %>%
  add_prop_miss() %>%
  rpart(prop_miss_all ~ ., data = .) %>%
  prp(type = 4, extra = 101, prefix = "Prop. Miss = ")


# improving legibility
merged1 %>%
  add_prop_miss() %>%
  rpart(prop_miss_all ~ ., data = ., 
        control = rpart.control(maxdepth = 24)) %>%  # limit tree depth
  prp(type = 5, 
      extra = 101, 
      prefix = "Prop. Miss = ",
      box.palette = "Blues",    # add color for readability
      shadow.col = "gray",      # add shadow to boxes
      nn = TRUE,                # show node numbers
      compress = TRUE,          # compress horizontal spacing
      ycompress = TRUE,         # compress vertical spacing
      cex = 0.5,                # text size
      tweak = 1.2)              # increase text size scaling

merged_clean %>%
  add_prop_miss() %>%
  rpart(prop_miss_all ~ ., data = ., 
        control = rpart.control(maxdepth = 24)) %>%  # limit tree depth
  prp(type = 5, 
      extra = 101, 
      prefix = "Prop. Miss = ",
      box.palette = "Blues",    # add color for readability
      shadow.col = "gray",      # add shadow to boxes
      nn = TRUE,                # show node numbers
      compress = TRUE,          # compress horizontal spacing
      ycompress = TRUE,         # compress vertical spacing
      cex = 0.5,                # text size
      tweak = 1.2)              # increase text size scaling


# prop_miss_all models the tree;s leaves to predict the expected
# missingness proportion for the subgroups

# ---------------------------------------------------------------------------
# analyzing wake time reporting 

# save column as its own table
wake <- merged_clean %>%
  select(id, condition, n_wake_recorded) %>% 
  filter(condition != "BL")

wake_id2 <- wake %>%
  group_by(id, condition) %>%
  summarise(n_wake_recorded = first(n_wake_recorded), .groups = "drop") 

ggplot(wake_id2, aes(x = condition, y = reorder(id, n_wake_recorded))) +
  geom_tile(aes(fill = n_wake_recorded), height = 0.9) +
  scale_fill_viridis_c() +
  labs(x = "Condition", y = "id", fill = "n_wake_recorded") +
  theme_minimal()

# making reportable average tables for number of recorded wake times per condition
library(gt)
wake_avg <- wake %>%
group_by(condition) %>%
  summarise(
    mean_n_wake_recorded = mean(n_wake_recorded, na.rm = TRUE),
    .groups = "drop"
  )

wake_avg %>%
  gt() %>%
  fmt_number(columns = mean_n_wake_recorded, decimals = 2) %>%
  cols_label(condition = "Condition",
             mean_n_wake_recorded = "Mean n_wake_time") %>%
  tab_header(title = "Average number of recorded wake times per condition")



# quantifying number of conditions per participant--------------------
cond_per_id <- wake %>%
  filter(!is.na(condition)) %>%
  distinct(id, condition) %>%
  count(id, name = "n_conditions")


ggplot(cond_per_id, aes(x = reorder(id, n_conditions), y = n_conditions, fill = factor(id))) +
  geom_col(color = "grey30") +
  scale_fill_viridis_d(option = "D", begin = 0.15, end = 0.85) +
  labs(x = "id", y = "Number of distinct conditions", fill = "id") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


filter <-  cond_per_id %>% 
  filter(n_conditions == 4)

out <- cond_per_id %>% 
  filter(n_conditions != 4)


# counts of distinct days with data per id, using only ids where n_conditions == 4
cond_per_id <- wake %>%
  filter(!is.na(condition)) %>%
  distinct(id, condition) %>%
  count(id, name = "n_conditions")

cond4_ids <- cond_per_id %>%
  filter(n_conditions == 4) %>%
  pull(id) %>%
  unique()

days_per_id <- merged_clean %>%
  filter(!is.na(mean_hr)) %>%
  filter(id %in% cond4_ids) %>%
  distinct(id, date) %>%             
  count(id, name = "n_days")

ggplot(days_per_id, aes(x = reorder(id, n_days), y = n_days, fill = factor(id))) +
  geom_col(color = "grey30") +
  scale_fill_viridis_d(option = "D", begin = 0.15, end = 0.85) +
  labs(x = "id", y = "Number of distinct days with exercise data", fill = "id") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


#install.packages("grateful")
library(grateful)

# scan your script for packages used and generate citations
#cite_packages(output = "paragraph",  # outputs a citation paragraph
              #out.dir = here("Dissemination"))  # saves output folder



