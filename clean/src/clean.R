#!/usr/bin/env Rscript --vanilla
# set expandtab ft=R ts=4 sw=4 ai fileencoding=utf-7
#
# Author: JR
# Maintainer(s): JR
# License:  2020, EICC, GPL v3 or later
#
# -----------------------------------------------------------
# COVID19/clean/src/clean.R

pacman::p_load("tidyverse", "lubridate", "readr", 
               "here", "assertr", "janitor", "forcats")

files <- list(
  ecdc_data = here("owid/covid-19-data/public/data/ecdc/full_data.csv"),
  NYT_data = here("NYTimes/us-counties.csv"),
  cvt_data = here("covid-public-api/v1/states/daily.csv"))

# export the _state_data and _inc csvs
stopifnot(length(files) == 3)

### ECDC data from Our World in Data

# shorten location to loc
# change reserved word date to date_rec to indicate date recorded
# keep all counts of cases and deaths in new dataframe
# populate missing dates and add in counts of 0 
#  (ex: in Afghanistan, no dates between from 
#   03 March 20 -- 08 March 2020 were recorded)
#   and are now listed as having 0 cases or deaths on these dates

ecdc_df <- as.data.frame(read_delim(files$ecdc_data, 
                                              delim = ",")) %>%
  clean_names() %>%
  transmute(loc = as.factor(location),
            date_rec = as.Date(date, "%Y/%m/%d"), 
            new_cases = as.double(new_cases), 
            new_deaths = as.double(new_deaths), 
            total_cases = as.double(total_cases), 
            total_deaths = as.double(total_deaths)) %>%
  group_by(loc) %>% 
  complete(loc, date_rec = seq(min(date_rec), max(date_rec), by = 'day')) %>%
  replace(., is.na(.), 0) %>%
  arrange(date_rec)

# unit tests

index_ecdc <- as.Date("2019-12-31")

ecdc_df <- ecdc_df %>%
  verify(ncol(ecdc_df) == 6 & (nrow(ecdc_df) == 1823270)) %>%
  verify(is.factor(loc) & is.Date(date_rec)) %>%
  verify(sum(total_cases) == 3686829354) %>%
  verify(is.na(new_cases) == FALSE) %>%
  verify(min(date_rec) == index_ecdc)

### NYT data at US State and County Level

# FIPS = fips code/geographic identifier
# earliest date = 01/21/2020
# change reserved word date to date_rec to indicate date recorded

nyt_df <-
    as.data.frame(read_delim(files$NYT_data, delim = ","), 
                  na.rm = FALSE) %>%
    clean_names() %>%
    mutate(date_rec = as.Date(parse_date_time(date, "ymd"))) %>%
    mutate_at(vars(county, state), as.factor) %>%
    mutate_at(vars(cases, deaths), as.integer) %>%
  arrange(date_rec)

# unit tests
index_nyt <- as.Date("2020-01-21")

nyt_df <- nyt_df %>%
  verify(ncol(nyt_df) == 7 & (nrow(nyt_df) == 261070)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 111375905) %>%
  verify(min(date_rec) == index_nyt)

## COVID tracking Project by The Atlantic data at the state level ##
# format date and data_quality_grade, 
# set some dbls of interest to integers, set other dbls of interest
# to factors, sort by date_rec var, remove data with low quality grade

index_cvt <- as.Date("2020-03-23")

good_grades <- c("A+","A","B","C")

cvt_df <-
  as.data.frame(read_delim(files$cvt_data, delim = ","), 
                na.rm = FALSE) %>%
  clean_names() %>%
  mutate(date_rec = as.Date(parse_date_time(date, "ymd")), 
         dgq = as.factor(data_quality_grade)) %>%
  mutate_at(vars(positive, negative, pending, hospitalized_currently, 
                  hospitalized_cumulative, in_icu_currently, in_icu_cumulative, 
                  on_ventilator_currently, on_ventilator_cumulative, 
                  recovered, death, hospitalized, total_tests_viral, 
                  positive_tests_viral, negative_tests_viral, 
                  positive_cases_viral, positive_increase, 
                  negative_increase, total, total_test_results, 
                  total_test_results_increase, pos_neg, death_increase, 
                  hospitalized_increase), as.integer, na.rm = TRUE) %>%
  mutate_at(vars(state, fips), as.factor) %>%
  arrange(date_rec) %>%
  filter(dgq %in% good_grades)

# unit tests

cvt_df <- cvt_df %>%
  verify(ncol(cvt_df) == 41 & (nrow(cvt_df) == 4943)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(max(total) == 3694345) %>%
  verify(dgq %in% good_grades) %>%
  verify(min(date_rec) == index_cvt)

# create graphing specific datasets

# create state level data sets from nyt and ppos data from filtered cvt

cvt_filt <- cvt_df %>%
  mutate(p_pos = round(((positive/total)*100), digits = 2),
         p_pend = round(((pending/total)*100), digits = 2)) %>%
  select(date_rec, state, dgq, hash, p_pos, 
         positive, negative, pending, 
         p_pend, total)

nyt_inc <- nyt_df %>%
  select(date_rec, state, cases, deaths)

###############################################################

# would love to make these a loop eventually but not sure how to filter by 
# a looping value 

# states_full <- as.list("Virginia", "Georgia", "New York", 
#                  "Texas", "Florida", "Michigan", 
#                  "North Carolina")

# state_ab <- as.list("VA", "GA", "NY", "TX", "FL", "MI", "NC")

#################################################################

# VA
va_inc <- nyt_inc %>%
  filter(state == "Virginia")

va_ppos <- cvt_filt %>%
  filter(state == "VA")

va_state_data <- left_join(va_inc, va_ppos, by = "date_rec") %>%
  clean_names() %>%
  mutate(state = as.character(state_x))%>%
  select(-c("state_x", "state_y"))

# GA

ga_inc <- nyt_inc %>%
  filter(state == "Georgia")

ga_ppos <- cvt_filt %>%
  filter(state == "GA")

ga_state_data <- left_join(ga_inc, ga_ppos, by = "date_rec") %>%
  clean_names() %>%
  mutate(state = as.character(state_x))%>%
  select(-c("state_x", "state_y"))

# NY

ny_inc <- nyt_inc %>%
  filter(state == "New York")

ny_ppos <- cvt_filt %>%
  filter(state == "NY")

ny_state_data <- left_join(ny_inc, ny_ppos, by = "date_rec") %>%
  clean_names() %>%
  mutate(state = as.character(state_x))%>%
  select(-c("state_x", "state_y"))

# FL

fl_inc <- nyt_inc %>%
  filter(state == "Florida")

fl_ppos <- cvt_filt %>%
  filter(state == "FL")

fl_state_data <- left_join(fl_inc, fl_ppos, by = "date_rec") %>%
  clean_names() %>%
  mutate(state = as.character(state_x))%>%
  select(-c("state_x", "state_y"))

# TX

tx_inc <- nyt_inc %>%
  filter(state == "Texas")

tx_ppos <- cvt_filt %>%
  filter(state == "TX")

tx_state_data <- left_join(tx_inc, tx_ppos, by = "date_rec") %>%
  clean_names() %>%
  mutate(state = as.character(state_x))%>%
  select(-c("state_x", "state_y"))

# TX

tx_inc <- nyt_inc %>%
  filter(state == "Texas")

tx_ppos <- cvt_filt %>%
  filter(state == "TX")

tx_state_data <- left_join(tx_inc, tx_ppos, by = "date_rec") %>%
  clean_names() %>%
  mutate(state = as.character(state_x))%>%
  select(-c("state_x", "state_y"))

### create county level lists for counties of interest

#dc 
dc_inc <- nyt_df %>%
  filter(state == "District of Columbia" & county == "District of Columbia")

# va
fx_inc <- nyt_df %>%
  filter(state == "Virginia" & county == "Fairfax")

st_inc <- nyt_df %>%
  filter(state == "Virginia" & county == "Stafford")

#ga
ft_inc <- nyt_df %>%
  filter(state == "Georgia" & county == "Fulton")

gt_inc <- nyt_df %>%
  filter(state == "Georgia" & county == "Gwinnett")

# fl
mt_inc <- nyt_df %>%
  filter(state == "Florida" & county == "Manatee")

# ny 
md_inc <- nyt_df %>%
  filter(state == "New York" & county == "Madison")

#tx 
ty_inc <- nyt_df %>%
  filter(state == "Texas" & county == "Taylor")

# nc
rd_inc <- nyt_df %>%
  filter(state == "North Carolina" & county == "Randolph")

# mi 
gen_inc <- nyt_df %>%
  filter(state == "Michigan" & county == "Genesee")


###done###
