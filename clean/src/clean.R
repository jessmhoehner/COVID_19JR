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
  cvt_data = here("covid-public-api/v1/states/daily.csv"),
  
  clean_ecdc_data = here("graph/input/ecdc_clean.csv"), 
  clean_nyt_data = here("graph/input/nyt_clean.csv"), 
  clean_cvt_data = here("graph/input/cvt_clean.csv"))

stopifnot(length(files) == 6)

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

pre_cases <- as.Date("2019-12-31")
#add in unit test to make sure dates are not reported prior to this date

ecdc_cases %>%
  verify(ncol(ecdc_cases) == 6 & (nrow(ecdc_cases) == 1823270)) %>%
  verify(is.factor(loc) & is.Date(date_rec)) %>%
  verify(sum(total_cases) == 3686829354) %>%
  verify(is.na(new_cases) == FALSE)%>% 
  write_delim(files$clean_ecdc_data, delim = "|")

### NYT data at US State and County Level

# FIPS = fips code/geographic identifier
# earliest date = 01/21/2020
# change reserved word date to date_rec to indicate date recorded

nyt_df <-
    as.data.frame(read_delim(files$NYT_data, delim = ","), 
                  na.rm = FALSE) %>%
    clean_names() %>%
    mutate(date_rec = as.Date(parse_date_time(date, "ymd")), 
           county = as.factor(county),
           state = as.factor(state), 
           fips = as.double(fips),
           cases = as.double(cases),
           deaths = as.double(deaths)) %>%
  arrange(date_rec)


#add unit tests for dates ocurring at earliest date

nyt_df <- nyt_df %>%
  verify(ncol(nyt_df) == 7 & (nrow(nyt_df) == 261070)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 111375905) %>%
  write_delim(files$clean_nyt_data, delim = "|")

## COVID tracking Project by The Atlantic data at the state level ##
# format date and data_quality_grade, 
# set some dbls of interest to integers, set other dbls of interest
# to factors, sort by date_rec var, remove data with low quality grade

first_case <- as.Date("2020-03-23")

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
# will break when new data are added
# will break if data with quality < C get through the filter
# will break if cases get added with dates before 3/23/2020

cvt_df <- cvt_df %>%
  verify(ncol(cvt_df) == 41 & (nrow(cvt_df) == 4943)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(max(total) == 3694345) %>%
  verify(dgq %in% good_grades) %>%
  verify(min(date_rec) == first_case) %>%
  write_delim(files$clean_cvt_data, delim = "|")

###done###
