#!/usr/bin/env Rscript --vanilla
# set expandtab ft=R ts=4 sw=4 ai fileencoding=utf-7
#
# Author: JR
# Maintainer(s): JR
# License: ?
#
# -----------------------------------------------------------
# COVID19/clean/src/clean.R

pacman::p_load("tidyverse", "lubridate", 
               "here", "assertr", "janitor", "forcats")

files <- list(
  ecdc_data = here("COVID19/owid/covid-19-data/public/data/ecdc/full_data.csv"),
  NYT_data = here("COVID19/NYTimes/us-counties.csv"),
  clean_ecdc_data = here("COVID19/write/input/ecdc_clean.csv"), 
  clean_nyt_data = here("COVID19/write/input/nyt_clean.csv"))

stopifnot(length(files) == 4)

### ECDC data from Our World in Data

# shorten location to loc
# change reserved word date to date_rec to indicate date recorded
# keep all counts of cases and deaths in new dataframe
# populate missing dates and add in counts of 0 
#  (ex: in Afghanistan, no dates between from 
#   03 March 20 -- 08 March 2020 were recorded)
#   and are now listed as having 0 cases or deaths on these dates

ecdc_cases <- as.data.frame(readr::read_delim(files$ecdc_data, 
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

nyt_data <-
    as.data.frame(readr::read_delim(files$NYT_data, delim = ","), 
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

nyt_data <- nyt_data %>%
  verify(ncol(nyt_data) == 7 & (nrow(nyt_data) == 261070)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 111375905) %>%
  write_delim(files$clean_nyt_data, delim = "|")

###done###
