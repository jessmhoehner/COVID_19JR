
#!/usr/bin/env Rscript --vanilla
# set expandtab ft=R ts=4 sw=4 ai fileencoding=utf-8
#
# Author: JR
# Maintainer(s): JR
# License: ?
#
# -----------------------------------------------------------
# COVID19/clean/src/clean.R

pacman::p_load("tidyverse", "lubridate", 
               "here", "assertr", "janitor", "reshape2")

files <- list(input_data= here::here("clean/input/full_data.csv"),
              cleaned_data = here::here("write/input/full_data_clean.csv"))

stopifnot(length(files) == 2)

# clean up full data set
# shorten location to loc
# change reserved word date to date_rec to indicate date recorded
# keep all counts of cases and deaths in new dataframe
# populate missing dates and add in counts of 0 
#   (ex: in Afghanistan, no dates between from 03 March 20 -- 08 March 2020 were recorded)
#   and are now listed as having 0 cases or deaths on these dates

cases_032320 <- as.data.frame(readr::read_delim(files$input_data, delim = ",")) %>%
  clean_names() %>%
  transmute(loc = as.factor(location),
         date_rec = as.Date(date, "%Y/%m/%d"), 
         new_cases = as.double(new_cases), 
         new_deaths = as.double(new_deaths), 
         total_cases = as.double(total_cases), 
         total_deaths = as.double(total_deaths)) %>%
  group_by(loc) %>% 
  complete(loc, date_rec = seq(min(date_rec), max(date_rec), by = 'day')) %>%
  replace(., is.na(.), 0)

cases_032320  <- cases_032320 %>%
  verify(ncol(cases_032320) == 6 & (nrow(cases_032320) == 1266656)) %>%
  verify(is.factor(loc) & is.Date(date_rec)) %>%
  verify(is.na(new_cases) == FALSE)%>% 
  write_delim(files$cleaned_data, delim = "|")

###done###