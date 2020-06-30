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
  NYT_data = here("NYTimes/us-counties.csv"),
  cvt_data = here("covid-public-api/v1/states/daily.csv"), 
  ga_countycases = here("clean/input/countycases.csv"), 
  ga_deaths = here("clean/input/deaths.csv"), 
  ga_demo = here("clean/input/demographics.csv"), 
  
  ga = here("graph/input/ga_stlev.csv"),
  ny = here("graph/input/ny_stlev.csv"),
  fl = here("graph/input/fl_stlev.csv"),
  va = here("graph/input/va_stlev.csv"),
  mi = here("graph/input/mi_stlev.csv"),
  nc = here("graph/input/nc_stlev.csv"),
  tx = here("graph/input/tx_stlev.csv"),
  
  ga_ft = here("graph/input/ga_fulton.csv"),
  ga_gw = here("graph/input/ga_gwinnett.csv"),
  va_fx = here("graph/input/va_fairfax.csv"),
  va_st = here("graph/input/va_stafford.csv"),
  fl_mt = here("graph/input/fl_manatee.csv"),
  mi_gn = here("graph/input/mi_genesee.csv"),
  nc_rn = here("graph/input/nc_randolph.csv"),
  tx_ty = here("graph/input/tx_taylor.csv"),
  dc_dc = here("graph/input/dc_dc.csv"),
  ny_md = here("graph/input/ny_madison.csv"), 
  va_mn = here("graph/input/va_manassas.csv"),
  
  
  clean_cvt = here("graph/input/cvt_filtered.csv"),
  nyt_clean = here("graph/input/nyt_df.csv"),
  nyt_county_cfrs = here("graph/input/nyt_county_cfrs.csv"),
  ga_cc_clean = here("graph/input/countycases_clean.csv"), 
  ga_d_clean = here("graph/input/deaths_clean.csv"), 
  ga_demo_clean = here("graph/input/demo_clean.csv")
)

stopifnot(length(files) == 29)

### NYT data at US State and County Level

# FIPS = fips code/geographic identifier
# earliest date = 01/21/2020
# change reserved word date to date_rec to indicate date recorded

nyt_df <-as.data.frame(read_delim(files$NYT_data, delim = ","),na.rm = FALSE) %>%
    clean_names() %>%
    mutate(date_rec = as.Date(parse_date_time(date, "ymd"))) %>%
    mutate_at(vars(county, state), as.factor) %>%
    mutate_at(vars(cases, deaths), as.integer) %>%
  arrange(date_rec) %>%
  group_by(date_rec)

# unit tests
index_nyt <- as.Date("2020-01-21")

# unit tests
index_nyt <- as.Date("2020-01-21")

nyt_df <- nyt_df %>%
  verify(ncol(nyt_df) == 7 & (nrow(nyt_df) == 261070)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 111375905) %>%
  verify(min(date_rec) == index_nyt) %>%
  write_delim(files$nyt_clean, delim = "|")

# obtain case fatality rates for each U.S. County according to NYT

# removed data where CFR is "Inf"
# state   county  cases deaths cfr
#	Arizona	Unknown	  0   	52  	Inf
#	Kansas	Unknown	  0	    28   	Inf
#	Pennsylvania	Unknown	0	  3	  Inf
#	South Dakota	Unknown	0	  10	Inf

nyt_counts <- as.data.frame(count(nyt_df, county, state, 
                                  cases, deaths, date_rec)) %>%
  group_by(state, county) %>%
  summarise(agg_cases = sum(cases), 
            agg_deaths = sum(deaths)) %>%
  mutate(county_cfr = as.numeric(round(((agg_deaths/agg_cases)*100), 
                                       digits = 2))) %>%
  filter(county_cfr != "Inf")

# unit tests

nyt_counts <- nyt_counts %>%
  verify(ncol(nyt_counts) == 5 & (nrow(nyt_counts) == 3065)) %>%
  verify(sum(agg_cases) == 111375905) %>%
  write_delim(files$nyt_county_cfrs, delim = "|")

## COVID tracking Project by The Atlantic data at the state level ##
# format date and data_quality_grade, 
# set some dbls of interest to integers, set other dbls of interest
# to factors, sort by date_rec var, remove data with low quality grade

index_cvt <- as.Date("2020-03-23")

good_grades <- c("A+","A","B","C")

cvt_filt <-
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
  filter(dgq %in% good_grades) %>%
  group_by(state, date_rec) %>%
  summarise_at(vars(positive, negative, pending, total), 
               list(daily_cases_cvt = sum)) %>%
  mutate(p_pos = round(((positive_daily_cases_cvt/total_daily_cases_cvt)*100), 
                       digits = 2),
         p_pend = round(((pending_daily_cases_cvt/total_daily_cases_cvt)*100), 
                        digits = 2)) %>%
  select(date_rec, state, p_pos, positive_daily_cases_cvt, negative_daily_cases_cvt, 
         pending_daily_cases_cvt, p_pend, total_daily_cases_cvt)

# unit tests

cvt_filt <- cvt_filt %>%
  verify(ncol(cvt_filt) == 8 & (nrow(cvt_filt) == 4943)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(max(total_daily_cases_cvt) == 3694345) %>%
  verify(min(date_rec) == index_cvt) %>%
  write_delim(files$clean_cvt, delim = "|")

# create graphing specific datasets #############################

# create state level data sets from nyt and ppos data from cvt

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
  filter(state == "Virginia") %>%
  group_by(date_rec) %>%
  summarise_at(vars(cases, deaths), 
               list(daily_nyt = sum)) %>%
  mutate(positive_daily_cases_nyt = as.integer(cases_daily_nyt), 
         daily_cfr = as.numeric(round(
           ((deaths_daily_nyt/positive_daily_cases_nyt)*100),digits = 2))) %>%
  select(-c("cases_daily_nyt"))

va_ppos <- cvt_filt %>%
  filter(state == "VA")

va_state_data <- left_join(va_inc, va_ppos, by = "date_rec") %>%
  clean_names() %>%
  select(-c("state"))

# unit tests

index_va <- as.Date("2020-03-07")
max_date <- as.Date("2020-06-22")

va_state_data  <- va_state_data  %>%
  verify(ncol(va_state_data ) == 10 & (nrow(va_state_data ) == 108)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(max(date_rec) == max_date) %>%
  verify(min(date_rec) == index_va)%>%
  write_delim(files$va, delim = "|")

# GA

ga_inc <- nyt_inc %>%
  filter(state == "Georgia") %>%
  group_by(date_rec) %>%
  summarise_at(vars(cases, deaths), 
               list(daily_nyt = sum)) %>%
  mutate(positive_daily_cases_nyt = as.integer(cases_daily_nyt), 
         daily_cfr = as.numeric(round(
           ((deaths_daily_nyt/positive_daily_cases_nyt)*100),digits = 2))) %>%
  select(-c("cases_daily_nyt"))

ga_ppos <- cvt_filt %>%
  filter(state == "GA")

ga_state_data <- left_join(ga_inc, ga_ppos, by = "date_rec") %>%
  clean_names() %>%
  select(-c("state"))

# unit tests
index_ga <- as.Date("2020-03-02")

ga_state_data  <- ga_state_data  %>%
  verify(ncol(ga_state_data ) == 10 & (nrow(ga_state_data ) == 113)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(max(date_rec) == max_date) %>%
  verify(min(date_rec) == index_ga)%>%
  write_delim(files$ga, delim = "|")

# unit tests

# NY

ny_inc <- nyt_inc %>%
  filter(state == "New York") %>%
  group_by(date_rec) %>%
  summarise_at(vars(cases, deaths), 
               list(daily_nyt = sum)) %>%
  mutate(positive_daily_cases_nyt = as.integer(cases_daily_nyt), 
         daily_cfr = as.numeric(round(
           ((deaths_daily_nyt/positive_daily_cases_nyt)*100),digits = 2))) %>%
  select(-c("cases_daily_nyt"))

ny_ppos <- cvt_filt %>%
  filter(state == "NY")

ny_state_data <- left_join(ny_inc, ny_ppos, by = "date_rec") %>%
  clean_names() %>%
  select(-c("state"))

# unit tests

index_ny <- as.Date("2020-03-01")

ny_state_data  <- ny_state_data  %>%
  verify(ncol(ny_state_data ) == 10 & (nrow(ny_state_data ) == 114)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(max(date_rec) == max_date) %>%
  verify(min(date_rec) == index_ny)%>%
  write_delim(files$ny, delim = "|")

# FL

fl_inc <- nyt_inc %>%
  filter(state == "Florida") %>%
  group_by(date_rec) %>%
  summarise_at(vars(cases, deaths), 
               list(daily_nyt = sum)) %>%
  mutate(positive_daily_cases_nyt = as.integer(cases_daily_nyt), 
         daily_cfr = as.numeric(round(
           ((deaths_daily_nyt/positive_daily_cases_nyt)*100),digits = 2))) %>%
  select(-c("cases_daily_nyt"))

fl_ppos <- cvt_filt %>%
  filter(state == "FL")

fl_state_data <- left_join(fl_inc, fl_ppos, by = "date_rec") %>%
  clean_names() %>%
  select(-c("state"))

# unit tests

index_fl <- as.Date("2020-03-01")

fl_state_data  <- fl_state_data  %>%
  verify(ncol(fl_state_data ) == 10 & (nrow(fl_state_data ) == 114)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(max(date_rec) == max_date) %>%
  verify(min(date_rec) == index_fl)%>%
  write_delim(files$fl, delim = "|")

# nc

nc_inc <- nyt_inc %>%
  filter(state == "North Carolina") %>%
  group_by(date_rec) %>%
  summarise_at(vars(cases, deaths), 
               list(daily_nyt = sum)) %>%
  mutate(positive_daily_cases_nyt = as.integer(cases_daily_nyt), 
         daily_cfr = as.numeric(round(
           ((deaths_daily_nyt/positive_daily_cases_nyt)*100),digits = 2))) %>%
  select(-c("cases_daily_nyt"))

nc_ppos <- cvt_filt %>%
  filter(state == "NC")

nc_state_data <- left_join(nc_inc, nc_ppos, by = "date_rec")  %>%
  clean_names() %>%
  select(-c("state"))

# unit tests

index_nc <- as.Date("2020-03-03")

nc_state_data  <- nc_state_data  %>%
  verify(ncol(nc_state_data ) == 10 & (nrow(nc_state_data ) == 112)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(max(date_rec) == max_date) %>%
  verify(min(date_rec) == index_nc)%>%
  write_delim(files$nc, delim = "|")

# TX

tx_inc <- nyt_inc %>%
  filter(state == "Texas") %>%
  group_by(date_rec) %>%
  summarise_at(vars(cases, deaths), 
               list(daily_nyt = sum)) %>%
  mutate(positive_daily_cases_nyt = as.integer(cases_daily_nyt), 
         daily_cfr = as.numeric(round(
           ((deaths_daily_nyt/positive_daily_cases_nyt)*100),digits = 2))) %>%
  select(-c("cases_daily_nyt"))

tx_ppos <- cvt_filt %>%
  filter(state == "TX")

tx_state_data <- left_join(tx_inc, tx_ppos, by = "date_rec") %>%
  clean_names() %>%
  select(-c("state"))

# unit tests

index_tx <- as.Date("2020-02-12")

tx_state_data  <- tx_state_data  %>%
  verify(ncol(tx_state_data ) == 10 & (nrow(tx_state_data ) == 132)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(max(date_rec) == max_date) %>%
  verify(min(date_rec) == index_tx)%>%
  write_delim(files$tx, delim = "|")

# mi

mi_inc <- nyt_inc %>%
  filter(state == "Michigan") %>%
  group_by(date_rec) %>%
  summarise_at(vars(cases, deaths), 
               list(daily_nyt = sum)) %>%
  mutate(positive_daily_cases_nyt = as.integer(cases_daily_nyt), 
         daily_cfr = as.numeric(round(
           ((deaths_daily_nyt/positive_daily_cases_nyt)*100),digits = 2))) %>%
  select(-c("cases_daily_nyt"))

mi_ppos <- cvt_filt %>%
  filter(state == "MI")

mi_state_data <- left_join(mi_inc, mi_ppos, by = "date_rec") %>%
  clean_names() %>%
  select(-c("state"))

# unit tests

index_mi <- as.Date("2020-03-10")

mi_state_data  <- mi_state_data  %>%
  verify(ncol(mi_state_data ) == 10 & (nrow(mi_state_data ) == 105)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(max(date_rec) == max_date) %>%
  verify(min(date_rec) == index_mi)%>%
  write_delim(files$mi, delim = "|")

### create county level lists for counties of interest #######################

#dc 
dc_inc <- nyt_df %>%
  filter(state == "District of Columbia" & county == "District of Columbia") %>%
  mutate(daily_county_cfr = as.numeric(round(((deaths/cases)*100), 
                                             digits = 2)))

dc_inc <- dc_inc  %>%
  verify(ncol(dc_inc) == 8 & (nrow(dc_inc) == 108)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 499158) %>%
  write_delim(files$dc_dc, delim = "|")

# va
fx_inc <- nyt_df %>%
  filter(state == "Virginia" & county == "Fairfax") %>%
  mutate(daily_county_cfr = as.numeric(round(((deaths/cases)*100), 
                                             digits = 2)))

fx_inc <- fx_inc  %>%
  verify(ncol(fx_inc) == 8 & (nrow(fx_inc) == 108)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 555345) %>%
  write_delim(files$va_fx, delim = "|")

st_inc <- nyt_df %>%
  filter(state == "Virginia" & county == "Stafford") %>%
  mutate(daily_county_cfr = as.numeric(round(((deaths/cases)*100), 
                                             digits = 2)))

st_inc <- st_inc  %>%
  verify(ncol(st_inc) == 8 & (nrow(st_inc) == 99)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 36684) %>%
  write_delim(files$va_st, delim = "|")

mn_inc <- nyt_df %>%
  filter(state == "Virginia" & county == "Manassas city") %>%
  mutate(daily_county_cfr = as.numeric(round(((deaths/cases)*100), 
                                             digits = 2)))

mn_inc <- mn_inc  %>%
  verify(ncol(mn_inc) == 8 & (nrow(mn_inc) == 90)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 48767) %>%
  write_delim(files$va_mn, delim = "|")

#ga
ft_inc <- nyt_df %>%
  filter(state == "Georgia" & county == "Fulton") %>%
  mutate(daily_county_cfr = as.numeric(round(((deaths/cases)*100), 
                                             digits = 2)))

ft_inc <- ft_inc  %>%
  verify(ncol(ft_inc) == 8 & (nrow(ft_inc) == 113)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 284413) %>%
  write_delim(files$ga_ft, delim = "|")

gt_inc <- nyt_df %>%
  filter(state == "Georgia" & county == "Gwinnett") %>%
  mutate(daily_county_cfr = as.numeric(round(((deaths/cases)*100), 
                                             digits = 2)))

gt_inc <- gt_inc  %>%
  verify(ncol(gt_inc) == 8 & (nrow(gt_inc) == 108)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 224822) %>%
  write_delim(files$ga_gw, delim = "|")

# fl
mt_inc <- nyt_df %>%
  filter(state == "Florida" & county == "Manatee") %>%
  mutate(daily_county_cfr = as.numeric(round(((deaths/cases)*100), 
                                             digits = 2)))

mt_inc <- mt_inc  %>%
  verify(ncol(mt_inc) == 8 & (nrow(mt_inc) == 114)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 66239) %>%
  write_delim(files$fl_mt, delim = "|")

# ny 
md_inc <- nyt_df %>%
  filter(state == "New York" & county == "Madison") %>%
  mutate(daily_county_cfr = as.numeric(round(((deaths/cases)*100), 
                                             digits = 2)))

md_inc <- md_inc  %>%
  verify(ncol(md_inc) == 8 & (nrow(md_inc) == 93)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 18575) %>%
  write_delim(files$ny_md, delim = "|")

#tx 
ty_inc <- nyt_df %>%
  filter(state == "Texas" & county == "Taylor") %>%
  mutate(daily_county_cfr = as.numeric(round(((deaths/cases)*100), 
                                             digits = 2)))

ty_inc <- ty_inc  %>%
  verify(ncol(ty_inc) == 8 & (nrow(ty_inc) == 88)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 22392) %>%
  write_delim(files$tx_ty, delim = "|")

# nc
rn_inc <- nyt_df %>%
  filter(state == "North Carolina" & county == "Randolph") %>%
  mutate(daily_county_cfr = as.numeric(round(((deaths/cases)*100), 
                                             digits = 2)))

rn_inc <- rn_inc  %>%
  verify(ncol(rn_inc) == 8 & (nrow(rn_inc) == 91)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 34863) %>%
  write_delim(files$nc_rn, delim = "|")

# mi 
gn_inc <- nyt_df %>%
  filter(state == "Michigan" & county == "Genesee") %>%
  mutate(daily_county_cfr = as.numeric(round(((deaths/cases)*100), 
                                             digits = 2)))

gn_inc <- gn_inc  %>%
  verify(ncol(gn_inc) == 8 & (nrow(gn_inc) == 96)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 143328) %>%
  write_delim(files$mi_gn, delim = "|")


# georgia data 
# ga_data come from here https://dph.georgia.gov/covid-19-daily-status-report 
#   Click on "Download the data(CSV)" to access these same files

# FIXME: add unit tests for GA data

# aggregated cases by county, keeping only real CFRs, 
# 140 counties remain, kept in non-GA res and Unknown groups
cc_df <- as.data.frame(read_delim(files$ga_countycases, 
                                  delim = ",")) %>%
  clean_names() %>%
  rename(cases_per100k = case_rate, 
         n_hosp = hospitalization, 
         n_deaths = deaths, 
         n_pos = positive, 
         cnty_res = county_resident) %>%
  mutate(cnty_cfr = as.numeric((n_deaths/n_pos) * 100), 
         cnty_chr = as.numeric((n_hosp/n_pos)*100), 
         log_cfr = as.numeric(log2(cnty_cfr)), 
         log_chr = as.numeric(log2(cnty_chr))) %>%
  filter(log_cfr != "-Inf") %>%
  filter(log_chr != "-Inf")%>%
  write_delim(files$ga_cc_clean, delim = "|")

# individual deaths, each row is an individual
# age, sex, race, county, chronic condition status
# can do county level CFRs but 
# can't do county level CFR for specific demographic groups because 
# don't know denominator (at county level all case ages, ethnicity, race, etc)
ga_deaths <- as.data.frame(read_delim(files$ga_deaths,
                                      delim = ",")) %>%
  clean_names() %>%
  mutate(age_gp = if_else(age >= 65, "65 and older", "< 65"), 
         age_gp = if_else(age_gp == "< 65" & age < 18, "17 and younger", "65 and older"), 
         age_gp = if_else(age_gp == "65 and older" & age < 65, "18-64", age_gp)) %>%
  mutate_at(vars(county, sex, age_gp, race, chronic_condition), as.factor) %>%
  count(county, sex, age_gp, race, chronic_condition) %>%
  mutate(n_deaths = as.numeric(n)) %>%
  group_by(county, sex, race, age_gp) %>%
  write_delim(files$ga_d_clean, delim = "|")

# state level cases by ethnicity, race
# can do state-level CFR by ethnicity and race
ga_demo_df <- as.data.frame(read_delim(files$ga_demo,
                                       delim = ",")) %>%
  clean_names() %>%
  mutate(logcfr_agg = as.numeric(log2(round((deaths/(confirmed_cases))*100, 
                                            digits = 2)))) %>%
  filter(logcfr_agg != "-Inf") %>%
  write_delim(files$ga_demo_clean, delim = "|")

###done###
