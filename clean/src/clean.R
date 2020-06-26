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
  ny_md = here("graph/input/ny_madison.csv")
)

stopifnot(length(files) == 20)

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

# create graphing specific datasets #############################

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

# unit tests

index_va <- as.Date("2020-03-07")

va_state_data  <- va_state_data  %>%
  verify(ncol(va_state_data ) == 12 & (nrow(va_state_data ) == 11351)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 2313600) %>%
  verify(min(date_rec) == index_va)%>%
  write_delim(files$va, delim = "|")

# GA

ga_inc <- nyt_inc %>%
  filter(state == "Georgia")

ga_ppos <- cvt_filt %>%
  filter(state == "GA")

ga_state_data <- left_join(ga_inc, ga_ppos, by = "date_rec") %>%
  clean_names() %>%
  mutate(state = as.character(state_x))%>%
  select(-c("state_x", "state_y"))

# unit tests

index_ga <- as.Date("2020-03-02")

ga_state_data  <- ga_state_data  %>%
  verify(ncol(ga_state_data ) == 12 & (nrow(ga_state_data ) == 14489)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 2755663) %>%
  verify(min(date_rec) == index_ga)%>%
  write_delim(files$ga, delim = "|")

# unit tests

# NY

ny_inc <- nyt_inc %>%
  filter(state == "New York")

ny_ppos <- cvt_filt %>%
  filter(state == "NY")

ny_state_data <- left_join(ny_inc, ny_ppos, by = "date_rec") %>%
  clean_names() %>%
  mutate(state = as.character(state_x))%>%
  select(-c("state_x", "state_y"))

# unit tests

index_ny <- as.Date("2020-03-01")

ny_state_data  <- ny_state_data  %>%
  verify(ncol(ny_state_data ) == 12 & (nrow(ny_state_data ) == 5669)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 26134161) %>%
  verify(min(date_rec) == index_ny)%>%
  write_delim(files$ny, delim = "|")

# FL

fl_inc <- nyt_inc %>%
  filter(state == "Florida")

fl_ppos <- cvt_filt %>%
  filter(state == "FL")

fl_state_data <- left_join(fl_inc, fl_ppos, by = "date_rec") %>%
  clean_names() %>%
  mutate(state = as.character(state_x))%>%
  select(-c("state_x", "state_y"))

# unit tests

index_fl <- as.Date("2020-03-01")

fl_state_data  <- fl_state_data  %>%
  verify(ncol(fl_state_data ) == 12 & (nrow(fl_state_data ) == 6450)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 3718582) %>%
  verify(min(date_rec) == index_fl)%>%
  write_delim(files$fl, delim = "|")

# nc

nc_inc <- nyt_inc %>%
  filter(state == "North Carolina")

nc_ppos <- cvt_filt %>%
  filter(state == "NC")

nc_state_data <- left_join(nc_inc, nc_ppos, by = "date_rec") %>%
  clean_names() %>%
  mutate(state = as.character(state_x))%>%
  select(-c("state_x", "state_y"))

# unit tests

index_nc <- as.Date("2020-03-03")

nc_state_data  <- nc_state_data  %>%
  verify(ncol(nc_state_data ) == 12 & (nrow(nc_state_data ) == 8808)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 1653003) %>%
  verify(min(date_rec) == index_nc)%>%
  write_delim(files$nc, delim = "|")

# TX

tx_inc <- nyt_inc %>%
  filter(state == "Texas")

tx_ppos <- cvt_filt %>%
  filter(state == "TX")

tx_state_data <- left_join(tx_inc, tx_ppos, by = "date_rec") %>%
  clean_names() %>%
  mutate(state = as.character(state_x))%>%
  select(-c("state_x", "state_y"))

# unit tests

index_tx <- as.Date("2020-02-12")

tx_state_data  <- tx_state_data  %>%
  verify(ncol(tx_state_data ) == 12 & (nrow(tx_state_data ) == 18746)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 3906892) %>%
  verify(min(date_rec) == index_tx)%>%
  write_delim(files$tx, delim = "|")

# mi

mi_inc <- nyt_inc %>%
  filter(state == "Michigan")

mi_ppos <- cvt_filt %>%
  filter(state == "MI")

mi_state_data <- left_join(mi_inc, mi_ppos, by = "date_rec") %>%
  clean_names() %>%
  mutate(state = as.character(state_x))%>%
  select(-c("state_x", "state_y"))

# unit tests

index_mi <- as.Date("2020-03-10")

mi_state_data  <- mi_state_data  %>%
  verify(ncol(mi_state_data ) == 12 & (nrow(mi_state_data ) == 7251)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 3840989) %>%
  verify(min(date_rec) == index_mi)%>%
  write_delim(files$mi, delim = "|")

### create county level lists for counties of interest

#dc 
dc_inc <- nyt_df %>%
  filter(state == "District of Columbia" & county == "District of Columbia")

dc_inc <- dc_inc  %>%
  verify(ncol(dc_inc) == 7 & (nrow(dc_inc) == 108)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 499158) %>%
  write_delim(files$dc_dc, delim = "|")

# va
fx_inc <- nyt_df %>%
  filter(state == "Virginia" & county == "Fairfax")

fx_inc <- fx_inc  %>%
  verify(ncol(fx_inc) == 7 & (nrow(fx_inc) == 108)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 555345) %>%
  write_delim(files$va_fx, delim = "|")

st_inc <- nyt_df %>%
  filter(state == "Virginia" & county == "Stafford")

st_inc <- st_inc  %>%
  verify(ncol(st_inc) == 7 & (nrow(st_inc) == 99)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 36684) %>%
  write_delim(files$va_st, delim = "|")

#ga
ft_inc <- nyt_df %>%
  filter(state == "Georgia" & county == "Fulton")

ft_inc <- ft_inc  %>%
  verify(ncol(ft_inc) == 7 & (nrow(ft_inc) == 113)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 284413) %>%
  write_delim(files$ga_ft, delim = "|")

gt_inc <- nyt_df %>%
  filter(state == "Georgia" & county == "Gwinnett")

gt_inc <- gt_inc  %>%
  verify(ncol(gt_inc) == 7 & (nrow(gt_inc) == 108)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 224822) %>%
  write_delim(files$ga_gw, delim = "|")

# fl
mt_inc <- nyt_df %>%
  filter(state == "Florida" & county == "Manatee")

mt_inc <- mt_inc  %>%
  verify(ncol(mt_inc) == 7 & (nrow(mt_inc) == 114)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 66239) %>%
  write_delim(files$fl_mt, delim = "|")

# ny 
md_inc <- nyt_df %>%
  filter(state == "New York" & county == "Madison")

md_inc <- md_inc  %>%
  verify(ncol(md_inc) == 7 & (nrow(md_inc) == 93)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 18575) %>%
  write_delim(files$ny_md, delim = "|")

#tx 
ty_inc <- nyt_df %>%
  filter(state == "Texas" & county == "Taylor")

ty_inc <- ty_inc  %>%
  verify(ncol(ty_inc) == 7 & (nrow(ty_inc) == 88)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 22392) %>%
  write_delim(files$tx_ty, delim = "|")

# nc
rd_inc <- nyt_df %>%
  filter(state == "North Carolina" & county == "Randolph")

rd_inc <- rd_inc  %>%
  verify(ncol(rd_inc) == 7 & (nrow(rd_inc) == 91)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 34863) %>%
  write_delim(files$nc_rd, delim = "|")

# mi 
gn_inc <- nyt_df %>%
  filter(state == "Michigan" & county == "Genesee")

gn_inc <- gn_inc  %>%
  verify(ncol(gn_inc) == 7 & (nrow(gn_inc) == 96)) %>%
  verify(is.na(date_rec) == FALSE) %>%
  verify(sum(cases) == 143328) %>%
  write_delim(files$mi_gn, delim = "|")

###done###
