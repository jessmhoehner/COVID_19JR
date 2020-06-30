#!/usr/bin/env Rscript --vanilla
# set expandtab ft=R ts=4 sw=4 ai fileencoding=utf-7
#
# Author: JR
# Maintainer(s): JR
# License: 2020, GPL v3 or later
#
# -----------------------------------------------------------
# COVID19/graph/src/graph.R

pacman::p_load("tidyverse", "knitr", "here", "assertr",
               "scales", "forcats", "readr", "janitor")

# pull in _state_data and _inc datasets

files <- list(ga = here("graph/input/ga_stlev.csv"),
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
              cvt_data = here("graph/input/cvt_filtered.csv"),
              nyt_clean = here("graph/input/nyt_df.csv"),
              nyt_county_cfrs = here("graph/input/nyt_county_cfrs.csv"),
              ga_cc_clean = here("graph/input/countycases_clean.csv"), 
              ga_d_clean = here("graph/input/deaths_clean.csv"), 
              ga_demo_clean = here("graph/input/demo_clean.csv"),
              
              
              ppos_facetplot = here("graph/output/plots/multistate_ppos.png")
)

locs <- list(files$ga,files$ny, files$fl,files$va, files$mi,files$nc,files$tx, 
               files$ga_ft, files$ga_gw, files$va_fx, files$va_st, files$fl_mt, 
               files$mi_gn, files$nc_rn, files$tx_ty, files$dc_dc, files$ny_md, 
             files$va_mn, files$cvt_data, files$nyt_clean, files$nyt_county_cfrs, 
             files$ga_cc_clean, files$ga_d_clean, files$ga_demo_clean)

stopifnot(length(locs) == 24)
stopifnot(locs %in% files == TRUE)

# creates a list called cleandfs, containing dataframes created from clean data
dfs <- lapply(locs, function(x) {
  
  df_a <- as.data.frame(read_delim(x, delim = "|")) %>%
    clean_names()
  
})

# add names for each df in the list corresponding to appropriate names for each
# spreadheet

names(dfs) <-  c("Georgia", "New_York", "Florida", "Virginia", "Michigan", 
                 "North_Carolina", "Texas", "Fulton_GA", "Gwinnett_GA", 
                 "Fairfax_VA","Stafford_VA", "Manatee_FL", "Genesee_MI", 
                 "Randolph_NC", "Taylor_TX","Washington_DC", "Madison_NY", 
                 "Manassas_VA", "Full_Set", "NYT_cases", "NYT_CountyCFRs", 
                 "GADPH_Cases_County", "GADPH_deaths", "GADPH_demography")

# faceted plot of percentage of positive cases for states of interest

state_abbs <- c("GA", "NY", "FL", "VA", "NC", "TX", "MI", "CA")

full <- as.data.frame(pluck(dfs, 19)) %>%
  filter(state %in% state_abbs) %>%
  mutate(state_fct = as.factor(state))

labs <- c("California (N = 117,092,611)", "Florida (N = 62,536,788)", 
          "Georgia (N = 27,692,108)", "Michigan (N = 36,661,684)", 
          "North Carolina (N = 25,399,116)", "New York (N = 133,026,236)", 
          "Texas (N = 56,839,328)", "Virginia (N = 18,756,128)")

levels(full$state_fct) <- labs

#plot observed cases
(fct_plot <- ggplot(full) +
  geom_line(aes(date_rec, p_pos), color = "#b2182b", size = 1) +
  facet_wrap( ~state_fct, ncol = 4) +
  labs(x ="Time (Months)", y = "Percentage of Positive Cases", 
       title = "Percentage of Positive Cases Over Time", 
       subtitle = "N = Total Cases Tested") +
  theme_minimal() +
  ylim(c(0, 100)))

# save each graph individually
ggsave(filename = files$ppos_facetplot, 
       plot = last_plot(),
       device = "png",
       dpi = 600)

# subset list of clean data frames

states <- dfs[1:7]
counties <- dfs[8:18]

### state level cases/day with percent positive tests ###

for (j in seq_along(states)){
  
  #messages for the user to keep them aware of model progress
  print(paste0("Creating plot for ",names(states)[j]))
  
  df_b <- as.data.frame(pluck(states, j))

  #plot observed cases
  ggplot(df_b) +
    geom_line(aes(date_rec, p_pos), 
              color = "#b2182b", 
              size = 2, 
              show.legend = TRUE) +
    labs(x ="Time (Days)", y = "Percentage of Cases", 
         title = names(states)[j]) +
    theme_minimal() +
    ylim(c(0, 100))

  # save each graph individually
  ggsave(filename = here(paste0("graph/output/plots/", names(states)[j],
                                "_ppos.png", sep = "")), 
         plot = last_plot(),
         device = "png",
         dpi = 600)

  #message to let the user know that each iteration has completed
  print(paste0("Plot for ",names(states)[j], " created successfully"))

}

### county level cases by day ###

for (i in seq_along(counties)){
  
  #messages for the user to keep them aware of model progress
  print(paste0("Creating plot for ",names(counties)[i]))
  
  df_c <- as.data.frame(pluck(counties, i))
  
  #plot observed cases
  ggplot(df_c) +
    geom_col(aes(date_rec, cases), color = "#f7f7f7", fill = "#053061") +
       labs(x ="Time (Days)", y = "Cases", title = names(counties)[i]) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=90)) +
    ylim(c(0, (max(df_c$cases, na.rm=TRUE) + 2000)))
  
  # save each graph individually
  ggsave(filename = here(paste0("graph/output/plots/", names(counties)[i],
                               "_casesbyday.png", sep = "")), 
         plot = last_plot(),
         device = "png",
         dpi = 600)
  
  #message to let the user know that each iteration has completed
  print(paste0("Plot for ",names(counties)[i], " created successfully"))
  
}

# case fatality rates #########################################################

# what are the county specific cfrs in known counties?
cfr_df <- as.data.frame(pluck(dfs, 21)) %>%
  filter(county != "Unknown")

va_cfr <- cfr_df %>%
  filter(state == "Virginia") %>%
  filter(county == "Stafford" | county == "Fairfax" | county == "Manassas City" |
           county == "Arlington") 

(cfr_plot <- ggplot(va_cfr, aes(county, county_cfr, 
                                color = county , 
                                fill = county)) +
  geom_dotplot(binaxis = "y", stackdir = "center") +
  theme_minimal() +
  labs(title = "Case-Fatality Rates by County"))
  
# how have cfrs changed over time for each state?
cases_ga <- as.data.frame(pluck(dfs, 1))

(ga_cfr_plot <- ggplot(cases_ga, aes(date_rec, daily_cfr)) +
    geom_line(color = "red") +
    theme_minimal() +
    ylim(0, 5) +
    labs(title = "Case-Fatality Rates Over Time (Georgia)"))

cases_ny <- as.data.frame(pluck(dfs, 2))

(ny_cfr_plot <- ggplot(cases_ny, aes(date_rec, daily_cfr)) +
    geom_line(color = "red") +
    theme_minimal() +
    ylim(0, 10) +
    labs(title = "Case-Fatality Rates Over Time (New York)"))



# snippets

ggplot(flt_dths_counts, aes(sex, cfr_fult, 
                            color = race, 
                            fill = race)) +
  geom_jitter(width = 0.025) +
  theme_minimal() + 
  ylim(0, 2.0) +
  labs(title = "Case-Fatality Ratios by Age Group and Race for Females in Fulton County")



# what are the cfrs in each race by age group?
ggplot(fult_fems, aes(age_gp, cfr_fult_fems, 
                      color = race, 
                      fill = race)) +
  geom_jitter(width = 0.025) +
  theme_minimal() + 
  ylim(0, 2.0) +
  labs(title = "Case-Fatality Rates by Age Group and Race for Females in Fulton County")

# is possible that this is due to chronic condition status?

# what are the cfrs in each race by chronic condition status?
ggplot(fult_fems, aes(chronic_condition, cfr_fult_fems, 
                      color = race, 
                      fill = race)) + 
  geom_jitter(width = 0.025) + 
  theme_minimal() + 
  ylim(0, 2.0) +
  labs(title = "Case-Fatality Rates by Chronic Condition Status and Race for Females in Fulton County", 
       ylab = "Case-Fatality Rate", 
       xlab = "Chronic Condition Status")


# what are the cfrs in each race by age group?
ggplot(ga_demo_df, aes(ethnicity, logcfr_agg, 
                       color = race, 
                       fill = race)) +
  geom_jitter(width = 0.15, size = 3) +
  theme_minimal() + 
  ylim(-5, 5) +
  labs(title = "Case-Fatality Ratios by Ethnicity and Race in Georgia")



# done
