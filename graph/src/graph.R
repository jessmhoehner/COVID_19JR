#!/usr/bin/env Rscript --vanilla
# set expandtab ft=R ts=4 sw=4 ai fileencoding=utf-7
#
# Author: JR
# Maintainer(s): JR
# License: ?
#
# -----------------------------------------------------------
# COVID19/graph/src/graph.R

pacman::p_load("tidyverse", "knitr", "here", "assertr",
               "scales", "forcats", "readr", "janitor")

# pull in _state_data and _inc datasets

files <- list(ecdc_data = here("graph/input/ecdc_clean.csv"),
              nyt_data = here("graph/input/nyt_clean.csv"), 
              cvt_data = here("graph/input/cvt_clean.csv"))





# only pull in _state_data and _inc datasets

# locations of interest as a list
locs <- list(dc_inc, fx_inc, st_inc, 
             ft_inc, gt_inc, mt_inc, 
             md_inc, ty_inc, rd_inc, gen_inc)

names(locs) <- c("WashingtonDC", "Fairfax_VA","Stafford_VA",
                 "Fulton_GA", "Gwinnett_GA", "Manatee_FL", 
                 "Madison_NY", "Randolph_NC", "Taylor_TX", "Genesee_MI")

# initialize empty df
df <- data.frame(matrix(0, 200, 7))

# loop to graph each loc of interest and export

for (i in seq_along(locs)){
  
  #messages for the user to keep them aware of model progress
  print(paste0("Creating plotting dataset for ",names(locs)[i]))
  
  df <- as.data.frame(pluck(locs, i))
  
  df <- df %>%
    write_excel_csv(quote = FALSE, path = 
                      here(paste("graph/output/datasets/", 
                                 names(locs)[i],"_obscases.csv", 
                                 sep = "")))
  
  #message to let the user know that each iteration has completed
  print(paste0("Dataset for ", names(locs)[i],
               " has exported successfully."))
  
  #plot observed cases
  ggplot(df) +
    geom_col(aes(date_rec, cases), color = "#f7f7f7", fill = "#053061") +
       labs(x ="Time (Days)", y = "Cases", title = names(locs)[i]) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=90)) +
    ylim(c(0, (max(df$cases, na.rm=TRUE) + 2000)))
  
  # save each graph individually
  ggsave(filename = here(paste("graph/output/plots/", names(locs)[i],
                               "_casesbyday.png", sep = "")), 
         plot = last_plot(),
         device = "png",
         dpi = 600)
  
  #message to let the user know that each iteration has completed
  print(paste0("Plot for ",names(locs)[i], " created successfully"))
  
}

# done
