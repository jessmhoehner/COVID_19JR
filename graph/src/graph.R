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
               "scales", "forcats")

files <- list(ecdc_data = here("graph/input/ecdc_clean.csv"),
              nyt_data = here("graph/input/nyt_clean.csv"))

ecdc_cases <- as.data.frame(read_delim(files$ecdc_data, delim="|"))

nyt_cases <- as.data.frame(read_delim(files$nyt_data, delim="|"))

### create scalable list of nyt cases for areas of interest 

#dc 
dc_inc <- nyt_cases %>%
  filter(state == "District of Columbia" & county == "District of Columbia")

# va
fx_inc <- nyt_cases %>%
  filter(state == "Virginia" & county == "Fairfax")

st_inc <- nyt_cases %>%
  filter(state == "Virginia" & county == "Stafford")

#ga
ft_inc <- nyt_cases %>%
  filter(state == "Georgia" & county == "Fulton")

gt_inc <- nyt_cases %>%
  filter(state == "Georgia" & county == "Gwinnett")

# fl
mt_inc <- nyt_cases %>%
  filter(state == "Florida" & county == "Manatee")

# ny 
md_inc <- nyt_cases %>%
  filter(state == "New York" & county == "Madison")

#tx 
ty_inc <- nyt_cases %>%
  filter(state == "Texas" & county == "Taylor")

# nc
rd_inc <- nyt_cases %>%
  filter(state == "North Carolina" & county == "Randolph")

# mi 
gen_inc <- nyt_cases %>%
  filter(state == "Michigan" & county == "Genesee")

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
