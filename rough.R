library(readxl)
library(stringr)
library(dplyr)
library(magrittr)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(viridis)
library(extrafont)

source('functions.R')

# list data sheets to be read in (all communes per dept)
list_db_paths <- dir('data', full.names = TRUE)
list_dept <- c('DSGA', 'DSS')

list_dat <- map(list_dept[1], WrangleData)

list_variables <- c('cas_vus_tot', 'cas_hosp_tot', 'deces_inst_tot', 'deces_comm_tot', 'deces_tot')

list_dat <- map(list_variables, AnalyseData, data = list_dat) %>% set_names(list_variables)

list_hist_year <- map(list_dat, PlotHistYear)
list_hist_commune <- map(list_dat, PlotHistCommune)

# since the hurricane
list_dat_post_hurr <- 
  map(list_dat, function(data) data %>% filter(year == '2016', epiweek >= 40) %>% mutate(epiweek = factor(epiweek)))

# by day
list_hist_day_post_hurr <- map(list_dat_post_hurr, PlotHistDayPostHurr)

# by week
list_hist_week_post_hurr <- map(list_dat_post_hurr, PlotHistWeekPostHurr)

# by week & commune
list_hist_week_commune_post_hurr <- map(list_dat_post_hurr, PlotHistWeekCommunePostHurr)


## read in different page (CTC - 'Saisie Inst')
list_db_paths <- dir('data', full.names = TRUE)
list_dept <-
  list_db_paths[str_detect(list_db_paths, 'Surveillance Cholera_2015_2016')] %>% 
  str_replace_all(c('data/' = '', '[-_]Surveillance Cholera.*' = ''))

list_dat_post_hurr <- map(list_dept, WrangleDataCTC) %>% set_names(list_dept)
list_hist_day_post_hurr <- map(list_dat_post_hurr, PlotHistDayPostHurrCTC) %>% set_names(list_dept)

