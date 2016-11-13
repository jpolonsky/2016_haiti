library(readxl)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(viridis)

# list data sheets to be read in (all communes per dept)
list_db_paths <- dir('data', full.names = TRUE)
list_dept <- c('DSGA', 'DSS')

WrangleData <- function(dept = NULL){
  
  list_path <- list_db_paths[str_detect(list_db_paths, dept) == TRUE]
  
  sheetnames <- excel_sheets(list_path)
  
  exclude_sheets <- 
    c(
      sheetnames[str_detect(sheetnames, 'Sheet') == TRUE],
      sheetnames[str_detect(sheetnames, 'List') == TRUE],
      sheetnames[str_detect(sheetnames, 'DS') == TRUE],
      sheetnames[str_detect(sheetnames, 'Inst') == TRUE],
      'Ouverture', 'A', 'Z', 'BTS', "Taux d'Attaque", 'Report_staging', 'Tableaux_Tests Cholera'
    )
  
  list_commune <- sheetnames[!sheetnames %in% exclude_sheets]
  
  PrepareData <- function(dept = NULL, commune = 'Baraderes') {
    
    dat <- 
      read_excel(path = list_path, sheet = commune, skip = 8) %>% 
      setNames(make.names(names(.)))
    
    dat_names <- 
      c('date', 'epiweek', 'cas_vus_<5', 'cas_vus_5+', 'cas_vus_tot', 'cas_hosp_<5', 'cas_hosp_5+', 'cas_hosp_tot', 'deces_inst_<5', 'deces_inst_5+', 'deces_inst_tot', 'deces_comm_<5', 'deces_comm_5+', 'deces_comm_tot', 'deces_tot_no_age', 'deces_tot')
    
    dat <-
      dat[1:16] %>%
      setNames(dat_names) %>%
      mutate(
        dept = dept,
        commune = commune,
        year = year(date),
        month = month(date, label = TRUE),
        week_year = paste(year(date), ifelse(nchar(isoweek(date)) == 1, paste0('0', isoweek(date)), isoweek(date)), sep = '-'),
        epiweek_year = paste(year(date), ifelse(nchar(epiweek) == 1, paste0('0', epiweek), epiweek), sep = '-')
      ) %>%
      select(dept, commune, date, month, week_year, epiweek_year, everything())

    return(dat)
    
  }
  
  list_dat <- map(list_commune, PrepareData, dept = dept) %>% set_names(list_commune) %>% bind_rows
  return(list_dat)
  
}
  
list_dat <- map(list_dept[1], WrangleData)

# dat <- 
#   list_dat %>% 
#   bind_rows %>% 
#   filter(date < today()) %>% 
#   group_by(dept, commune, date, year, month, epiweek_year, epiweek) %>% 
#   summarise(n = sum(cas_vus_tot))

AnalyseData <- function(data = NULL, variable = NULL){

  data %>%
    bind_rows %>%
    filter(date < today()) %>%
    group_by(dept, commune, date, year, month, epiweek_year, epiweek) %>%
    summarise_(n = lazyeval::interp(~sum(var), var = as.name(variable)))
    
}

list_variables <- c('cas_vus_tot', 'cas_hosp_tot', 'deces_inst_tot', 'deces_comm_tot', 'deces_tot')

# tmp <- AnalyseData(data = list_dat, variable = 'cas_hosp_tot')

list_dat <- map(list_variables, AnalyseData, data = list_dat) %>% set_names(list_variables)

# dat <- bind_rows(tmp, .id = 'variable')

# plots

PlotHistYear <- function(data = NULL){

  ggplot(data) +
    geom_bar(aes(x = month, y = n, fill = factor(year)), stat = 'identity') +
    scale_fill_viridis(discrete = TRUE) +
    facet_wrap(~year) +
    ggthemes::theme_tufte()
  
}

list_hist_year <- map(list_dat, PlotHistYear)
  
PlotHistCommune <- function(data = NULL){
  
  ggplot(filter(data, year >= 2015)) +
    geom_bar(aes(x = epiweek_year, y = n, fill = commune), stat = 'identity') +
    scale_fill_viridis(discrete = TRUE) +
    facet_wrap(~commune) +
    ggthemes::theme_tufte()
  
}

list_hist_commune <- map(list_dat, PlotHistCommune)

# since the hurricane
list_dat_post_hurr <- map(list_dat, function(data) data %>% filter(year == '2016', epiweek >= 40) %>% mutate(epiweek = factor(epiweek)))

# by day
PlotHistDayPostHurr <- function(data = NULL){
  
  ggplot(data, aes(x = date, y = n, fill = commune)) +
    geom_bar(stat = 'identity') +
    # geom_text(label = dat_post_hurr %>% group_by(date) %>% summarise(n = sum(n))) +
    scale_fill_viridis(discrete = TRUE) +
    labs(x = 'Date', y = '# cases') +
    ggthemes::theme_tufte()
  
}

list_hist_day_post_hurr <- map(list_dat_post_hurr, PlotHistDayPostHurr)

# by week
PlotHistWeekPostHurr <- function(data = NULL){
  
  ggplot(data, aes(x = date, y = n, fill = commune)) +
    geom_bar(aes(x = epiweek, y = n, fill = commune), stat = 'identity') +
    scale_fill_viridis(discrete = TRUE) +
    labs(x = 'Epiweek 2016', y = '# cases') +
    ggthemes::theme_tufte()
  
}

list_hist_week_post_hurr <- map(list_dat_post_hurr, PlotHistWeekPostHurr)

# by week & commune
PlotHistWeekCommunePostHurr <- function(data = NULL){
  
  ggplot(data) +
    geom_bar(aes(x = epiweek, y = n, fill = commune), stat = 'identity') +
    scale_fill_viridis(discrete = TRUE) +
    facet_wrap(~commune) +
    labs(x = 'Epiweek 2016', y = '# cases') +
    ggthemes::theme_tufte()
  
}

list_hist_week_commune_post_hurr <- map(list_dat_post_hurr, PlotHistWeekCommunePostHurr)
