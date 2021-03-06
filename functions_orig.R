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


AnalyseData <- function(data = NULL, variable = NULL){
  
  data %>%
    bind_rows %>%
    filter(date < today()) %>%
    group_by(dept, commune, date, year, month, epiweek_year, epiweek) %>%
    summarise_(n = lazyeval::interp(~sum(var), var = as.name(variable)))
  
}


# plotting functions
PlotHistYear <- function(data = NULL){
  
  ggplot(data) +
    geom_bar(aes(x = month, y = n, fill = factor(year)), stat = 'identity') +
    scale_fill_viridis(discrete = TRUE) +
    facet_wrap(~year) +
    ggthemes::theme_tufte()
  
}

PlotHistCommune <- function(data = NULL){
  
  ggplot(filter(data, year >= 2015)) +
    geom_bar(aes(x = epiweek_year, y = n, fill = commune), stat = 'identity') +
    scale_fill_viridis(discrete = TRUE) +
    facet_wrap(~commune) +
    ggthemes::theme_tufte()
  
}

# Post-hurricane plots
PlotHistDayPostHurr <- function(data = NULL){
  
  ggplot(data) +
    geom_bar(aes(x = date, y = n), stat = 'identity') +
    # geom_bar(aes(x = date, y = n, fill = commune), stat = 'identity') +
    # scale_fill_viridis(discrete = TRUE) +
    geom_text(
      data = data %>% group_by(date) %>% summarise(total = sum(n)), 
      aes(x = date, y = total + max(total)/50, label = total), size = 1
    ) +
    labs(x = 'Date', y = '# cases') +
    # ggthemes::theme_tufte() +
    theme_bw() +
    theme(
      axis.title = element_text(size = 4), 
      axis.text = element_text(size = 3),
      axis.ticks = element_blank()
    ) 
  
}

PlotHistWeekPostHurr <- function(data = NULL){
  
  ggplot(data) +
    geom_bar(aes(x = epiweek, y = n), stat = 'identity') +
    # geom_bar(aes(x = epiweek, y = n, fill = commune), stat = 'identity') +
    # scale_fill_viridis(discrete = TRUE) +
    geom_text(
      data = data %>% group_by(epiweek) %>% summarise(total = sum(n)), 
      aes(x = epiweek, y = total + max(total)/50, label = total), size = 1
    ) +
    labs(x = 'Epiweek 2016', y = '# cases') +
    ggthemes::theme_tufte()
  
}

PlotHistWeekCommunePostHurr <- function(data = NULL){
  
  ggplot(data) +
    geom_bar(aes(x = epiweek, y = n), stat = 'identity') +
    # geom_bar(aes(x = epiweek, y = n, fill = commune), stat = 'identity') +
    # scale_fill_viridis(discrete = TRUE) +
    geom_text(
      data = data %>% group_by(commune, epiweek) %>% summarise(total = sum(n)), 
      aes(x = epiweek, y = total + max(total)/25, label = total), size = 1
    ) +
    facet_wrap(~commune) +
    labs(x = 'Epiweek 2016', y = '# cases') +
    theme(legend.position = 'none', axis.line = element_line(color = "black")) +
    ggthemes::theme_tufte() 
  # ggthemes::theme_tufte(base_family = 'BemboStd')
  
}
