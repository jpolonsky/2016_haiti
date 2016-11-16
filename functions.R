WrangleData <- function(dept = NULL){
  
  list_path <- list_db_paths[str_detect(list_db_paths, dept) == TRUE]
  
  sheetnames <- excel_sheets(list_path)
  
  sheet <- sheetnames[str_detect(sheetnames, 'Saisie') == TRUE]
  
  dat_raw <- 
    read_excel(path = list_path, sheet = sheet, skip = 2) %>% 
    setNames(make.names(names(.)))
  
  dat_dates_raw <- 
    read_excel(path = list_path, sheet = sheet, skip = 0) %>% 
    setNames(make.names(names(.)))
  
  dates_for_names <- 
    names(dat_dates_raw)[str_detect(names(dat_dates_raw), 'X')] %>% 
    gsub('X', '', .) %>% 
    as.numeric %>% 
    as.Date(origin = "1899-12-30")
  
  vars_for_names <- c('cas_vus_<5', 'cas_vus_5+', 'cas_hosp_<5', 'cas_hosp_5+', 'deces_inst_<5', 'deces_inst_5+', 'deces_comm_<5', 'deces_comm_5+')
  dates_for_pasting <- rep(dates_for_names, each = length(vars_for_names))
  vars_for_pasting <- rep(vars_for_names, times = length(dates_for_names))
  new_var_names <- paste0(dates_for_pasting, ':', vars_for_pasting)
  dat_names <- c('inst', 'commune', new_var_names)
  
  dat <-
    dat_raw[4:ncol(dat_raw)] %>%
    setNames(dat_names) %>%
    gather(key, value, -c(inst, commune)) 
  
  dat_post_hurr <- 
    dat %>%
    separate(key, c('date', 'key'), ':') %>% 
    mutate(
      inst = str_trim(inst),
      date = ymd(date),
      key = str_replace_all(key, fixed('_<5'), ''),
      key = str_replace_all(key, fixed('_5+'), ''),
      # key = str_replace_all(key, 'deces.*', 'deces'),
      value = as.numeric(value)
    ) %>% 
    filter(
      !is.na(inst),
      inst != 'TOTAL',
      date >= ymd('2016-10-03'),
      date < today()
    ) %>% 
    group_by(inst, commune, date, key) %>% 
    summarise(n = sum(value, na.rm = TRUE))
  
  return(dat_post_hurr)
  
}


PlotHistDay <- function(data = NULL){
  
  ggplot(
    data %>% filter(key %in% 'cas_vus')
    ) +
    geom_bar(aes(x = date, y = n), stat = 'identity') +
    # geom_bar(aes(x = date, y = n, fill = inst), stat = 'identity') +
    # scale_fill_viridis(discrete = TRUE) +
    geom_text(
      data =
        data %>% 
        filter(key %in% 'cas_vus') %>% 
        group_by(date) %>% 
        summarise(total = sum(n)),
      aes(x = date, y = total + max(total)/50, label = total), size = 1
    ) +
    labs(x = 'Date', y = '# cases') +
    ggthemes::theme_tufte(base_family = 'Palatino') +
    # theme_bw() +
    theme(
      axis.title = element_text(size = 6), 
      axis.text = element_text(size = 5),
      axis.ticks = element_blank()
    ) 
  
}


PlotHistDayInst <- function(data = NULL){
  
  dat <- 
    # data %>%
    list_dat[[1]] %>%
    filter(key == 'cas_vus') %>% 
    ungroup() %>% 
    mutate(inst = ifelse(inst == 'UTC', paste(inst, commune), inst)) %>% 
    group_by(inst, date, key) %>%
    summarise(n = sum(n))
  
  exclude <- 
    dat %>% 
    group_by(inst) %>% 
    summarise(n = sum(n)) %>% 
    filter(n == 0) %>% 
    c
  
  ggplot(
    dat %>% filter(!inst %in% exclude$inst)
  ) +
    geom_bar(aes(x = date, y = n), stat = 'identity') +
    facet_wrap(~ inst) +
    labs(x = 'Date', y = '# cases') +
    ggthemes::theme_tufte(base_family = 'Palatino') +
    theme(
      legend.position = 'none', 
      axis.line = element_line(color = "black"),
      axis.title = element_text(size = 6), 
      axis.text.x = element_text(size = 5, angle = 45),
      axis.text.y = element_text(size = 5),
      strip.text = element_text(size = 5),
      axis.ticks = element_blank()
    )
  
}


PlotHistWeek <- function(data = NULL){
  
  tmp <- 
    data %>% 
    mutate(epiweek = epitools::as.week(date)[['week']]) %>%
    group_by(inst, commune, epiweek, key) %>%
    summarise(n = sum(n))
  
  ggplot(tmp) +
    geom_bar(aes(x = epiweek, y = n), stat = 'identity') +
    # geom_bar(aes(x = epiweek, y = n, fill = commune), stat = 'identity') +
    # scale_fill_viridis(discrete = TRUE) +
    geom_text(
      data = tmp %>% group_by(epiweek) %>% summarise(total = sum(n)), 
      aes(x = epiweek, y = total + max(total)/50, label = total), size = 1
    ) +
    labs(x = 'Epiweek 2016', y = '# cases') +
    ggthemes::theme_tufte()
  
}


PlotHistWeekCommune <- function(data = NULL){
  
  dat <- 
    data %>%
    # list_dat_post_hurr[[1]] %>% 
    filter(key == 'cas_vus') %>% 
    mutate(epiweek = epitools::as.week(date)[['week']]) %>%
    group_by(commune, epiweek) %>%
    summarise(n = sum(n))
  
  exclude <- 
    dat %>% 
    group_by(commune) %>% 
    summarise(n = sum(n)) %>% 
    filter(n == 0) %>% 
    c
  
  ggplot(
    dat %>% filter(!commune %in% exclude$commune)
  ) +
    geom_bar(aes(x = epiweek, y = n), stat = 'identity') +
    facet_wrap(~ commune) +
    labs(x = 'Epiweek 2016', y = '# cases') +
    # ggthemes::theme_tufte(base_family = 'Open Sans') +
    # ggthemes::theme_tufte(base_family = 'GillSans') +
    # ggthemes::theme_tufte(base_family = 'Helvetica') +
    ggthemes::theme_tufte(base_family = 'Palatino') +
    # theme_bw() +
    theme(
      legend.position = 'none', 
      axis.line = element_line(color = "black"),
      axis.title = element_text(size = 6), 
      axis.text = element_text(size = 5),
      strip.text = element_text(size = 5),
      axis.ticks = element_blank()
    )
  
}


PlotHistWeekInst <- function(data = NULL){
  
  dat <- 
    data %>%
    # list_dat_post_hurr[[1]] %>%
    filter(key == 'cas_vus') %>% 
    ungroup() %>% 
    mutate(inst = ifelse(inst == 'UTC', paste(inst, commune), inst)) %>% 
    mutate(epiweek = epitools::as.week(date)[['week']]) %>%
    group_by(inst, epiweek, key) %>%
    summarise(n = sum(n))
  
  exclude <- 
    dat %>% 
    group_by(inst) %>% 
    summarise(n = sum(n)) %>% 
    filter(n == 0) %>% 
    c
  
  ggplot(
    dat %>% filter(!inst %in% exclude$inst)
  ) +
    geom_bar(aes(x = epiweek, y = n), stat = 'identity') +
    facet_wrap(~inst) +
    labs(x = 'Epiweek 2016', y = '# cases') +
    theme(legend.position = 'none', axis.line = element_line(color = "black")) +
    ggthemes::theme_tufte() +
    # theme_bw() +
    theme(
      legend.position = 'none', 
      axis.line = element_line(color = "black"),
      axis.title = element_text(size = 6), 
      axis.text = element_text(size = 5),
      strip.text = element_text(size = 5),
      axis.ticks = element_blank()
    )
  
}



# Table
MakeTable <- function(data){
  
  tmp <- 
    data %>%
    # list_dat_post_hurr[[1]] %>%
    filter(!key == 'cas_hosp') %>%
    ungroup() %>% 
    mutate(inst = ifelse(inst == 'UTC', paste(inst, commune), inst)) %>% 
    group_by(commune, inst, key) %>% 
    summarise(n = sum(n)) %>% 
    spread(key, n) %>% 
    ungroup %>% 
    mutate(
      `% cas` = round(cas_vus/sum(cas_vus)*100, 1),
      `% deces_inst` = round(deces_inst/sum(deces_inst)*100, 1),
      `% deces_comm` = round(deces_comm/sum(deces_comm)*100, 1)
    ) %>% 
    select(1:3, 6, 5, 7, 4, 8) %>% 
    bind_rows(
      summarise(.,
                commune = 'Total',
                inst = '-',
                cas_vus = sum(cas_vus, na.rm = TRUE),
                `% cas` = round(cas_vus/sum(cas_vus, na.rm = TRUE)*100, 1),
                deces_comm = sum(deces_comm, na.rm = TRUE),
                `% deces_comm` = round(deces_comm/sum(deces_comm, na.rm = TRUE)*100, 1),
                deces_inst = sum(deces_inst, na.rm = TRUE),
                `% deces_inst` = round(deces_inst/sum(deces_inst, na.rm = TRUE)*100, 1)
      )
    ) %>% 
    mutate(
      deces_tot = deces_inst + deces_comm,
      `% cas` = paste0('(', `% cas`, ')'),
      `% deces_inst` = paste0('(', `% deces_inst`, ')'),
      `% deces_comm` = paste0('(', `% deces_comm`, ')')
    ) %>% 
    unite(cas_vus, cas_vus:`% cas`, sep = ' ') %>% 
    unite(deces_inst, deces_inst:`% deces_inst`, sep = ' ') %>% 
    unite(deces_comm, deces_comm:`% deces_comm`, sep = ' ') %>% 
    # set_names(c('Commune', 'UTC/CTC', 'Cas', '% cas', 'Décès inst.', '% décès inst.', 'Décès comm.', '% décès comm', 'Total décès'))
    set_names(c('Commune', 'UTC/CTC', 'Cas (%)', 'Décès inst. (%)', 'Décès comm. (%)', 'Total décès'))
    
  
  return(tmp)
   
}
