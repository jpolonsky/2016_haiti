library(readxl)
library(stringr)
library(dplyr)
library(magrittr)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)
# library(viridis)
library(extrafont)

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
    gather(key, value, -c(inst, commune)) %>% 
    separate(key, c('date', 'key'), ':') %>% 
    mutate(
      dept = dept,
      inst = str_trim(inst),
      date = ymd(date),
      value = as.numeric(value)
    ) %>% 
    filter(
      !is.na(inst),
      inst != 'TOTAL',
      date >= ymd('2016-10-03'),
      date < today()
    ) %>% 
    group_by(dept, inst, commune, date, key) %>% 
    summarise(value = sum(value, na.rm = TRUE)) %>% 
    ungroup
  
  return(dat)
  
}

MergeAges <- function(data = NULL){
  
  dat <- 
    data %>%
    mutate(
      key = str_replace_all(key, fixed('_<5'), ''),
      key = str_replace_all(key, fixed('_5+'), '')
      # key = str_replace_all(key, 'deces.*', 'deces')
    ) %>% 
    group_by(dept, inst, commune, date, key) %>% 
    summarise(value = sum(value, na.rm = TRUE)) %>% 
    ungroup
  
  return(dat)
  
}

PlotHistDay <- function(data = NULL){
  
  ggplot(
    data %>% filter(key %in% 'cas_vus')
  ) +
    geom_bar(aes(x = date, y = value), stat = 'identity') +
    # geom_bar(aes(x = date, y = value, fill = inst), stat = 'identity') +
    # scale_fill_viridis(discrete = TRUE) +
    geom_text(
      data =
        data %>% 
        filter(key %in% 'cas_vus') %>% 
        group_by(date) %>% 
        summarise(total = sum(value)),
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
    summarise(value = sum(value))
  
  exclude <- 
    dat %>% 
    group_by(inst) %>% 
    summarise(value = sum(value)) %>% 
    filter(value == 0) %>% 
    c
  
  ggplot(
    dat %>% filter(!inst %in% exclude$inst)
  ) +
    geom_bar(aes(x = date, y = value), stat = 'identity') +
    facet_wrap(~ inst) +
    labs(x = 'Date', y = '# cases') +
    # ggthemes::theme_tufte(base_family = 'Palatino') +
    theme_bw() +
    theme(
      legend.position = 'none', 
      axis.line = element_line(color = "black"),
      axis.title = element_text(size = 6, family = 'Palatino', face = 'bold'), 
      axis.text.x = element_text(size = 5, angle = 45, family = 'Palatino'),
      axis.text.y = element_text(size = 5, family = 'Palatino'),
      panel.grid  = element_blank(),
      strip.text = element_text(size = 5, family = 'Palatino', face = 'bold'),
      axis.ticks = element_blank()
    )
  
}


PlotHistWeek <- function(data = NULL){
  
  tmp <- 
    data %>% 
    mutate(epiweek = epitools::as.week(date)[['week']]) %>%
    group_by(inst, commune, epiweek, key) %>%
    summarise(value = sum(value))
  
  ggplot(tmp) +
    geom_bar(aes(x = epiweek, y = value), stat = 'identity') +
    # geom_bar(aes(x = epiweek, y = value, fill = commune), stat = 'identity') +
    # scale_fill_viridis(discrete = TRUE) +
    geom_text(
      data = tmp %>% group_by(epiweek) %>% summarise(total = sum(value)), 
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
    summarise(value = sum(value))
  
  # exclude <- 
  #   dat %>% 
  #   group_by(commune) %>% 
  #   summarise(value = sum(value)) %>% 
  #   filter(value == 0) %>% 
  #   c
  
  ggplot(
    # dat %>% filter(!commune %in% exclude$commune)
    dat
  ) +
    geom_bar(aes(x = epiweek, y = value), stat = 'identity') +
    facet_wrap(~ commune) +
    labs(x = 'Epiweek 2016', y = '# cases') +
    # ggthemes::theme_tufte(base_family = 'Open Sans') +
    # ggthemes::theme_tufte(base_family = 'GillSans') +
    # ggthemes::theme_tufte(base_family = 'Helvetica') +
    # ggthemes::theme_tufte(base_family = 'Palatino') +
    theme_bw() +
    theme(
      legend.position = 'none', 
      axis.line = element_line(color = "black"),
      axis.title = element_text(size = 6, family = 'Palatino', face = 'bold'), 
      axis.text = element_text(size = 5, family = 'Palatino'),
      panel.grid  = element_blank(),
      strip.text = element_text(size = 5, family = 'Palatino', face = 'bold'),
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
    summarise(value = sum(value))
  
  exclude <- 
    dat %>% 
    group_by(inst) %>% 
    summarise(value = sum(value)) %>% 
    filter(value == 0) %>% 
    c
  
  ggplot(
    dat %>% filter(!inst %in% exclude$inst)
  ) +
    geom_bar(aes(x = epiweek, y = value), stat = 'identity') +
    facet_wrap(~inst) +
    labs(x = 'Epiweek 2016', y = '# cases') +
    theme(legend.position = 'none', axis.line = element_line(color = "black")) +
    # ggthemes::theme_tufte() +
    theme_bw() +
    theme(
      legend.position = 'none', 
      axis.line = element_line(color = "black"),
      axis.title = element_text(size = 6, family = 'Palatino', face = 'bold'), 
      axis.text = element_text(size = 5, family = 'Palatino'),
      panel.grid  = element_blank(),
      strip.text = element_text(size = 5, family = 'Palatino', face = 'bold'),
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
    summarise(value = sum(value)) %>% 
    spread(key, value) %>% 
    ungroup %>% 
    mutate(
      `% cas` = round(cas_vus/sum(cas_vus)*100, 1) %>% ifelse(is.nan(.), 0, .),
      `% deces_inst` = round(deces_inst/sum(deces_inst)*100, 1) %>% ifelse(is.nan(.), 0, .),
      `% deces_comm` = round(deces_comm/sum(deces_comm)*100, 1) %>% ifelse(is.nan(.), 0, .)
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


WrangleDataMapPopAR <- function(data = NULL) {
  # browser()
  library(rgeos)
  library(maptools)
  # maptools::gpclibPermit()
  map_haiti <- rgdal::readOGR(dsn = 'map', layer = 'HTI_adm3')
  map_haiti@data$id <- rownames(map_haiti@data)
  df_map_haiti <- 
    fortify(map_haiti, region = 'id') %>% 
    inner_join(map_haiti@data, by = 'id') %>% 
    tbl_df
  
  df_commune_sheets <-
    data_frame(commune = c('DSGA', 'DSS'), sheet = c('Dept_Grand anse', 'Dept_Sud'))
  
  dat <- 
    df_map_haiti %>%
    mutate(
      NAME_3 = str_replace_all(NAME_3, "-", ' '),
      NAME_3 = str_replace_all(NAME_3, "é|è", 'e'),
      NAME_3 = str_replace_all(NAME_3, "à|â", 'a'),
      NAME_3 = str_replace_all(NAME_3, " A ", ' a '),
      NAME_3 = str_replace_all(NAME_3, "ô", 'o'),
      NAME_3 = str_replace_all(NAME_3, "Saint", 'St.')
    ) %>% 
    inner_join(
      data %>% 
        mutate(commune = str_replace_all(commune, "-", ' ')) %>% 
        group_by(commune) %>% 
        summarise(cases = sum(value)) %>% 
        full_join(    
          read_excel(
            'data/population.xlsx', 
            sheet = df_commune_sheets$sheet[df_commune_sheets$commune == unique(data$dept)],
            skip = 4) %>% 
            select(c(1, 3)) %>% 
            set_names(c('commune', 'pop')) %>% 
            filter(str_detect(commune, 'Commune')) %>% 
            mutate(
              commune = str_replace_all(commune, "-", ' '), 
              commune = str_replace_all(commune, ".*Commune (des|d'|de)", '') %>% str_trim(),
              commune = str_replace_all(commune, "é|è", 'e'),
              commune = str_replace_all(commune, "à|â", 'a'),
              commune = str_replace_all(commune, " A ", ' a '),
              commune = str_replace_all(commune, "ô", 'o'),
              commune = str_replace_all(commune, "Cayes", 'Les Cayes'),
              commune = str_replace_all(commune, "Irois", 'Les Irois'),
              commune = str_replace_all(commune, "Anglais", 'Les Anglais'),
              commune = str_replace_all(commune, "l'Île", 'Ile'),
              commune = str_replace_all(commune, "Saint", 'St.'),
              pop = str_replace_all(pop, ' ', ''),
              pop = as.numeric(pop)
            ), 
          by = 'commune') %>% 
        mutate(ar = cases/pop*10000), 
      by = c('NAME_3' = 'commune'))
  
  return(dat)
  
}


MakeMapAR <- function(data){
  
  ggplot(data) +
    geom_polygon(aes(long, lat, group = group, fill = ar), colour = 'white', size = .2) +
    coord_equal() +
    ggthemes::theme_tufte(base_family = 'Palatino') +
    scale_fill_gradient(name = "TA (#/10,000)", low = 'lightyellow', high = 'darkred', na.value = "lightgrey", labels = scales::comma) +
    # viridis::scale_fill_viridis(labels = scales::comma) +
    theme(
      legend.title = element_text(size = 6, face = 'bold'),
      legend.text = element_text(size = 5),
      axis.title = element_text(size = 6, face = 'bold'),
      axis.text = element_text(size = 5),
      axis.ticks = element_blank()
    ) +
    labs(x = 'Longitude', y = 'Latitude')
  
}


# pie charts
WrangleDataPie <- function(data = NULL, variable = NULL){
  
  data %>% 
    filter(str_detect(key, variable)) %>% 
    mutate(key = str_replace_all(key, paste0(variable, '_'), ''))
  
}  

#' Pie plot
#' @export
PlotPie <- function(data, key = 'key', value = 'value', colour_scheme = 'Blues', title = NULL, facet_var = NULL, retain.order = F){
  
  tmp <-
    if(is.null(facet_var)) {
      
      eval(substitute(
        data %>%
          group_by_(key) %>%
          summarise(value_sum = round(sum(colname, na.rm = T))),
        list(colname = as.symbol(value)))
      ) %>%
        mutate(prop = round(value_sum/sum(value_sum) * 100, 1))
      
    } else {
      
      eval(substitute(
        data %>%
          group_by_(facet_var, key) %>%
          summarise(value_sum = round(sum(colname, na.rm = T))),
        list(colname = as.symbol(value)))
      ) %>%
        group_by_(facet_var) %>%
        mutate(prop = round(value_sum/sum(value_sum) * 100, 1))
      
    }
  
  tmp <- if (retain.order == T) tmp else arrange(tmp, desc(prop))
  
  if (length(unique(tmp[[key]])) > 10) {

    tmp1 <- tmp[1:7, ]

    tmp2 <-
      tmp %>% 
      slice(8:nrow(tmp)) %>%
      summarise(value_sum = sum(value_sum)) %>%
      mutate(prop = round(value_sum/sum(tmp$value_sum) * 100, 1))

    # browser()
    tmp1[8, 1] <- 'Other'
    
    tmp1[8, (ncol(tmp1)-1):ncol(tmp1)] <- tmp2
      
    tmp <- tmp1 %>% mutate(pos = cumsum(prop) - 0.5 * prop)

    tmp[[key]] <-
      factor(tmp[[key]],
             levels = c(tmp[[key]][nrow(tmp)],
                        tmp[[key]][-nrow(tmp)][order(tmp$prop[-nrow(tmp)])]))

  } else {
  
  tmp <- mutate(tmp, pos = cumsum(prop) - 0.5 * prop)
  tmp[[key]] <- factor(tmp[[key]], levels = unique(tmp[[key]]))
  
  }
  
  tmp$char_lab <- as.character(tmp[[key]])
  
  TwoLines <- function(x) {
    if (nchar(x) > 10) {
      x <- gsub(' or ', '/\n', x)
      x <- gsub(' ', '\n', x)
    } else x
    return(x)
  }
  
  tmp$char_lab <- sapply(as.list(tmp$char_lab), TwoLines)
  
  for(i in 1:nrow(tmp)) tmp$label[i] <- paste0(tmp[i, 'char_lab'], '\n', tmp[i, 'prop'], '%')
  
  tmp[[key]] <- factor(tmp[[key]], levels = unique(tmp[[key]]))
  
  p <-
    if(is.null(facet_var)) {

      ggplot(tmp) +
        geom_bar(
          aes_string(x = 1, y = 'prop', fill = key),
          width = 1, stat = 'identity', colour = 'white'
        ) +
        coord_polar('y', start = 0) +
        geom_text(
          aes(x = 1.75, y = tmp$pos, label = tmp$label),
          colour = 'black', family = 'Palatino', size = 4, check_overlap = TRUE
        ) +
        scale_y_continuous(breaks = tmp$pos, labels = NULL) +
        scale_fill_manual(
          values = if(colour_scheme %in% c('viridis', 'Viridis')) viridis::viridis(length(unique(tmp[[key]])))
          else colorRampPalette(RColorBrewer::brewer.pal(9, colour_scheme))(length(unique(tmp[[key]])))
        ) +
        ggthemes::theme_tufte(base_family = 'Palatino') +
        theme(
          panel.border = element_blank(),
          plot.title = element_text(size = 12, face = 'bold', color = 'darkblue'),
          legend.key = element_blank(),
          legend.position = '',
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid  = element_blank()
        ) +
        labs(x = '', y = '', title = ifelse(title %in% NULL, NULL, paste0(title)))
      
    } else {
      
      ggplot(tmp) +
        geom_bar(
          aes_string(x = 1, y = 'prop', fill = key),
          width = 1, stat = 'identity', colour = 'white'
        ) +
        coord_polar('y', start = 0) +
        facet_wrap(facet_var) +
        scale_y_continuous(breaks = tmp$pos, labels = NULL) +
        scale_fill_manual(
          values = if(colour_scheme %in% c('viridis', 'Viridis')) viridis::viridis(length(unique(tmp[[key]])))
          else colorRampPalette(RColorBrewer::brewer.pal(9, colour_scheme))(length(unique(tmp[[key]])))
        ) +
        # ggthemes::theme_tufte(base_family = 'Palatino') +
        theme_bw() +
        theme(
          plot.title = element_text(size = 12, face = 'bold', color = 'darkblue'),
          legend.title = element_text(family = 'Palatino'),
          legend.text = element_text(family = 'Palatino'),
          legend.key = element_blank(),
          # legend.position = '',
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          strip.text = element_text(size = 5, family = 'Palatino', face = 'bold'),
          # panel.border = element_blank(),
          panel.grid  = element_blank()
        ) +
        labs(x = '', y = '', title = ifelse(title %in% NULL, NULL, paste0(title)))
      
    }
  
  return(p)
  
}

