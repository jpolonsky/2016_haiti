library(tidyverse)
library(stringr)
library(readxl)
library(lubridate)
library(magrittr)
# library(extrafont)

# displays the results from the nested dataframe
DisplayResult <- function(variable) df_nested %>% filter(dept %in% list_depts) %>% extract2(variable) %>% extract2(1)

# standard clean theme for the plots
theme_report <- 
  theme_bw() +
  theme(
    legend.title = element_text(family = 'Palatino', size = 6, face = 'bold'),
    legend.text = element_text(family = 'Palatino', size = 5),
    legend.key.size = unit(.4, 'cm'), 
    axis.title = element_text(family = 'Palatino', size = 6, face = 'bold'),
    axis.text = element_text(family = 'Palatino', size = 5),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(size = .3),
    panel.grid.minor  = element_blank(),
    strip.background = element_rect(fill = 'white'),
    strip.text = element_text(size = 5, family = 'Palatino', face = 'bold')
  )

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
      # date < (today()-1)
      epitools::as.week(date)[['week']] < epitools::as.week(today())[['week']]
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
    # group_by(dept, inst, commune, date, key) %>% 
    group_by(inst, commune, date, key) %>% 
    summarise(value = sum(value, na.rm = TRUE)) %>% 
    ungroup
  
  return(dat)
  
}

PlotHistDay <- function(data = NULL){
  
  data %>% 
    filter(key %in% 'cas_vus') %>% 
    group_by(date) %>% 
    summarise(value = sum(value)) %>% 
  ggplot() +
    geom_col(aes(x = date, y = value)) +
    geom_text(
      data =
        data %>% 
        filter(key %in% 'cas_vus') %>% 
        group_by(date) %>% 
        summarise(total = sum(value)),
      aes(x = date, y = total + max(total)/50, label = total), size = 1
    ) +
    # ggthemes::theme_tufte(base_family = 'Palatino') +
    theme_report +
    theme(panel.border = element_blank()) +
    labs(x = 'Date', y = '# cases')
  
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
  
  dat %>% 
    filter(!inst %in% exclude$inst) %>% 
    ggplot() +
    geom_col(aes(x = date, y = value)) +
    facet_wrap(~ inst) +
    theme_report +
    labs(x = 'Date', y = '# cases')
  
}


PlotHistWeek <- function(data = NULL){
  
  data %>% 
    mutate(epiweek = epitools::as.week(date)[['week']]) %>%
    group_by(inst, commune, epiweek, key) %>%
    summarise(value = sum(value)) %>% 
    ggplot() +
    geom_col(aes(x = epiweek, y = value)) +
    # geom_col(aes(x = epiweek, y = value, fill = commune)) +
    # scale_fill_viridis(discrete = TRUE) +
    geom_text(
      data = 
        data %>% 
        mutate(epiweek = epitools::as.week(date)[['week']]) %>%  
        group_by(epiweek) %>% 
        summarise(total = sum(value)), 
      aes(x = epiweek, y = total + max(total)/50, label = total), size = 1
    ) +
    theme_report +
    theme(
      legend.position = 'none', 
      axis.text.x = element_text(size = 5, angle = 45, family = 'Palatino')
    ) +
    labs(x = 'Epiweek 2016', y = '# cases')
  
}


PlotHistWeekCommune <- function(data = NULL){
  
  data %>%
    filter(key == 'cas_vus') %>% 
    mutate(epiweek = epitools::as.week(date)[['week']]) %>%
    group_by(commune, epiweek) %>%
    summarise(value = sum(value)) %>% 
    ggplot() +
    geom_col(aes(x = epiweek, y = value)) +
    facet_wrap(~ commune) +
    labs(x = 'Epiweek 2016', y = '# cases') +
    theme_report +
    theme(legend.position = 'none')
  
}


PlotHistWeekInst <- function(data = NULL, colour_scheme = 'Spectral'){
  
  dat <- 
    data %>%
    filter(key == 'cas_vus') %>% 
    ungroup() %>% 
    mutate(inst = ifelse(inst == 'UTC', paste(inst, commune), inst)) %>% 
    mutate(epiweek = epitools::as.week(date)[['week']]) %>%
    group_by(commune, inst, epiweek, key) %>%
    summarise(value = sum(value))
  
  dat$inst <- forcats::fct_inorder(dat$inst)
  
  exclude <- 
    dat %>% 
    group_by(inst) %>% 
    summarise(value = sum(value)) %>% 
    filter(value == 0) %>% 
    c
  
  dat %>% 
    filter(!inst %in% exclude$inst) %>% 
    ggplot() +
    geom_col(aes(x = epiweek, y = value, fill = commune), colour = 'black', size = .25) +
    scale_fill_manual(values = colorRampPalette(RColorBrewer::brewer.pal(9, colour_scheme))(length(unique(dat$commune)))) +
    facet_wrap(~ inst) +
    labs(x = 'Epiweek 2016', y = '# cases') +
    theme_report +
    theme(legend.position = 'bottom')
  
}



# Table
MakeTable <- function(data){
  
  tmp <- 
    data %>%
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
    set_names(c('Commune', 'UTC/CTC', 'Cas (%)', 'Décès inst. (%)', 'Décès comm. (%)', 'Total décès'))
  
  
  return(tmp)
  
}


PrepareMapData <- function(map_shp){
  
  map_shp@data$id <- rownames(map_shp@data)
  
  map_shp %>% 
    fortify(region = 'id') %>% 
    inner_join(
      map_shp@data %>% mutate(id = rownames(.)), by = 'id'
    ) %>% 
    tbl_df  
  
}

WrangleDataMapPopAR <- function(data = NULL, dept = NULL, df_map = NULL) {

  df_commune_sheets <-
    data_frame(commune = c('DSGA', 'DSS'), sheet = c('Dept_Grand anse', 'Dept_Sud'))
  
  df_map %>%
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
            # sheet = df_commune_sheets$sheet[df_commune_sheets$commune == unique(data$dept)],
            sheet = df_commune_sheets$sheet[df_commune_sheets$commune == dept],
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
  
}


MakeMapMain <- function(data, dept = NULL, df_map = NULL, key = NULL) {

  df_dept_names <- data_frame(short = c('DSGA', 'DSS'), full = c("Grand'Anse", 'Sud'))
  map_dept <- df_map[df_map@data$NAME_1 %in% df_dept_names[df_dept_names$short %in% dept, 2], ]
  
  commune_names <-
    sp::coordinates(map_dept) %>%
    tbl_df() %>%
    set_names(c('long', 'lat')) %>%
    mutate(label = map_dept@data$NAME_3)
  
  ggplot(data) +
    geom_polygon(aes(long + 0.008, lat - 0.005, group = group), fill = "#9ecae1") +
    geom_polygon(aes_string('long', 'lat', group = 'group', fill = key), colour = 'black', size = .2) +
    geom_text(data = commune_names, aes(long, lat, label = label), family = 'Palatino', fontface = 'bold', size = 1) +
    coord_equal() +
    scale_fill_gradient(name = "TA (#/10,000)", low = 'lightyellow', high = 'darkred', na.value = "lightgrey", labels = scales::comma) +
    theme_report +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
    labs(x = 'Longitude', y = 'Latitude')
  
}


MakeMapInset <- function(data, dept = NULL, df_map = NULL){
  
  df_dept_names <- data_frame(short = c('DSGA', 'DSS'), full = c("Grand'Anse", 'Sud'))
  map_dept <- df_map[df_map@data$NAME_1 %in% df_dept_names[df_dept_names$short == dept, 2], ]
  
  ## define object limits
  ggobj <- 
    map_dept %>% 
    fortify(region = 'id') %>% 
    inner_join(
      map_dept@data %>% mutate(id = rownames(.)), by = 'id'
    ) %>% 
    ggplot() + 
    geom_polygon(aes(long, lat, group = group)) + 
    coord_equal()
  
  xlim <- ggplot_build(ggobj)$layout$panel_ranges[[1]]$x.range
  ylim <- ggplot_build(ggobj)$layout$panel_ranges[[1]]$y.range
  inset_frame <- data_frame(xmin = xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2])
  
  ggplot(map_shps$adm1) + 
    geom_polygon(aes(long, lat, group = group), col = 'black', fill = 'lightgrey', size = .1) +
    geom_rect(
      data = inset_frame, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
      alpha = 0, colour = 'darkred', size = .25, linetype = 2
    ) +
    coord_equal() + 
    theme_report + 
    theme(
      axis.text = element_blank(), 
      axis.title = element_blank(),
      panel.grid = element_blank()
    )
  
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
  tmp[[key]] <- forcats::fct_inorder(tmp[[key]])
  
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
  
  tmp[[key]] %<>% 
    forcats::fct_inorder() %>% 
    forcats::fct_rev()
  
  p <-
    if(is.null(facet_var)) {
      # browser()
      ggplot(tmp) +
        geom_col(
          aes_string(x = 1, y = 'prop', fill = key),
          width = 1, colour = 'white', size = .1
        ) +
        coord_polar('y', start = 0) +
        geom_text(
          aes(x = 1.75, y = tmp$pos, label = tmp$label),
          colour = 'black', family = 'Palatino', size = 1, fontface = 'bold', check_overlap = TRUE
        ) +
        scale_y_continuous(breaks = tmp$pos, labels = NULL) +
        scale_fill_manual(
          values = if(colour_scheme %in% c('viridis', 'Viridis')) viridis::viridis(length(unique(tmp[[key]])))
          else colorRampPalette(RColorBrewer::brewer.pal(9, colour_scheme))(length(unique(tmp[[key]])))
        ) +
        labs(x = '', y = '', title = ifelse(title %in% NULL, NULL, paste0(title))) +
        theme_report +
        theme(
          panel.border = element_blank(),
          legend.key = element_blank(),
          legend.position = '',
          axis.text.y = element_blank(),
          panel.grid  = element_blank()
        )
      
    } else {
      
      ggplot(tmp) +
        geom_col(
          aes_string(x = 1, y = 'prop', fill = key),
          width = 1, colour = 'white', size = .1
        ) +
        coord_polar('y', start = 0) +
        facet_wrap(facet_var) +
        scale_y_continuous(breaks = tmp$pos, labels = NULL) +
        scale_fill_manual(
          values = if(colour_scheme %in% c('viridis', 'Viridis')) viridis::viridis(length(unique(tmp[[key]])))
          else colorRampPalette(RColorBrewer::brewer.pal(9, colour_scheme))(length(unique(tmp[[key]])))
        ) +
        labs(x = '', y = '', title = ifelse(title %in% NULL, NULL, paste0(title))) +
        theme_report +
        theme(
          legend.title = element_text(size = 4),
          legend.text = element_text(size = 3),
          legend.key = element_blank(),
          legend.key.size = unit(.2, 'cm'), 
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          strip.text = element_text(size = 3),
          panel.grid  = element_blank()
        )
      
    }
  
  return(p)
  
}


#' Horizontal bar plots 
#' @export
PlotBar <- function(data, key = 'key', value = 'value', facet_var = NULL, title = NULL, colour_scheme = 'Blues') {
  
  tmp <-
    data %>%
    group_by_(facet_var, key) %>%
    summarise_(value = lazyeval::interp(~ round(sum(var)), var = as.name(value))) %>% 
    mutate(prop = round(value/sum(value), 2))
    
  tmp[[key]] <- forcats::fct_rev(tmp[[key]])
  tmp[[facet_var]] <- forcats::fct_rev(tmp[[facet_var]])
  
  ggplot(tmp) +
    geom_col(aes_string(x = facet_var, y = 'prop', fill = key)) +
    scale_fill_manual(
      values = if(colour_scheme %in% c('viridis', 'Viridis')) viridis::viridis(length(unique(tmp[[key]])))
      else colorRampPalette(RColorBrewer::brewer.pal(9, colour_scheme))(length(unique(tmp[[key]]))),
      guide = guide_legend(reverse = TRUE)
    ) +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() +
    theme_report +
    theme(
      panel.border = element_blank(),
      axis.text.y = element_text(size = 6, face = 'bold'),
      legend.key = element_blank(),
      legend.position = 'bottom',
      legend.title = element_blank()
    ) +
    # guides(guide_legend(reverse = T)) +
    labs(title = '', x = '', y = title)

}  
