# load("ws.RData")
load('map/maps.RData')
source('functions.R')

## read in different page (CTC - 'Saisie Inst')
list_db_paths <- dir('data', full.names = TRUE)
list_depts <-
  list_db_paths[str_detect(list_db_paths, 'Surveillance Cholera_2015_2016')] %>% 
  str_replace_all(c('data/' = '', '[-_]Surveillance Cholera.*' = ''))

list_depts <- 'DSGA'
# list_depts <- 'DSS'

map_shps <- map(c(hti_adm1, hti_adm3), PrepareMapData) %>% set_names(c('adm1', 'adm3'))

df_nested <-
  list_depts %>% 
  map(WrangleData) %>% 
  set_names(list_depts) %>% 
  bind_rows() %>% 
  nest(-c(dept), .key = data_by_age) %>%
  mutate(
    # data sets
    data = map(data_by_age, MergeAges),
    data_pie = map(data_by_age, WrangleDataPie, variable = 'cas_vus'),
    # data_map = map2(data, dept, WrangleDataMapPopAR),
    data_map = list(data, dept = dept, df_map = list(map_shps$adm3)) %>% pmap(WrangleDataMapPopAR),
    # table
    epitable = map(data, MakeTable),
    # plots
    ## epicurves
    epicurve_by_day_all = map(data, PlotHistDay),
    epicurve_by_week_all = map(data, PlotHistWeek),
    epicurve_by_week_commune = map(data, PlotHistWeekCommune),
    epicurve_by_week_inst = map(data, PlotHistWeekInst),
    ## proportion plots
    pie_cases_by_commune = map(data_pie, PlotPie, colour_scheme = 'RdBu', key = 'commune'),
    bar_cases_by_commune = map(data_pie, PlotBar, colour_scheme = 'RdBu', facet_var = 'commune'),
    ## map
    map_main = list(data_map, dept = dept, df_map = list(hti_adm3), key = 'ar') %>% pmap(MakeMapMain),
    map_inset = list(data_map, dept = dept, df_map = list(hti_adm3)) %>% pmap(MakeMapInset)
  )



## read in WASH data
WrangleDataMapWash <- function(data = NULL) {
  
  library(rgeos)
  library(maptools)
  # maptools::gpclibPermit()
  map_haiti <- rgdal::readOGR(dsn = 'map', layer = 'HTI_adm3')
  map_haiti@data$id <- rownames(map_haiti@data)
  df_map_haiti <- 
    fortify(map_haiti, region = 'id') %>% 
    inner_join(map_haiti@data, by = 'id') %>% 
    tbl_df
  
  # df_commune_sheets <-
  #   data_frame(commune = c('DSGA', 'DSS'), sheet = c('Dept_Grand anse', 'Dept_Sud'))
  # browser()
  dat <- 
    df_map_haiti %>%
    filter(NAME_1 %in% "Grand'Anse") %>% 
    mutate(
      NAME_3 = str_replace_all(NAME_3, "-", ' '),
      NAME_3 = str_replace_all(NAME_3, "é|è", 'e'),
      NAME_3 = str_replace_all(NAME_3, "à|â", 'a'),
      NAME_3 = str_replace_all(NAME_3, " A ", ' a '),
      NAME_3 = str_replace_all(NAME_3, "ô", 'o'),
      NAME_3 = str_replace_all(NAME_3, "Saint", 'St.')
    ) %>% 
    full_join(
      read_excel('data/wash.xlsx', sheet = 'Total', skip = 0) %>% 
        setNames(1:ncol(.)) %>% 
        select(7:18) %>% 
        set_names(c('Marfranc (J)', 'Previle (J)', 'Chambellan', 'St. Antoine (J)', 'Moron', "Anse d'Hainault", 'Les Irois', 'Dame Marie', 'Pestel', 'Abricots', 'Carrefour Sanon (J)', 'Roseaux')) %>% 
        slice(1) %>% 
        t %>% 
        data.frame %>% 
        tibble::rownames_to_column() %>% 
        set_names(c('commune', 'wash_rating')) %>% 
        mutate(commune = ifelse(str_detect(commune, '(J)'), 'Jeremie', commune)) %>% 
        group_by(commune) %>% 
        summarise(wash_rating = mean(wash_rating)),
      by = c('NAME_3' = 'commune'))
  
  return(dat)
  
}

df_map_wash <- map(list_dat, WrangleDataMapWash)

MakeMapWASH <- function(data){
  # browser()
  
  data %>% 
    mutate(
      wash_col = 
        ifelse(wash_rating < .5, '<50%',
               ifelse(wash_rating >= .5 & wash_rating < .75, '50-75%',
                      ifelse(wash_rating >= .75, '>75%', NA)
               )
        )
    ) %>% 
    ggplot() +
    # geom_polygon(aes(long, lat, group = group, fill = wash_rating), colour = 'white', size = .2) +
    # scale_fill_gradient(name = "WASH rating (%)", limits = c(0, 1), low = 'red', high = 'green', na.value = "lightgrey", labels = scales::percent) +
    geom_polygon(aes(long, lat, group = group, fill = wash_col), colour = 'white', size = .2) +
    scale_fill_manual(name = "WASH rating", values = c('<50%' = 'red', '50-75%' = 'orange', '>75%' = 'green'), na.value = "lightgrey") +
    coord_equal() +
    ggthemes::theme_tufte(base_family = 'Palatino') +
    theme(
      legend.title = element_text(size = 6, face = 'bold'),
      legend.text = element_text(size = 5),
      legend.key.size = unit(.4, 'cm'), 
      axis.title = element_text(size = 6, face = 'bold'),
      axis.text = element_text(size = 5),
      axis.ticks = element_blank()
    ) +
    labs(x = 'Longitude', y = 'Latitude')
  
}

list_map_wash <- map(df_map_wash, MakeMapWASH)

df_map_haiti %>% 
  filter(NAME_1 %in% "Grand'Anse") %>% 
  select(NAME_3) %>% 
  unique 
