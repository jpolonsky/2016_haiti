# load("ws.RData")

source('functions.R')

# # list data sheets to be read in (all communes per dept)
# source('functions_orig.R')
# 
# list_db_paths <- dir('data', full.names = TRUE)
# list_depts <- c('DSGA', 'DSS')
# 
# list_dat <- map(list_depts[1], WrangleData)
# 
# list_variables <- c('cas_vus_tot', 'cas_hosp_tot', 'deces_inst_tot', 'deces_comm_tot', 'deces_tot')
# 
# list_dat <- map(list_variables, AnalyseData, data = list_dat) %>% set_names(list_variables)
# 
# list_hist_year <- map(list_dat, PlotHistYear)
# list_hist_commune <- map(list_dat, PlotHistCommune)
# 
# # since the hurricane
# list_dat <- 
#   map(list_dat, function(data) data %>% filter(year == '2016', epiweek >= 40) %>% mutate(epiweek = factor(epiweek)))
# 
# # by day
# list_hist_day <- map(list_dat, PlotHistDay)
# 
# # by week
# list_hist_week <- map(list_dat, PlotHistWeek)
# 
# # by week & commune
# list_hist_week_commune <- map(list_dat, PlotHistWeekCommune)


## read in different page (CTC - 'Saisie Inst')
list_db_paths <- dir('data', full.names = TRUE)
list_depts <-
  list_db_paths[str_detect(list_db_paths, 'Surveillance Cholera_2015_2016')] %>% 
  str_replace_all(c('data/' = '', '[-_]Surveillance Cholera.*' = ''))

list_depts <- 'DSGA'
# list_depts <- 'DSS'

list_dat_raw <- map(list_depts, WrangleData) %>% set_names(list_depts)
list_dat <- map(list_dat_raw, MergeAges)

# plot by day
list_hist_day <- map(list_dat, PlotHistDay)

# by day & inst
list_hist_day_inst <- map(list_dat, PlotHistDayInst)

# plot by epiweek
list_hist_week <- map(list_dat, PlotHistWeek)

# by week & commune
list_hist_week_commune <- map(list_dat, PlotHistWeekCommune)

# by day & inst
list_hist_day_inst <- map(list_dat, PlotHistDayInst)


# tables
list_table <- map(list_dat, MakeTable)

# pie charts
df_pie_cas_vus <- map(list_dat_raw, WrangleDataPie, variable = 'cas_vus')
# df_pie_deces_inst <- map(list_dat_raw, WrangleDataPie, variable = 'deces_inst')

list_pie_age <- map(df_pie_cas_vus, PlotPie, colour_scheme = 'RdBu', facet_var = 'commune')
map(df_pie_cas_vus, PlotPie, colour_scheme = 'viridis', retain.order = T)
map(df_pie_cas_vus, PlotPie, colour_scheme = 'RdBu', key = 'commune', retain.order = F)

# horizontal bar chart
map(df_pie_cas_vus, PlotBar, colour_scheme = 'RdBu', facet_var = 'commune')


# maps
## read in population data
df_map <- map(list_dat, WrangleDataMapPopAR)
list_map <- map(df_map, MakeMapAR)

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



# library(rgeos)
# library(maptools)
# # maptools::gpclibPermit()
# map_haiti <- rgdal::readOGR(dsn = 'map', layer = 'HTI_adm3')
# map_haiti@data$id <- rownames(map_haiti@data)
# df_map_haiti <- 
#   fortify(map_haiti, region = 'id') %>% 
#   inner_join(map_haiti@data, by = 'id') %>% 
#   tbl_df
# 
# ## Define owin window
# spatstat::spatstat.options(checkpolygons = F, npixel = 500)
# w <- 
#   spatstat::owin(
#     poly = 
#       data_frame(x = df_map_haiti$long, y = df_map_haiti$lat) %>% 
#       slice(-nrow(.)) %>% 
#       map(rev)
#   )
# spatstat::spatstat.options(checkpolygons = T)
# 
# ## define object limits
# ggobj <- ggplot(df_map_haiti) + geom_polygon(aes(long, lat, group = group)) 
# 
# xlim <- ggplot_build(ggobj)$panel$ranges[[1]]$x.range
# ylim <- ggplot_build(ggobj)$panel$ranges[[1]]$y.range
# 
# ## get map tile
# ggmap_haiti <- ggmap::get_map(location = c(xlim[1], ylim[1], xlim[2], ylim[2]), maptype = 'satellite')
# 
# ggmap::ggmap(ggmap_haiti, darken = 0) +
#   geom_polygon(
#     data = df_map_haiti %>% filter(NAME_1 %in% "Grand'Anse"),
#     aes(long, lat, group = group), 
#     fill = 'grey', colour = 'white',
#     size = .2
#   ) +
#   coord_equal()

