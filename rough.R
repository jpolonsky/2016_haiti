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

# by week & inst
list_hist_week_inst <- map(list_dat, PlotHistWeekInst)


# tables
list_table <- map(list_dat, MakeTable)


# maps
library(rgeos)
library(maptools)
# maptools::gpclibPermit()
map_haiti <- rgdal::readOGR(dsn = 'map', layer = 'HTI_adm3')
map_haiti@data$id <- rownames(map_haiti@data)
df_map_haiti <- 
  fortify(map_haiti, region = 'id') %>% 
  inner_join(map_haiti@data, by = 'id')

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

# ## Get maps
# ## Define object limits
# ggobj <- ggplot(df_map_haiti) + geom_polygon(aes(long, lat, group = group)) 
# 
# xlim <- ggplot_build(ggobj)$panel$ranges[[1]]$x.range
# ylim <- ggplot_build(ggobj)$panel$ranges[[1]]$y.range
# 
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

# read in population data
df_map <- map(list_dat, WrangleDataMapPopAR)
list_map <- map(df_map, MakeMapAR)


# pie charts
df_pie_cas_vus <- map(list_dat_raw, WrangleDataPie, variable = 'cas_vus')
# df_pie_deces_inst <- map(list_dat_raw, WrangleDataPie, variable = 'deces_inst')

list_pie_age <- map(df_pie_cas_vus, PlotPie, colour_scheme = 'RdBu', facet_var = 'commune')
map(df_pie_cas_vus, PlotPie, colour_scheme = 'viridis')
map(df_pie_cas_vus, PlotPie, colour_scheme = 'RdBu', key = 'commune')

