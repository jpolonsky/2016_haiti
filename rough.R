# load("ws.RData")

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

list_dat <- map(list_depts, WrangleData) %>% set_names(list_depts)

# plot by day
list_hist_day <- map(list_dat, PlotHistDay) %>% set_names(list_depts)

# by day & inst
list_hist_day_inst <- map(list_dat, PlotHistDayInst) %>% set_names(list_depts)

# plot by epiweek
list_hist_week <- map(list_dat, PlotHistWeek) %>% set_names(list_depts)

# by epiweek & inst
list_hist_week_inst <- map(list_dat, PlotHistWeekInst) %>% set_names(list_depts)


# tables
list_table <- map(list_dat, MakeTable) %>% set_names(list_depts)


# maps
library(rgeos)
library(maptools)
maptools::gpclibPermit()
map_haiti <- rgdal::readOGR(dsn = 'map', layer = 'HTI_adm3')
map_haiti@data$id <- rownames(map_haiti@data)
df_map_haiti <- 
  fortify(map_haiti, region = 'id') %>% 
  inner_join(map_haiti@data, by = 'id')
#df_map_haitiDistSL <- df_map_haitiDist %>% filter(NAME_2 %in% c('Port Loko', 'Bombali', 'Tonkolili'))

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

# ggmap::ggmap(ggmap_haiti, darken = 0) + 
ggplot(
  df_map_haiti %>% filter(NAME_1 %in% "Grand'Anse")
) + 
  geom_polygon(
    data = df_map_haiti %>% filter(NAME_1 %in% "Grand'Anse"),
    aes(long, lat, group = group), 
    fill = 'grey', colour = 'white',
    size = .2
  ) +
  coord_equal() +
  ggthemes::theme_tufte(base_family = 'Palatino') +
  viridis::scale_fill_viridis(discrete = TRUE) +
  theme(
    legend.position = 'none', 
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 6), 
    axis.text = element_text(size = 5),
    strip.text = element_text(size = 5),
    axis.ticks = element_blank()
  ) +
  labs(title = "", x = 'Longitude', y = 'Latitude')

# read in population data
pop <-
  read_excel('data/population.xlsx', sheet = 'Dept_Grand anse', skip = 4) %>% 
  select(c(1, 3)) %>% 
  set_names(c('commune', 'pop')) %>% 
  filter(str_detect(commune, 'Commune')) %>% 
  mutate(
    commune = str_replace_all(commune, "[1-9].*Commune (des|d'|de)", '') %>% str_trim(),
    commune = str_replace_all(commune, "Irois", 'Les Irois'),
    pop = str_replace_all(pop, ' ', ''),
    pop = as.numeric(pop)
  )

df_commune_shp <-
  df_map_haiti %>% 
  filter(NAME_1 %in% "Grand'Anse") %>% 
  select(NAME_3) %>% 
  set_names('commune') %>% 
  map_df(unique) %>% 
  mutate(commune = str_replace(commune, "Jérémie", 'Jérémie'))

# anti_join(pop, df_commune_shp, by = 'commune')
# tmp <- inner_join(map_haiti@data, pop, by = c('NAME_3' = 'commune'))

df_map_DSGA <-
  df_map_haiti %>%
  tbl_df %>%
  mutate(NAME_3 = str_replace(NAME_3, "Jérémie", 'Jérémie')) %>% 
  inner_join(pop, by = c('NAME_3' = 'commune'))


ggplot(df_map_DSGA) +
  geom_polygon(aes(long, lat, group = group, fill = pop), colour = 'white', size = .2) +
  coord_equal() +
  ggthemes::theme_tufte(base_family = 'Palatino') +
  scale_fill_gradient(name = '# cases', low = 'lightyellow', high = 'darkred', labels = scales::comma) +
  # viridis::scale_fill_viridis(labels = scales::comma) +
  theme(
    legend.title = element_text(size = 6, face = 'bold'),
    legend.text = element_text(size = 5),
    axis.title = element_text(size = 6, face = 'bold'),
    axis.text = element_text(size = 5),
    axis.ticks = element_blank()
  ) +
  labs(x = 'Longitude', y = 'Latitude')

  
