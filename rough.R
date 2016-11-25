# load("ws.RData")
source('functions.R')

## read in different page (CTC - 'Saisie Inst')
list_db_paths <- dir('data', full.names = TRUE)
list_depts <-
  list_db_paths[str_detect(list_db_paths, 'Surveillance Cholera_2015_2016')] %>% 
  str_replace_all(c('data/' = '', '[-_]Surveillance Cholera.*' = ''))

list_depts <- 'DSGA'
# list_depts <- 'DSS'

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
    data_map = map2(data, dept, WrangleDataMapPopAR),
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
    map = map(data_map, MakeMapAR)
  )


theme_report <- 
  theme_bw() +
  theme(
    legend.title = element_text(family = 'Palatino', size = 6, face = 'bold'),
    legend.text = element_text(family = 'Palatino', size = 5),
    legend.key.size = unit(.4, 'cm'), 
    axis.title = element_text(family = 'Palatino', size = 6, face = 'bold'),
    axis.text = element_text(family = 'Palatino', size = 5),
    axis.ticks = element_blank()
  )



# library(rgeos)
# library(maptools)
# # maptools::gpclibPermit()
# map_haiti <- rgdal::readOGR(dsn = 'map', layer = 'HTI_adm3')
# map_haiti@data$id <- rownames(map_haiti@data)
# df_map_haiti <- 
#   map_haiti %>% 
#   fortify(region = 'id') %>% 
#   inner_join(
#     map_haiti@data %>% mutate(id = rownames(.)), by = 'id'
#   ) %>% 
#   tbl_df

hti_adm1 <- raster::getData('GADM', country = 'HTI', level = 1)
hti_adm1@data$id <- rownames(hti_adm1@data)
df_map_haiti <- 
  hti_adm1 %>% 
  fortify(region = 'id') %>% 
  inner_join(
    hti_adm1@data %>% mutate(id = rownames(.)), by = 'id'
  ) %>% 
  tbl_df

hti_adm3 <- raster::getData('GADM', country = 'HTI', level = 3)
hti_adm3@data$id <- rownames(hti_adm3@data)
df_map_dsga <- 
  hti_adm3 %>% 
  fortify(region = 'id') %>% 
  inner_join(
    hti_adm3@data %>% mutate(id = rownames(.)), by = 'id'
  ) %>% 
  tbl_df

map_dsga <- hti_adm3[hti_adm3@data$NAME_1 %in% "Grand'Anse", ]

commune_names <-
  sp::coordinates(map_dsga) %>%
  tbl_df() %>%
  set_names(c('long', 'lat')) %>%
  mutate(label = map_dsga@data$NAME_3)

p1 <-
  DisplayResult('data_map') %>% 
  ggplot() +
  geom_polygon(aes(long + 0.008, lat - 0.005, group = group), fill = "#9ecae1") +
  geom_polygon(aes(long, lat, group = group, fill = ar), colour = 'black', size = .2) +
  # geom_text(data = commune_names, aes(long, lat, label = label), family = 'Palatino', fontface = 'bold', size = 3) +
  coord_equal() +
  scale_fill_gradient(name = "TA (#/10,000)", low = 'lightyellow', high = 'darkred', na.value = "lightgrey", labels = scales::comma) +
  theme_report +
  theme(
    axis.text.y = element_text(angle = 90, hjust = 0.5)
  ) +
  labs(x = 'Longitude', y = 'Latitude')


## define object limits
ggobj <- 
  map_dsga %>% 
  fortify(region = 'id') %>% 
  inner_join(
    map_dsga@data %>% mutate(id = rownames(.)), by = 'id'
  ) %>% 
  ggplot() + 
  geom_polygon(aes(long, lat, group = group)) + 
  coord_equal()

xlim <- ggplot_build(ggobj)$layout$panel_ranges[[1]]$x.range
ylim <- ggplot_build(ggobj)$layout$panel_ranges[[1]]$y.range
inset_frame <- data_frame(xmin = xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2])


p2 <- 
  ggplot(df_map_haiti) + 
  geom_polygon(aes(long, lat, group = group), col = 'black', fill = 'lightgrey', size = .1) +
  geom_rect(
    data = inset_frame, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
    alpha = 0, colour = 'black', size = .5, linetype = 2
  ) +
  coord_equal() + 
  theme_bw() + 
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title = element_blank(),
    panel.grid = element_blank()
    )

png(file = "inset_map.png", w = 2400, h = 1800, res = 300)
grid::grid.newpage()
v1 <- grid::viewport(width = 1, height = 1, x = 0.5, y = 0.5) # plot area for the main map
v2 <- grid::viewport(width = 0.4, height = 0.3, x = 0.7, y = 0.8) # plot area for the inset map
print(p1, vp = v1) 
print(p2, vp = v2)
dev.off()






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

