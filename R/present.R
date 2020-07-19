source("R/package.R")
source("R/help.R")

#################################
## Spatial data
authorities <-
  read_sf("data/authorities.shp") %>%
  clean_names() %>%
  rename(area = st_rshp,
         leng = st_lngt) %>%
  select(code, area, leng) %>%
  filter(str_detect(code, "E|W|S")) %>%
  filter(code != "S12000027") %>%
  ms_simplify(keep = 0.01)  %>%
  st_transform(27700)

background <- 
  authorities %>%
  st_union() %>%
  st_combine()

##

plot(background)

##

box <- 
  background %>%
  st_transform(27700) %>% 
  #st_bbox() %>%
  #st_as_sfc() %>%
  st_make_grid(cellsize = 50000) %>%
  st_as_sf() %>%
  rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname))

grid <-
  box %>%
  st_union() %>%
  st_combine()

##

plot(box)

##

frst_degree <- 
  box %>% 
  st_touches() %>% 
  as_tibble() %>% 
  clean_names()

scnd_degree <- 
  frst_degree %>%
  rename(id = row_id,
         row_id = col_id) %>%
  left_join(frst_degree) %>%
  transmute(id = id,
            rowname = col_id) %>%
  group_by(id) %>%
  distinct(rowname, .keep_all = TRUE) %>%
  ungroup() %>%
  left_join(box) %>%
  st_as_sf()

geom <- st_geometry(scnd_degree)

final <- 
  scnd_degree %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(scnd_degree) %>%
  st_as_sf() %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  mutate(x = abs(X - mean(X)),
         y = abs(Y - mean(Y))) %>%
  mutate(score = (x + y) / 2) %>%
  ungroup() %>% 
  mutate(geometry = geom) %>%
  st_as_sf()

##

windows <- 
  ggplot(final) +
  geom_sf(data = background,
          aes(), 
          fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = box,
          aes(), 
          fill = NA, colour = '#000000', alpha = 0.1, size = 0.5) +
  geom_sf(aes(fill = score), show.legend = FALSE) +
  scale_fill_scico(palette = 'buda', direction = -1) +
  transition_manual(id) +
  ease_aes('linear') +
  theme_map()

anim_save(windows, filename = "windows.gif", fps = 3)

##

expectancy <- 
  read_xls("data/hackathon/healthy.xls", skip = 10, sheet = 1) %>%
  clean_names()

play <- 
  expectancy %>%
  left_join(authorities) %>%
  filter(code != "E06000053") %>%
  rownames_to_column() %>%
  mutate(rowname = as.numeric(rowname)) %>%
  st_as_sf() 
 

frst_degree <- 
  play %>% 
  st_touches() %>% 
  as_tibble() %>% 
  clean_names()

scnd_degree <- 
  frst_degree %>%
  rename(id = row_id,
         row_id = col_id) %>%
  left_join(frst_degree) %>%
  transmute(id = id,
            rowname = col_id) %>%
  group_by(id) %>%
  distinct(rowname, .keep_all = TRUE) %>%
  ungroup() %>%
  left_join(play) %>%
  st_as_sf()

geom <- st_geometry(scnd_degree)

final <- 
  scnd_degree %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(scnd_degree) %>%
  st_as_sf() %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  mutate(x = abs(X - mean(X)),
         y = abs(Y - mean(Y))) %>%
  mutate(score = (x + y) / 2) %>%
  ungroup() %>% 
  mutate(geometry = geom) %>%
  st_as_sf()

##

background <- 
  authorities %>%
  st_union() %>%
  st_combine()

windows <- 
  ggplot(final) +
  geom_sf(data = background,
          aes(), 
          fill = 'grey70', colour = NA, size = 0) +
  geom_sf(aes(fill = score), show.legend = FALSE) +
  scale_fill_scico(palette = 'buda', direction = -1) +
  transition_manual(id) +
  ease_aes('linear') +
  theme_map()

anim_save(windows, filename = "gwr.gif", fps = 3)

names(final)

models <- 
  ggplot(final %>%
           mutate(HLE = (healthy_life_expectancy_for_females_2009_2013_years + healthy_life_expectancy_for_males_2009_2013) / 2),
         aes(x = index_of_multiple_deprivation_imd_score_2015, y = HLE)) +
  geom_point(aes(colour = score), show.legend = FALSE) +
  geom_smooth(method = lm, fullrange = TRUE, colour = 'grey') +
  scale_colour_scico(palette = 'buda', direction = -1) +
  xlab("English Index of Multiple Deprivation") +
  ylab("Healthy Life Expectancy") +
  transition_manual(id) +
  ease_aes('linear') +
  theme_ver()

anim_save(models, filename = "models.gif", fps = 3)




