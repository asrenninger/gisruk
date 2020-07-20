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
  scale_fill_scico(palette = 'buda', direction = 1) +
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
  st_as_sf() 

play <- 
  play %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(play) %>%
  arrange(Y) %>%
  rownames_to_column() %>%
  mutate(rowname = as.numeric(rowname)) %>%
  select(-X, -Y) %>%
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
  scale_y_continuous(breaks = c(55, 60, 65, 70), limits = c(50, 75)) +
  xlab("English Index of Multiple Deprivation") +
  ylab("Healthy Life Expectancy") +
  transition_manual(id) +
  ease_aes('linear') +
  theme_ver()

anim_save(models, filename = "models.gif", fps = 3)

##

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
            row_id = col_id) %>%
  left_join(frst_degree) %>%
  transmute(id = id,
            row_id = col_id) %>%
  left_join(frst_degree) %>%
  transmute(id = id,
            rowname = col_id) %>%
  group_by(id) %>%
  distinct(rowname, .keep_all = TRUE) %>%
  ungroup() %>%
  left_join(play) %>%
  st_as_sf()

scnd_degree %>% st_drop_geometry() %>% group_by(id) %>% summarise(n = n ())
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

anim_save(windows, filename = "gwr_larger.gif", fps = 3)

names(final)

models <- 
  ggplot(final %>%
           mutate(HLE = (healthy_life_expectancy_for_females_2009_2013_years + healthy_life_expectancy_for_males_2009_2013) / 2),
         aes(x = index_of_multiple_deprivation_imd_score_2015, y = HLE)) +
  geom_point(aes(colour = score), show.legend = FALSE) +
  geom_smooth(method = lm, fullrange = TRUE, colour = 'grey') +
  scale_colour_scico(palette = 'buda', direction = -1) +
  scale_y_continuous(breaks = c(55, 60, 65, 70), limits = c(50, 75)) +
  xlab("English Index of Multiple Deprivation") +
  ylab("Healthy Life Expectancy") +
  transition_manual(id) +
  ease_aes('linear') +
  theme_ver()

anim_save(models, filename = "models_larger.gif", fps = 3)

##

left <-
  data %>%
  mutate(HLE = (healthy_life_expectancy_for_females_2009_2013_years + healthy_life_expectancy_for_males_2009_2013) / 2,
         pollution = scale(no2)[, 1] + scale(pm10)[, 1] + scale(so2)[, 1],
         income = (income_2016 - income_2006) / (income_2016 + income_2006)) %>%
  select(code, region, lower_tier_local_authority, HLE, everything()) %>%
  filter(code != "E06000053") %>%
  mutate_if(is.numeric, rescale)


vars <- str_split("pollution + unemployment + income + gamb_d + ffood_d + pubs2_d + tobac_d + green900 + leis_d + ed_d + pharm_d + gpp_d + dent_d",
                  " \\+ ")

y <- left$HLE
x <- 
  left %>%
  st_drop_geometry() %>%
  select(vars[[1]]) %>%
  as.matrix()

##

library(glmnet)

##

ids <- 
  final %>% 
  pull(id) %>% 
  unique()

length(unique(test$code))

spatial <- 
  final %>%
  select(code, id, rowname) %>% 
  st_drop_geometry() %>%
  left_join(left) %>%
  st_as_sf() %>%
  select(HLE, id, vars[[1]]) 

regress <- st_drop_geometry(spatial)
  
lambda <- c()

ranks <- tibble()

vimp <- function(object, lambda = NULL, ...) {
  
  beta <- predict(object, s = lambda, type = "coef")
  if(is.list(beta)) {
    out <- do.call("cbind", lapply(beta, function(x) x[,1]))
    out <- as.data.frame(out)
  } else out <- data.frame(Overall = beta[,1])
  out <- abs(out[rownames(out) != "(Intercept)",,drop = FALSE])
  out
}


for (i in 1:length(ids)) {
  df <- filter(regress, id == i)
  y <- df$HLE
  x <- as.matrix(df[, 3:ncol(df)])
  
  try(fit <- cv.glmnet(x, y, alpha = 1, type.measure = "mse", nfolds = 20))
  iteration <- glue("{i} : {fit$lambda.min}")
  print(iteration)
  lambda <- c(lambda, fit$lambda.min)
  
  imp <- vimp(fit, lambda = fit$lambda.min)
  
  ranks <- 
    imp %>% 
    rownames_to_column() %>% 
    as_tibble() %>% 
    rename(variable = rowname) %>%
    mutate(iteration = i) %>%
    bind_rows(ranks)
  
}

pred <- predict(fit, s = "lambda.min", type = 'coef')

betas <- 
  ranks %>%
  clean_names() %>%
  pivot_wider(names_from = variable, values_from = overall)

ranks %>%
  clean_names() %>%
  filter(overall == 0) %>%
  group_by(variable) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

ggplot(ranks %>% 
         clean_names() %>%
         group_by(variable) %>%
         summarise(n = n(),
                   m = mean(overall)) %>%
         ungroup() %>%
         rename(var = variable) %>%
         mutate(var = str_replace_all(var, pattern = "_d", replacement = " (d)"),
                var = str_remove(var, "900"),
                var = str_replace(var, pattern = "ffood", replacement = "fast food"),
                var = str_replace(var, pattern = "pubs2", replacement = "pubs")),
       aes(x = reorder(var, m), y = m)) +
  geom_point(colour = scico(palette = 'buda', 10)[1]) +
  geom_segment(aes(xend = reorder(var, m), yend = 0), colour = scico(palette = 'buda', 10)[1]) +
  xlab("") +
  ylab("importance") +
  coord_flip() + 
  theme_rot() +
  ggsave('ranks.png')

play %>%
  filter(code != "E06000046") %>%
  mutate(lambdas = lambda) %>%
  select(lambdas) %>%
  ggplot() +
  geom_sf(data = background,
          aes(), 
          fill = 'grey70', colour = NA, size = 0) +
  geom_sf(aes(fill = lambdas), show.legend = FALSE) +
  scale_fill_scico(palette = 'buda') + 
  theme_map() +
  ggsave("lambdas.png")

##





