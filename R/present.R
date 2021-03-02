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
  as.data.frame() %>% 
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
  geom_sf(aes(fill = score), show.legend = FALSE, size = 0, colour = NA) +
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
  geom_sf(aes(fill = score), show.legend = FALSE, size = 0, colour = NA) +
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
library(glue)

##

ids <- 
  final %>% 
  pull(id) %>% 
  unique()

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
  geom_sf(aes(fill = lambdas), show.legend = FALSE, colour = NA, size = 0) +
  scale_fill_scico(palette = 'buda') + 
  theme_map() +
  ggsave("lambdas.png")

##

map_quads_1 <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = autocorrelating,
          aes(fill = factor(quad_sig_1)), size = 0, colour = NA) +
  scale_fill_manual(values = scico(palette = 'buda', 2, direction = -1),
                    name = "lifespan - healthspan",
                    labels = c("high-high", "low-low"),
                    guide = guide_discrete,
                    na.translate = FALSE) +
  labs(caption = "p < 0.01") +
  theme_map()  

map_quads_2 <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = autocorrelating,
          aes(fill = factor(quad_sig_2)), size = 0, colour = NA) +
  scale_fill_manual(values = scico(palette = 'buda', 2, direction = -1),
                    name = "lifespan - healthspan",
                    labels = c("high-high", "low-low"),
                    guide = guide_discrete,
                    na.translate = FALSE) +
  labs(caption = "p < 0.05") +
  theme_map()  

map_quads_3 <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = autocorrelating,
          aes(fill = factor(quad_sig_3)), size = 0, colour = NA) +
  scale_fill_manual(values = scico(palette = 'buda', 2, direction = -1),
                    name = "lifespan - healthspan",
                    labels = c("high-high", "low-low"),
                    guide = guide_discrete,
                    na.translate = FALSE) +
  labs(caption = "p < 0.1") +
  theme_map()  

p <- map_quads_1 + map_quads_2 + map_quads_3
p <- p + plot_layout(guides = "collect")

ggsave(filename = "quads.png", height = 6, width = 10)

##

left <-
  data %>%
  mutate(HLE = (healthy_life_expectancy_for_females_2009_2013_years + healthy_life_expectancy_for_males_2009_2013) / 2,
         income = (income_2016 - income_2006) / (income_2016 + income_2006)) %>%
  select(code, region, lower_tier_local_authority, HLE, everything()) %>%
  filter(code != "E06000053") %>%
  mutate_if(is.numeric, scale) %>%
  as('Spatial')

names(data)

bandwidth <- gwr.sel(HLE ~ income, 
                     data = left)

geogress <- gwr(HLE ~ income, 
                data = left, 
                bandwidth = bandwidth)

geogress <- st_as_sf(geogress$SDF)

##

map_income<- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = geogress,
          aes(fill = factor(ntile(income, 5))), size = 0.01, colour = 'gray70') +
  scale_fill_manual(values = scico(palette = 'buda', 5, direction = -1),
                    labels = str_sub(as.character(quantile(geogress$income,
                                                           c(.1,.2,.4,.6,.8),na.rm = TRUE)), 1, 4),
                    name = "coefficient",
                    guide = guide_discrete) +
  labs(caption = "income") +
  theme_map() +
  theme(plot.margin = margin(5, 30, 5, 30)) +
  ggsave("income_coefficients.png", height = 8, width = 5, dpi = 300)

##

vars <- c("income", "unemployment", "pollution", "lifestyle", "care")

lasso$beta

results <- tibble(index = 1:325)

left <-
  data %>%
  mutate(HLE = (healthy_life_expectancy_for_females_2009_2013_years + healthy_life_expectancy_for_males_2009_2013) / 2,
         income = (income_2016 - income_2006) / (income_2016 + income_2006), 
         pollution = scale(no2)[, 1] + scale(pm10)[, 1] + scale(so2)[, 1],
         lifestyle = scale(pubs2_d)[, 1] + scale(gamb_d)[, 1] + scale(ffood_d)[, 1],
         care = scale(gpp_d)[, 1] + scale(dent_d)[, 1]) %>%
  select(code, region, lower_tier_local_authority, HLE, everything()) %>%
  filter(code != "E06000053") %>%
  mutate_if(is.numeric, scale)

for (i in 1:length(vars)) {
  
  var <- vars[i]
  reg <-
    left %>%
    select(HLE, var) %>%
    as('Spatial')
  
  
  bandwidth <- gwr.sel(HLE ~ ., 
                       data = reg)
  
  geogress <- gwr(HLE ~ ., 
                  data = reg, 
                  bandwidth = bandwidth)
  
  geogress <- 
    geogress %>%
    magrittr::use_series(SDF) %>%
    st_as_sf() %>%
    select(var) %>%
    st_drop_geometry()
  
  results <- bind_cols(results, geogress)
  
}

##

results %>%
  mutate(geometry = st_geometry(left)) %>%
  st_as_sf()

##

longform <- 
  data %>%
  mutate(HLE = (healthy_life_expectancy_for_females_2009_2013_years + healthy_life_expectancy_for_males_2009_2013) / 2) %>%
  select(code, region, lower_tier_local_authority, HLE) %>%
  filter(code != "E06000053") %>%
  st_as_sf() %>%
  bind_cols(results) %>%
  clean_names() %>%
  select(code, region, lower_tier_local_authority, hle, 
         vars) %>%
  mutate(lifestyle = lifestyle * -1,
         care = care * -1) %>%
  gather(variable, value, income:care) %>%
  group_by(variable) %>%
  mutate(scaled = scale(value)) %>%
  mutate(tile = ntile(scaled, 10)) %>%
  ungroup()

map_layers <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'transparent', colour = '#000000', size = 0.25) +
  geom_sf(data = filter(longform, variable == "pollution"),
          aes(fill = factor(tile)), alpha = 0.2, size = 0, colour = NA) +
  geom_sf(data = filter(longform, variable == "unemployment"),
          aes(fill = factor(tile)), alpha = 0.2, size = 0, colour = NA) +
  geom_sf(data = filter(longform, variable == "income"),
          aes(fill = factor(tile)), alpha = 0.2, size = 0, colour = NA) +
  geom_sf(data = filter(longform, variable == "lifestyle"),
          aes(fill = factor(tile)), alpha = 0.2, size = 0, colour = NA) +
  geom_sf(data = filter(longform, variable == "care"),
          aes(fill = factor(tile)), alpha = 0.2, size = 0, colour = NA) +
  scale_fill_manual(values = scico(palette = "grayC", 10),
                    guide = 'none') +
  theme_map()

ggsave(map_layers, filename = "layers.png")

##

pal <- scico(10, palette = 'buda')

plot_regions <-
  data %>%
  mutate(HLE = (healthy_life_expectancy_for_females_2009_2013_years + healthy_life_expectancy_for_males_2009_2013) / 2) %>%
  filter(code != "E06000053") %>%
  st_as_sf() %>%
  st_drop_geometry() %>%
  select(code, region, lower_tier_local_authority, HLE) %>% 
  bind_cols(results) %>%
  clean_names() %>%
  select(code, region, lower_tier_local_authority, hle, 
         vars) %>%
  mutate(lifestyle = lifestyle * -1,
         care = care * -1) %>%
  gather(variable, value, income:care) %>%
  group_by(variable) %>%
  mutate(scaled = scale(value)) %>%
  ungroup() %>%
  group_by(variable, region) %>%
  summarise(m = abs(mean(scaled))) %>%
  ggplot(aes(x = m, y = reorder(region, m))) +
  geom_line(aes(group = region), colour = '#848484', size = 0.5) +
  geom_point(aes(colour = variable), shape = '|', size = 5) +
  scale_colour_manual(values = c(pal[1], pal[3], pal[5], pal[7], pal[9]),
                      labels = c("income", "unemployment", "pollution", "lifestyle", "care")) +
  scale_x_continuous(breaks = c(0)) +
  labs(x = "effect size", y = "") +
  theme_ver() 

ggsave(plot_regions, filename = "regions.png")

##

female <- 
  lm(healthy_life_expectancy_for_females_2009_2013_years ~ life_expectancy_at_birth_for_females_2009_2013, 
     data = expectancy) %>%
  summary() %>%
  magrittr::use_series("r.squared") %>%
  round(2)

male <- 
  lm(healthy_life_expectancy_for_males_2009_2013 ~ life_expectancy_at_birth_for_males_2009_2013, 
     data = expectancy) %>%
  summary() %>%
  magrittr::use_series("r.squared") %>%
  round(2)

plot <- ggplot(bind_rows(select(expectancy, 
                                healthy_life_expectancy_for_females_2009_2013_years,
                                life_expectancy_at_birth_for_females_2009_2013) %>%
                           set_names(c("in good health", "at birth")) %>%
                           mutate(class = "female"),
                         select(expectancy, healthy_life_expectancy_for_males_2009_2013,
                                life_expectancy_at_birth_for_males_2009_2013) %>%
                           set_names(c("in good health", "at birth")) %>%
                           mutate(class = "male")), 
               aes(x = `at birth`, y = `in good health`, colour = class)) +
  geom_point(alpha = 0.5, show.legend = FALSE) +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, 
              linetype = 2, show.legend = FALSE) +
  geom_text(aes(x = 75, y = 80, label = glue("r-squared = {male}")), hjust = 0) +
  geom_text(aes(x = 80, y = 50, label = glue("r-squared = {female}")), hjust = 0) +
  scale_colour_manual(values = c(scico(palette = "grayC", 10)[3], 
                                 scico(palette = "grayC", 10)[10])) +
  theme_ver() +
  theme(legend.position = 'bottom')

map <- ggplot(data =
                expectancy %>%
                left_join(authorities) %>%
                drop_na() %>%
                st_as_sf() %>%
                select(healthy_life_expectancy_for_females_2009_2013_years,
                       healthy_life_expectancy_for_males_2009_2013) %>%
                gather(variable, value, healthy_life_expectancy_for_females_2009_2013_years:healthy_life_expectancy_for_males_2009_2013) %>%
                mutate(name = case_when(variable == "healthy_life_expectancy_for_males_2009_2013" ~ "male",
                                        variable == "healthy_life_expectancy_for_females_2009_2013_years" ~ "female")) %>%
                select(variable, name, value, geometry)) +
  geom_sf(data = background,
          aes(), 
          fill = 'grey70', colour = NA, size = 0) +
  geom_sf(aes(fill = value), 
          colour = NA, size = 0) +
  scale_fill_scico(palette = 'buda', direction = -1,
                   guide = guide_continuous) +
  facet_wrap(~ name) +
  theme_map()

##

p <- plot + map + plot_layout(widths = c(1, 2)) 

ggsave(p, filename = "left.png", height = 4, width = 8, dpi = 300)

##

rezzies <- 
  ridge$SDF %>% 
  st_as_sf() %>% 
  transmute(yhat = lasso$yhat,
            residual = y - yhat) 

rezzies <- 
  bind_rows(ridge$SDF %>% 
              st_as_sf() %>% 
              transmute(yhat = lasso$yhat,
                        y = y,
                        model = "lasso"),
            ridge$SDF %>% 
              st_as_sf() %>% 
              transmute(yhat = yhat,
                        y = y,
                        model = "ridge")) %>%
  st_as_sf() %>%
  mutate(residual = y - yhat)

map <- ggplot(rezzies) + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = rezzies,
          aes(fill = residual), size = 0.01, colour = 'gray70') +
  scale_fill_scico(palette = 'buda',
                   name = "residual",
                   breaks = c(-0.2, 0, 0.2),
                   limits = c(-0.4, 0.4),
                   oob = squish, 
                   guide = guide_continuous) +
  facet_wrap(~ model) +
  theme_map() +
  ggsave("rezzies.png")

plot <- ggplot(rezzies) + 
  geom_hline(yintercept = 0, colour = 'grey', linetype = 2, size = 1) +
  geom_point(data = rezzies,
             aes(colour = residual,
                 x = y, y = residual), size = 1, 
             show.legend = FALSE) +
  scale_colour_scico(palette = 'buda',
                     name = "residual",
                     breaks = c(-0.2, 0, 0.2),
                     limits = c(-0.4, 0.4),
                     oob = squish, 
                     guide = guide_continuous) +
  facet_wrap(~ model, nrow = 2) +
  theme_ver() +
  ggsave("scedasticity.png")

p <- plot + map + 
  plot_layout(widths = c(1, 2)) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 8))

ggsave(p, filename = "rezzies.png", height = 4, width = 8, dpi = 300)

##

t(lasso$beta)

##

summary_variables <- 
  read_sf("data/hackathon/hexgrid.shp") %>%
  st_as_sf() %>%
  select(code) %>%
  left_join(st_drop_geometry(data)) %>%
  st_as_sf()

##

map <-
  ggplot(data = 
           summary_variables %>%
           filter(code %in% expectancy$code) %>%
           transmute(`doctors` = gpp_d, 
                     `leisure` = leis_d, 
                     `fast food` = ffood_d, 
                     `pubs` = pubs2_d) %>%
           gather(variable, value, `doctors`:`pubs`)) +
  geom_sf(aes(fill = value), 
          colour = NA, size = 0) +
  scale_fill_scico(palette = 'buda', direction = 1,
                   guide = guide_continuous,
                   name = "distance to... (km)",
                   limits = c(0, 10), oob = squish) +
  facet_wrap(~ variable) +
  theme_map()

ggsave(map, filename = "variables.png", height = 8, width = 8, dpi = 300)

##

map <-
  ggplot(data = 
           summary_variables %>%
           filter(code %in% expectancy$code) %>%
           mutate(HLE = (healthy_life_expectancy_for_females_2009_2013_years + healthy_life_expectancy_for_males_2009_2013) / 2) %>%
           select(income_2000, income_2005, income_2010, income_2015) %>%
           gather(variable, value, income_2000:income_2015) %>%
           mutate(variable = str_replace_all(variable, pattern = "_", replacement = " "))) +
  geom_sf(aes(fill = value), 
          colour = NA, size = 0) +
  scale_fill_scico(palette = 'buda', direction = 1,
                   guide = guide_continuous,
                   name = "median income") +
  facet_wrap(~ variable) +
  theme_map()

ggsave(map, filename = "incomes.png", height = 8, width = 8, dpi = 300)

##

df <- filter(regress, id == i)
y <- df$HLE
x <- as.matrix(df[, 3:ncol(df)])

fit <- cv.glmnet(x, y, alpha = 0, type.measure = "mse", nfolds = 20)

betas <- fit$glmnet.fit$beta
mats <- as.matrix(betas)

lambda_1 <-
  mats %>% 
  as_tibble() %>% 
  set_names(fit$lambda) %>%
  mutate(variable = names(df[, 3:ncol(df)])) %>%
  select(variable, everything()) %>%
  pivot_longer(cols = as.character(fit$lambda)) %>%
  mutate(name = as.numeric(str_remove_all(name, "s"))) %>%
  ggplot(aes(x = log(name), y = value, colour = variable, linetype = variable)) +
  geom_line(size = 1, show.legend = FALSE) +
  scale_colour_manual(values = scico(palette = 'buda', 13)) +
  scale_y_continuous(breaks = c(-0.2, 0, 0.2), limits = c(-0.4, 0.4)) +
  xlab("lambda (logo)") + 
  ylab("beta") +
  theme_ver() +
  ggsave("demo_ridge.png")

fit <- cv.glmnet(x, y, alpha = 1, type.measure = "mse", nfolds = 20)

betas <- fit$glmnet.fit$beta
mats <- as.matrix(betas)

length(fit$lambda)

lambda_2 <- 
  mats %>% 
  as_tibble() %>%
  set_names(fit$lambda) %>%
  mutate(variable = names(df[, 3:ncol(df)])) %>%
  select(variable, everything()) %>%
  pivot_longer(cols = as.character(fit$lambda)) %>%
  mutate(name = as.numeric(str_remove_all(name, "s"))) %>%
  ggplot(aes(x = log(name), y = value, colour = variable, linetype = variable)) +
  geom_line(size = 1, show.legend = FALSE) +
  scale_colour_manual(values = scico(palette = 'buda', 13)) +
  scale_y_continuous(breaks = c(-1, 0, 1), limits = c(-2, 2), labels = c("-1.0", "0.0", "1.0")) + 
  xlab("lambda (logo)") + 
  ylab("beta") +
  theme_ver() +
  ggsave("demo_lasso.png")

p <- lambda_1 + lambda_2 + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 8))

ggsave(p, filename = "demo_lambdas.png", height = 5, width = 14)
