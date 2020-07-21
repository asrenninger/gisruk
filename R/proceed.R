source("R/help.R")
source("R/package.R")

#################################
## Background

national_ages <-
  bind_rows(read_xls("data/hackathon/aging.xls", skip = 5, sheet = 2) %>%
              clean_names() %>%
              filter(area == "England" & age_group != "All ages") %>%
              separate(age_group, sep = "-", into = c("lower", "upper")) %>%
              replace_na(list("upper" = 94)) %>%
              mutate(upper = as.numeric(upper)) %>%
              select(upper, x2016:x2041) %>%
              gather(year, population, x2016:x2036) %>%
              mutate(population = population * -1, group = "male"),
            read_xls("data/hackathon/aging.xls", skip = 5, sheet = 3) %>%
              clean_names() %>%
              filter(area == "England" & age_group != "All ages") %>%
              separate(age_group, sep = "-", into = c("lower", "upper")) %>%
              replace_na(list("upper" = 94)) %>%
              mutate(upper = as.numeric(upper)) %>%
              select(upper, x2016:x2041) %>%
              gather(year, population, x2016:x2036) %>%
              mutate(group = "female"))

##

library(gganimate)

##

groups <- 
  read_xls("data/hackathon/aging.xls", skip = 5, sheet = 2) %>%
  clean_names() %>%
  filter(area == "England" & age_group != "All ages") %>%
  separate(age_group, sep = "-", into = c("lower", "upper"), remove = FALSE) %>%
  replace_na(list("upper" = 94)) %>%
  mutate(upper = as.numeric(upper)) %>%
  select(age_group, lower, upper) %>%
  group_by(age_group) %>%
  slice(1)

## Figure 1
national_aging <- 
  ggplot(national_ages %>%
           left_join(groups) %>%
           mutate(year = str_replace_all(year, pattern = "x", replacement = ""))) +
  geom_bar(aes(y = population, x = upper, fill = group), stat = 'identity',
           show.legend = FALSE) +
  geom_text(aes(x = upper, y = 0, label = age_group), colour = '#ffffff') +
  scale_fill_manual(values = c(scico(palette = "grayC", 10)[3], scico(palette = "grayC", 10)[10])) +
  scale_y_continuous(labels = abs, limits = max(national_ages$population, na.rm = TRUE) * c(-1, 1)) +
  scale_x_continuous(labels = c("", "", ""), breaks = c(25, 50, 75), limits = c(-1, 100)) +
  transition_states(year) +
  ease_aes('linear') +
  labs(title = "national projections {closest_state}",
       subtitle = "PROJECTED POPULATION BY AGE",
       x = "age bracket", y = "population") +
  coord_flip() +
  theme_pop()

##

anim_save(national_aging, filename = "aging.gif")

##

age_2016 <- 
  ggplot(national_ages %>%
           left_join(groups) %>%
           mutate(year = str_replace_all(year, pattern = "x", replacement = "")) %>%
           filter(year == "2016")) +
  geom_bar(aes(y = population, x = upper, fill = group), stat = 'identity',
           show.legend = FALSE) +
  geom_text(aes(x = upper, y = 0, label = age_group), colour = '#ffffff') +
  scale_fill_manual(values = c(scico(palette = "grayC", 10)[3], scico(palette = "grayC", 10)[10])) +
  scale_y_continuous(labels = abs, limits = max(national_ages$population, na.rm = TRUE) * c(-1, 1)) +
  scale_x_continuous(labels = c("", "", ""), breaks = c(25, 50, 75), limits = c(-1, 100)) +
  labs(x = "age bracket", y = "population") +
  coord_flip() +
  theme_pop()

age_2036 <- 
  ggplot(national_ages %>%
           left_join(groups) %>%
           mutate(year = str_replace_all(year, pattern = "x", replacement = "")) %>%
           filter(year == "2036")) +
  geom_bar(aes(y = population, x = upper, fill = group), stat = 'identity',
           show.legend = FALSE) +
  geom_text(aes(x = upper, y = 0, label = age_group), colour = '#ffffff') +
  scale_fill_manual(values = c(scico(palette = "grayC", 10)[3], scico(palette = "grayC", 10)[10])) +
  scale_y_continuous(labels = abs, limits = max(national_ages$population, na.rm = TRUE) * c(-1, 1)) +
  scale_x_continuous(labels = c("", "", ""), breaks = c(25, 50, 75), limits = c(-1, 100)) +
  labs(x = "age bracket", y = "population") +
  coord_flip() +
  theme_pop()

p <- age_2016 + age_2036 +  
  plot_layout(widths = c(1, 1)) + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 8))

ggsave(p, filename = "fig1.png", height = 4, width = 8, dpi = 300)

## Data
expectancy <- 
  read_xls("data/hackathon/healthy.xls", skip = 10, sheet = 1) %>%
  clean_names()

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

tmap_mode("view")

tm_shape(authorities) +
  tm_polygons()

##

library(magrittr)
library(glue)

#################################
## Left

names(expectancy)

female <- 
  lm(healthy_life_expectancy_for_females_2009_2013_years ~ life_expectancy_at_birth_for_females_2009_2013, 
   data = expectancy) %>%
  summary() %>%
  use_series("r.squared") %>%
  round(2)

male <- 
  lm(healthy_life_expectancy_for_males_2009_2013 ~ life_expectancy_at_birth_for_males_2009_2013, 
     data = expectancy) %>%
  summary() %>%
  use_series("r.squared") %>%
  round(2)

## Figure 3
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

##

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
  scale_fill_scico(palette = 'lajolla', direction = -1,
                   guide = guide_continuous) +
  facet_wrap(~ name) +
  theme_map()

##

p <- plot + map +  
  plot_layout(widths = c(1, 2)) + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 8))

ggsave(p, filename = "fig3.png", height = 4, width = 8, dpi = 300)

##

lm(healthy_life_expectancy_for_females_2009_2013_years ~ life_expectancy_at_birth_for_females_2009_2013, 
   data = expectancy) %>%
  summary() %>%
  use_series("coef")

lm(healthy_life_expectancy_for_males_2009_2013 ~ life_expectancy_at_birth_for_males_2009_2013, 
     data = expectancy) %>%
  summary() %>%
  use_series("coef")

#################################
## Right

ahah <- read_csv("data/hackathon/ahahinputs.csv")

area <-   
  read_sf("data/hackathon/area.shp") %>%
  st_drop_geometry() %>%
  transmute(lsoa11 = lsoa01cd,
            code = lad17cd) %>%
  left_join(ahah) %>%
  select(lsoa11:green900) %>%
  group_by(code) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  select(-seqnum)

## Figure 2

summary_variables <- 
  read_sf("data/hackathon/hexgrid.shp") %>%
  st_as_sf() %>%
  select(code) %>%
  left_join(area) %>%
  st_as_sf()

##

map <-
  ggplot(data = 
           summary_variables %>%
           filter(code %in% expectancy$code) %>%
           transmute(`general practicioners` = gpp_d, 
                     `hospitals` = ed_d, 
                     `dentists` = dent_d, 
                     `pharmacies` = pharm_d) %>%
           gather(variable, value, `general practicioners`:`pharmacies`)) +
  geom_sf(aes(fill = value), 
          colour = NA, size = 0) +
  scale_fill_scico(palette = 'lajolla', direction = 1,
                   guide = guide_continuous,
                   name = "distance to... (km)",
                   limits = c(0, 10), oob = squish) +
  facet_wrap(~ variable) +
  theme_map()

##

ggsave(map, filename = "fig2.png", height = 8, width = 8, dpi = 300)

## Extra data

rururb <- 
  read_csv("data/hackathon/rururb.csv") %>%
  clean_names() %>%
  rename(code = lad11cd) %>%
  select(code, total_rural_population_2011:total_population_2011, ruc11, broad_ruc11) %>%
  rename(class = ruc11,
         class_broad = broad_ruc11)

##

deaths <- read_csv("data/hackathon/deaths.csv") %>%
  clean_names() %>%
  rename(code = lad11cd) %>%
  select(code, deaths_num:agestand_mortality)

##

income <- 
  read_xlsx("data/hackathon/income.xlsx", sheet = 2, skip = 2) %>%
  clean_names() %>%
  rename(code = lau1_code) %>%
  select(code, region, x1998:x2016)

names(income) <- str_replace_all(names(income), pattern = "x", replacement = "income_")

#################################
## Aggregating

data <- 
  expectancy %>%
  mutate(difference_male = life_expectancy_at_birth_for_males_2009_2013 - healthy_life_expectancy_for_males_2009_2013,
         difference_female = life_expectancy_at_birth_for_females_2009_2013 - healthy_life_expectancy_for_females_2009_2013_years) %>%
  left_join(area) %>%
  left_join(rururb) %>%
  left_join(deaths) %>%
  left_join(income) %>%
  left_join(authorities) %>%
  st_as_sf() %>%
  select(code, region, everything())

##

correlations <-
  data %>%
  mutate(difference_male = life_expectancy_at_birth_for_males_2009_2013 - healthy_life_expectancy_for_males_2009_2013,
         difference_female = life_expectancy_at_birth_for_females_2009_2013 - healthy_life_expectancy_for_females_2009_2013_years) %>%
  transmute(`male (diff)` = difference_male,
            `female (diff)` = difference_female,
            `mortality` = agestand_mortality,
            `fast food (d)` = ffood_d,
            `pubs (d)` = pubs2_d,
            `GPs (d)` = gpp_d,
            `population` = total_population_2011,
            `density` = total_population_2011 / (area / (1000 * 1000)),
            `income` = income_2011,
            `rur-urban` = urban_city_and_town_population_2011 / total_population_2011,
            `living alone` = older_people_living_alone,
            `unemployment` = unemployment,
            `NO2` = no2,
            `PM10` = pm10, 
            `parks` = green900) %>%
  st_drop_geometry() %>%
  drop_na()

##

correlate(correlations, "fig4.png")

#################################
## Regressing

left <-
  data %>%
  mutate(HLE = (healthy_life_expectancy_for_females_2009_2013_years + healthy_life_expectancy_for_males_2009_2013) / 2,
         income = (income_2016 - income_2006) / (income_2016 + income_2006)) %>%
  select(code, region, lower_tier_local_authority, HLE, everything()) %>%
  filter(code != "E06000053") %>%
  mutate_if(is.numeric, scale) %>%
  as('Spatial')

## Indexing
left$pollution <- scale(left$no2)[, 1] + scale(left$pm10)[, 1] + scale(left$so2)[, 1]
left$lifestyle <- scale(left$pubs2_d)[, 1] + scale(left$gamb_d)[, 1] + scale(left$ffood_d)[, 1]
left$care <- scale(left$gpp_d)[, 1] + scale(left$dent_d)[, 1]

##

bandwidth <- gwr.sel(HLE ~ unemployment, 
                     data = left)

geogress <- gwr(HLE ~ unemployment, 
                data = left, 
                bandwidth = bandwidth)

geogress <- st_as_sf(geogress$SDF)

map_income<- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = geogress,
          aes(fill = factor(ntile(unemployment, 5))), size = 0.01, colour = 'gray70') +
  scale_fill_manual(values = scico(palette = 'buda', 5, direction = -1),
                    labels = str_sub(as.character(quantile(geogress$unemployment,
                                                           c(.1,.2,.4,.6,.8),na.rm = TRUE)), 1, 4),
                    name = "coefficent",
                    guide = guide_discrete) +
  labs(caption = "unemployment (d)") +
  theme_map() +
  theme(plot.margin = margin(5, 30, 5, 30)) +
  ggsave("unemployment.png")

## Figure 6
results <- read_csv("data/hackathon/gwrresults.csv")

dim(results)
dim(data)

shape <- 
  data %>%
  filter(code != "E06000053") %>%
  select(code, geometry)

results_shape <-
  results %>%
  bind_cols(shape) %>%
  clean_names() %>%
  st_as_sf() 

##

map_deprivation <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = results_shape,
          aes(fill = factor(ntile(depriv_coef, 5))), size = 0.01, colour = 'gray70') +
  scale_fill_manual(values = scico(palette = 'oslo', 5),
                    labels = str_sub(as.character(quantile(results_shape$depriv_coef,
                                                           c(.1,.2,.4,.6,.8),na.rm = TRUE)), 1, 4),
                    name = "coefficent",
                    guide = guide_discrete) +
  labs(caption = "deprivation") +
  theme_map() +
  theme(plot.margin = margin(5, 30, 5, 30))

map_air <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = results_shape,
          aes(fill = factor(ntile(airpol_coef, 5))), size = 0.01, colour = 'gray70') +
  scale_fill_manual(values = scico(palette = 'oslo', 5),
                    labels = str_sub(as.character(quantile(results_shape$airpol_coef,
                                                           c(.1,.2,.4,.6,.8),na.rm = TRUE)), 1, 4),
                    name = "coefficent",
                    guide = guide_discrete) +
  labs(caption = "air pollution") +
  theme_map() +
  theme(plot.margin = margin(5, 30, 5, 30))

map_lifestyle <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = results_shape,
          aes(fill = factor(ntile(lifest_coef, 5))), size = 0.01, colour = 'gray70') +
  scale_fill_manual(values = scico(palette = 'oslo', 5, direction = -1),
                    labels = str_sub(as.character(quantile(results_shape$lifest_coef,
                                                           c(.1,.2,.4,.6,.8),na.rm = TRUE)), 1, 4),
                    name = "coefficent",
                    guide = guide_discrete) +
  labs(caption = "lifestyle distances") +
  theme_map() +
  theme(plot.margin = margin(5, 30, 5, 30))

map_unemployment <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = results_shape,
          aes(fill = factor(ntile(unempl_coef, 5))), size = 0.01, colour = 'gray70') +
  scale_fill_manual(values = scico(palette = 'oslo', 5),
                    labels = str_sub(as.character(quantile(results_shape$unempl_coef,
                                                           c(.1,.2,.4,.6,.8),na.rm = TRUE)), 1, 4),
                    name = "coefficent",
                    guide = guide_discrete) +
  labs(caption = "unemployment") +
  theme_map() +
  theme(plot.margin = margin(5, 30, 5, 30))

map_health <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = results_shape,
          aes(fill = factor(ntile(hlthserv_coef, 5))), size = 0.01, colour = 'gray70') +
  scale_fill_manual(values = scico(palette = 'oslo', 5),
                    labels = str_sub(as.character(quantile(results_shape$hlthserv_coef,
                                                           c(.1,.2,.4,.6,.8),na.rm = TRUE)), 1, 4),
                    name = "coefficent",
                    guide = guide_discrete) +
  labs(caption = "health services") +
  theme_map() +
  theme(plot.margin = margin(5, 30, 5, 30))

##

p <- (map_lifestyle + map_health) / (map_unemployment + map_deprivation)
p <- p +   
  theme(plot.margin = margin(20, 20, 20, 20)) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 8))

ggsave(p, filename = "fig6.png", height = 8, width = 8, dpi = 300)

## Ridge 
left <-
  data %>%
  mutate(HLE = (healthy_life_expectancy_for_females_2009_2013_years + healthy_life_expectancy_for_males_2009_2013) / 2,
         pollution = scale(no2)[, 1] + scale(pm10)[, 1] + scale(so2)[, 1],
         income = (income_2016 - income_2006) / (income_2016 + income_2006)) %>%
  select(code, region, lower_tier_local_authority, HLE, everything()) %>%
  filter(code != "E06000053") %>%
  mutate_if(is.numeric, rescale) %>%
  glimpse() %>%
  as('Spatial')

ridge <- 
  gwr.lcr(HLE ~ pollution + unemployment + income + 
            gamb_d + ffood_d + pubs2_d + tobac_d + green900 + leis_d +
            ed_d + pharm_d + gpp_d + dent_d,
          data =   left,
          lambda.adjust = TRUE, 
          cn.thresh = 30,
          cv = TRUE,
          bw = bandwidth, 
          kernel = "gaussian")

## Lasso 
left <-
  data %>%
  mutate(HLE = (healthy_life_expectancy_for_females_2009_2013_years + healthy_life_expectancy_for_males_2009_2013) / 2,
         pollution = scale(no2)[, 1] + scale(pm10)[, 1] + scale(so2)[, 1],
         income = (income_2016 - income_2006) / (income_2016 + income_2006)) %>%
  select(code, region, lower_tier_local_authority, HLE, everything()) %>%
  filter(code != "E06000053") %>%
  mutate_if(is.numeric, rescale) %>%
  st_drop_geometry()

coords <-
  data %>%
  filter(code != "E06000053") %>%
  st_transform(4326) %>%
  st_centroid() %>%
  st_coordinates()

lasso <- 
  gwl.est(HLE ~ pollution + unemployment + income + 
            gamb_d + ffood_d + pubs2_d + tobac_d + green900 + leis_d +
            ed_d + pharm_d + gpp_d + dent_d,
          locs = coords, 
          data = as.data.frame(left), 
          cv.tol = 30,
          kernel = "exp")

real <- ridge$SDF %>% st_as_sf() %>% filter(y != 0) %>% pull(y)
pred <- ridge$SDF %>% st_as_sf() %>% filter(y != 0) %>% pull(yhat)

rmspe_ridge <- MLmetrics::RMSPE(pred, real)

real <- 
  left %>%
  mutate(yhat = lasso$yhat) %>%
  filter(HLE != 0) %>%
  pull(HLE)

pred <- 
  left %>%
  mutate(yhat = lasso$yhat) %>%
  filter(HLE != 0) %>%
  pull(yhat)

rmspe_lasso <- MLmetrics::RMSPE(pred, real)

mean(left$HLE - ridge$SDF$yhat)
mean(left$HLE - lasso$yhat)

gwrr:::gwl.est
gwrr:::gwl.beta

t(lasso$beta) %>% 
  as_tibble() %>%
  set_names(c("intercept", "pollution", "unemployment", "income", "gamb_d", "ffood_d", "pubs2_d", "tobac_d",
              "green900", "leis_d", 
              "ed_d", "pharm_d", "gpp_d", "dent_d")) %>%
  select(2:14) %>%
  summarise_all(mean)

ridge$SDF %>%
  as_tibble() %>%
  select(2:14) %>%
  summarise_all(mean)

ridge$SDF %>% st_as_sf() %>% select(yhat, residual) %>% plot()
ridge$SDF%>% st_as_sf() %>% transmute(yhat = lasso$yhat,
                                      residual = y - yhat) %>% plot()

## Figure 7
longform <- 
  data %>%
  mutate(HLE = (healthy_life_expectancy_for_females_2009_2013_years + healthy_life_expectancy_for_males_2009_2013) / 2) %>%
  select(code, region, lower_tier_local_authority, HLE) %>%
  filter(code != "E06000053") %>%
  st_as_sf() %>%
  bind_cols(results) %>%
  clean_names() %>%
  select(code, region, lower_tier_local_authority, hle, 
         airpol_coef, depriv_coef, lifest_coef, unempl_coef, hlthserv_coef) %>%
  mutate(lifest_coef = lifest_coef * -1) %>%
  gather(variable, value, airpol_coef:hlthserv_coef) %>%
  group_by(variable) %>%
  mutate(scaled = scale(value)) %>%
  mutate(tile = ntile(scaled, 10)) %>%
  ungroup()

unique(longform$variable)

map_layers <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'transparent', colour = '#000000', size = 0.25) +
  geom_sf(data = filter(longform, variable == "airpol_coef"),
          aes(fill = factor(tile)), alpha = 0.2, size = 0, colour = NA) +
  geom_sf(data = filter(longform, variable == "depriv_coef"),
          aes(fill = factor(tile)), alpha = 0.2, size = 0, colour = NA) +
  geom_sf(data = filter(longform, variable == "hlthserv_coef"),
          aes(fill = factor(tile)), alpha = 0.2, size = 0, colour = NA) +
  geom_sf(data = filter(longform, variable == "lifest_coef"),
          aes(fill = factor(tile)), alpha = 0.2, size = 0, colour = NA) +
  geom_sf(data = filter(longform, variable == "unempl_coef"),
          aes(fill = factor(tile)), alpha = 0.2, size = 0, colour = NA) +
  scale_fill_manual(values = scico(palette = "grayC", 10),
                    guide = 'none') +
  theme_map()

ggsave(map_layers, filename = "layers.png", height = 8, width = 8, dpi = 300)

##

pal <- scico(10, palette = 'acton')

##

plot_regions <-
  data %>%
  mutate(HLE = (healthy_life_expectancy_for_females_2009_2013_years + healthy_life_expectancy_for_males_2009_2013) / 2) %>%
  filter(code != "E06000053") %>%
  st_as_sf() %>%
  st_drop_geometry() %>%
  bind_cols(results) %>%
  clean_names() %>%
  select(code, region, lower_tier_local_authority, hle, 
         airpol_coef, depriv_coef, lifest_coef, unempl_coef, hlthserv_coef) %>%
  mutate(lifest_coef = lifest_coef * -1) %>%
  gather(variable, value, airpol_coef:hlthserv_coef) %>%
  group_by(variable, region) %>%
  mutate(scaled = scale(value)) %>%
  summarise(m = mean(scaled)) %>%
  ggplot(aes(x = m, y = reorder(region, m))) +
  geom_line(aes(group = region), colour = '#848484', size = 0.5) +
  geom_point(aes(colour = variable), shape = '|', size = 5) +
  scale_colour_manual(values = c(pal[1], pal[3], pal[5], pal[7], pal[9]),
                      labels = c("pollution", "deprivation", "services", "lifestyle", "unemployment")) +
  scale_x_continuous(breaks = c(0)) +
  labs(x = "mean coefficient z-score", y = "") +
  theme_ver() 

p <- map_layers + plot_regions 
p <- p +
  plot_layout(widths = c(1, 1)) + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 8))

ggsave(p, filename = "fig7.png", height = 4, width = 8, dpi = 300)

#################################
## Moransiing

autocorrelating <- 
  expectancy %>%
  mutate(difference_male = life_expectancy_at_birth_for_males_2009_2013 - healthy_life_expectancy_for_males_2009_2013,
         difference_female = life_expectancy_at_birth_for_females_2009_2013 - healthy_life_expectancy_for_females_2009_2013_years) %>%
  mutate(difference = ((difference_male + difference_female) / 2)) %>%
  left_join(authorities) %>%
  drop_na() %>%
  st_as_sf() 

coords <- 
  autocorrelating %>%
  st_centroid() %>%
  st_coordinates()

plot(background)
plot(autocorrelating[, 1], add = T)

## Global
nearest <- knn2nb(knearneigh(coords, 5))
weights <- nb2listw(nearest, style = "W")

moranstest <- moran.test(autocorrelating$difference, weights)
montecarlo <- moran.mc(autocorrelating$difference, weights, nsim = 999)

moranstest

ggplot(as.data.frame(montecarlo$res), aes(montecarlo$res)) + 
  geom_histogram(binwidth = 0.01) +
  geom_vline(aes(xintercept = 0.466), colour = "grey70",size = 1) +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(title = "observed and permuted Moran's I",
       subtitle = "I = 0.466 | P < 0.01",
       x = "results",
       y = "count") +
  theme_ver()

## Local
moransi <- localmoran(autocorrelating$difference, weights) %>% as_tibble()

autocorrelating <- 
  autocorrelating %>%
  st_transform(27700) %>%
  bind_cols(moransi) %>%
  rename(locali = Ii,
         expectation = E.Ii,
         variance = Var.Ii,
         deviation = Z.Ii,
         p_value = `Pr(z > 0)`)

##

map_moran_v <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = autocorrelating,
          aes(fill = factor(ntile(difference, 5))), size = 0, colour = NA) +
  scale_fill_manual(values = scico(palette = 'cork', 5),
                    labels = as.character(quantile(autocorrelating$difference,
                                                   c(.1,.2,.4,.6,.8),
                                                   na.rm = TRUE)),
                    name = "difference",
                    guide = guide_discrete) +
  labs(title = "difference") +
  theme_map()

map_moran_i <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = autocorrelating,
          aes(fill = factor(ntile(locali, 5))), size = 0, colour = NA) +
  scale_fill_manual(values = scico(palette = 'cork', 5),
                    labels = str_sub(as.character(quantile(autocorrelating$locali,
                                                           c(.1,.2,.4,.6,.8),
                                                           na.rm = TRUE)), 1, 4),
                    name = "i value",
                    guide = guide_discrete) +
  labs(title = "local moran's i") +
  theme_map()

map_moran_p <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = autocorrelating,
          aes(fill = factor(ntile(p_value, 5))), size = 0, colour = NA) +
  scale_fill_manual(values = scico(palette = 'cork', 5),
                    labels = str_sub(as.character(quantile(autocorrelating$p_value,
                                                           c(.1,.2,.4,.6,.8),
                                                           na.rm = TRUE)), 1, 4),
                    name = "p value",
                    guide = guide_discrete) +
  labs(title = "p value") +
  theme_map()  

library(patchwork)

p <- map_moran_v + map_moran_i + map_moran_p

ggsave(filename = "moran.png", height = 6, width = 10)

##

autocorrelating <- 
  autocorrelating %>%
  mutate(scaled_difference = scale(difference)) %>%
  select(difference, scaled_difference, locali, expectation, variance, deviation, p_value) %>%
  mutate(lagged_difference = lag.listw(weights, scaled_difference),
         quad_sig = NA)

##

autocorrelating <-
  autocorrelating %>%
  mutate(quad_sig_1 = 
           case_when(scaled_difference >= 0 & lagged_difference >= 0 & p_value <= 0.01 ~ 1,
                     scaled_difference <= 0 & lagged_difference <= 0 & p_value <= 0.01 ~ 2,
                     scaled_difference >= 0 & lagged_difference <= 0 & p_value <= 0.01 ~ 3,
                     scaled_difference >= 0 & lagged_difference <= 0 & p_value <= 0.01 ~ 4,
                     scaled_difference <= 0 & lagged_difference >= 0 & p_value <= 0.01 ~ 5)) %>%
  mutate(quad_sig_2 = 
           case_when(scaled_difference >= 0 & lagged_difference >= 0 & p_value <= 0.05 ~ 1,
                     scaled_difference <= 0 & lagged_difference <= 0 & p_value <= 0.05 ~ 2,
                     scaled_difference >= 0 & lagged_difference <= 0 & p_value <= 0.05 ~ 3,
                     scaled_difference >= 0 & lagged_difference <= 0 & p_value <= 0.05 ~ 4,
                     scaled_difference <= 0 & lagged_difference >= 0 & p_value <= 0.05 ~ 5)) %>%
  mutate(quad_sig_3 = 
           case_when(scaled_difference >= 0 & lagged_difference >= 0 & p_value <= 0.1 ~ 1,
                     scaled_difference <= 0 & lagged_difference <= 0 & p_value <= 0.1 ~ 2,
                     scaled_difference >= 0 & lagged_difference <= 0 & p_value <= 0.1 ~ 3,
                     scaled_difference >= 0 & lagged_difference <= 0 & p_value <= 0.1 ~ 4,
                     scaled_difference <= 0 & lagged_difference >= 0 & p_value <= 0.1 ~ 5)) %>%
  st_as_sf()

##

map_quads_1 <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = autocorrelating,
          aes(fill = factor(quad_sig_1)), size = 0, colour = NA) +
  scale_fill_manual(values = scico(palette = 'cork', 2),
                    name = "quadrants",
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
  scale_fill_manual(values = scico(palette = 'cork', 2),
                    name = "quadrants",
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
  scale_fill_manual(values = scico(palette = 'cork', 2),
                    name = "quadrants",
                    labels = c("high-high", "low-low"),
                    guide = guide_discrete,
                    na.translate = FALSE) +
  labs(caption = "p < 0.1") +
  theme_map()  

p <- map_quads_1 + map_quads_2 + map_quads_3
p <- p +   
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 8))

ggsave(filename = "fig5.png", height = 6, width = 10)

##

tm_shape(autocorrelating) +
  tm_fill(col = "quad_sig_3", alpha = 0.5)