##########################################################
# Wrangling Script
##########################################################

## Libraries
library(vroom)
library(tidyverse)
library(janitor)

library(sf)
library(rmapshaper)
library(tmap)
library(tmaptools)

library(scico)
library(scales)

##
source("R/help.R")

## Shapes
lsoas <- 
  read_sf("data/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_EW_BGC-shp/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_EW_BGC.shp") %>%
  clean_names() %>%
  ms_simplify(keep = 0.005) 

plot(lsoas)

## Variables
data <- 
  read_csv("data/ahahinputs.csv") %>%
  rename(lsoa11cd = lsoa11)

variables <- data %>% select(3:14) %>% names()

## Aggregation
data_spatial <- 
  data %>%
  left_join(lsoas) %>% 
  drop_na(lsoa11nm) %>%
  mutate_at(variables, scale) %>% 
  filter(seqnum != 18582 & seqnum != 15695) %>%
  st_as_sf()

## Left
deprivation <- 
  vroom("data/imd.csv") %>% 
  clean_names() %>%
  rename(lsoa11cd = lsoa_code_2011) %>%
  left_join(lsoas) %>%
  st_as_sf()

## Exploration
names(deprivation)

ggplot(deprivation) + 
  geom_sf(aes(fill = discreter(index_of_multiple_deprivation_imd_score, 5)), lwd = 0) +
  scale_fill_manual(values = scico(palette = 'lajolla', 5),
                    guide = guide_discrete,
                    labels = labeller(deprivation$index_of_multiple_deprivation_imd_score, 5),
                    name = "deprivation score",
                    na.value = "blue") +
  theme_map()

long <- 
  deprivation %>%
  select_if(!str_detect(names(.),"decile")) %>%
  select_if(!str_detect(names(.),"rank")) %>%
  set_names(str_remove_all(names(.), "_score")) %>%
  select(employment_rate, education_skills_and_training, health_deprivation_and_disability, crime, geometry) %>%
  pivot_longer(employment_rate:crime) %>%
  select(name, value, geometry) %>%
  group_by(name) %>%
  mutate(value = scale(value)) %>%
  ungroup() %>%
  st_as_sf()

ggplot(long) +
  geom_sf(aes(fill = discreter(value, 5)), lwd = 0) +
  scale_fill_manual(values = scico(palette = 'lajolla', 5),
                    guide = guide_discrete,
                    labels = labeller(long$value, 5),
                    name = "deprivation score",
                    na.value = "green") +
  facet_wrap(~ name) +
  theme_map()

ggplot(data_spatial) + 
  geom_sf(aes(fill = discreter(ffood_d, 5)), lwd = 0) +
  scale_fill_manual(values = scico(palette = 'turku', 5),
                    guide = guide_discrete,
                    labels = labeller(deprivation$index_of_multiple_deprivation_imd_score, 5),
                    name = "fast food (d)") +
  theme_map()

ggplot(data_spatial) + 
  geom_sf(aes(fill = discreter(green900, 5)), lwd = 0) +
  scale_fill_manual(values = scico(palette = 'turku', 5),
                    guide = guide_discrete,
                    labels = labeller(deprivation$index_of_multiple_deprivation_imd_score, 5),
                    name = "green space (d)") +
  theme_map()

ggplot(data_spatial) + 
  geom_sf(aes(fill = discreter(pharm_d, 5)), lwd = 0) +
  scale_fill_manual(values = scico(palette = 'turku', 5),
                    guide = guide_discrete,
                    labels = labeller(deprivation$index_of_multiple_deprivation_imd_score, 5),
                    name = "pharmacy (d)") +
  theme_map()

##

library(spdep)
library(spgwr)

autocorrelating <-
  deprivation %>%
  drop_na(shape_are, shape_len) %>%
  st_as_sf()

coords <- 
  autocorrelating %>%
  st_centroid() %>%
  st_coordinates()

nearest <- knn2nb(knearneigh(coords, 5))
weights <- nb2listw(nearest, style = "W")

moranstest <- moran.test(autocorrelating$index_of_multiple_deprivation_imd_score, weights)
montecarlo <- moran.mc(autocorrelating$index_of_multiple_deprivation_imd_score, weights, nsim = 999)

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

moransi <- localmoran(autocorrelating$index_of_multiple_deprivation_imd_score, weights) %>% as_tibble()

autocorrelating <- 
  autocorrelating %>%
  st_transform(27700) %>%
  bind_cols(moransi) %>%
  rename(locali = Ii,
         expectation = E.Ii,
         variance = Var.Ii,
         deviation = Z.Ii,
         p_value = `Pr(z > 0)`)

background <- 
  st_read("data/authorities.shp", crs = 4326) %>%
  st_transform(27700) %>%
  filter(str_detect(code, "E|W|S")) %>%
  filter(code != "S12000027") %>%
  ms_simplify(0.05) %>%
  st_union() %>%
  st_combine()
  
plot(background)
plot(autocorrelating, add = TRUE)

map_moran_v <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = autocorrelating,
          aes(fill = factor(ntile(index_of_multiple_deprivation_imd_score, 5))), size = 0, colour = NA) +
  scale_fill_manual(values = scico(palette = 'cork', 5),
                    labels = as.character(quantile(autocorrelating$index_of_multiple_deprivation_imd_score,
                                                   c(.1,.2,.4,.6,.8),
                                                   na.rm = TRUE)),
                    name = "deprivation",
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

autocorrelating <- 
  autocorrelating %>%
  rename(dep = index_of_multiple_deprivation_imd_score) %>%
  mutate(scaled_dep = scale(dep)) %>%
  select(dep, scaled_dep, locali, expectation, variance, deviation, p_value) %>%
  mutate(lagged_dep = lag.listw(weights, scaled_dep),
         quad_sig = NA)

autocorrelating <-
  autocorrelating %>%
  mutate(quad_sig_1 = 
           case_when(scaled_dep >= 0 & lagged_dep >= 0 & p_value <= 0.01 ~ 1,
                     scaled_dep <= 0 & lagged_dep <= 0 & p_value <= 0.01 ~ 2,
                     scaled_dep >= 0 & lagged_dep <= 0 & p_value <= 0.01 ~ 3,
                     scaled_dep >= 0 & lagged_dep <= 0 & p_value <= 0.01 ~ 4,
                     scaled_dep <= 0 & lagged_dep >= 0 & p_value <= 0.01 ~ 5)) %>%
  mutate(quad_sig_2 = 
           case_when(scaled_dep >= 0 & lagged_dep >= 0 & p_value <= 0.05 ~ 1,
                     scaled_dep <= 0 & lagged_dep <= 0 & p_value <= 0.05 ~ 2,
                     scaled_dep >= 0 & lagged_dep <= 0 & p_value <= 0.05 ~ 3,
                     scaled_dep >= 0 & lagged_dep <= 0 & p_value <= 0.05 ~ 4,
                     scaled_dep <= 0 & lagged_dep >= 0 & p_value <= 0.05 ~ 5)) %>%
  mutate(quad_sig_3 = 
           case_when(scaled_dep >= 0 & lagged_dep >= 0 & p_value <= 0.1 ~ 1,
                     scaled_dep <= 0 & lagged_dep <= 0 & p_value <= 0.1 ~ 2,
                     scaled_dep >= 0 & lagged_dep <= 0 & p_value <= 0.1 ~ 3,
                     scaled_dep >= 0 & lagged_dep <= 0 & p_value <= 0.1 ~ 4,
                     scaled_dep <= 0 & lagged_dep >= 0 & p_value <= 0.1 ~ 5)) %>%
  st_as_sf()

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
  labs(title = "p < 0.01") +
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
  labs(title = "p < 0.05") +
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
  labs(title = "p < 0.1") +
  theme_map()  

p <- map_quads_1 + map_quads_2 + map_quads_3

ggsave(filename = "hotspots.png", height = 6, width = 10)
