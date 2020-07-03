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
  scale_fill_manual(values = scico(palette = 'lajolla', 10),
                    guide = guide_discrete,
                    labels = labeller(deprivation$index_of_multiple_deprivation_imd_score, 5),
                    name = "deprivation score") +
  theme_map()


