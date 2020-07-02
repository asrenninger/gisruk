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
