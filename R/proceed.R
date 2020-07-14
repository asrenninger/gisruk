source("R/help.R")
source("R/package.R")

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

##

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
  labs(title = "national projections {closest_state}",
       subtitle = "PROJECTED POPULATION BY AGE",
       x = "age bracket", y = "population") +
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
  labs(title = "national projections {closest_state}",
       subtitle = "PROJECTED POPULATION BY AGE",
       x = "age bracket", y = "population") +
  coord_flip() +
  theme_pop()

ggsave(age_2016, filename = "age2016.png", height = 5, width = 5, dpi = 300)
ggsave(age_2036, filename = "age2036.png", height = 5, width = 5, dpi = 300)

## Data
expectancy <- 
  read_xls("data/hackathon/healthy.xls", skip = 10, sheet = 1) %>%
  clean_names()

##

library(magrittr)
library(glue)

##

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

## Figure 1
ggplot(bind_rows(select(expectancy, 
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
  geom_text(aes(x = 75, y = 70, label = glue("r-squared = {male}")), hjust = 0) +
  geom_text(aes(x = 80, y = 50, label = glue("r-squared = {female}")), hjust = 0) +
  scale_colour_manual(values = c(scico(palette = "grayC", 10)[3], 
                                 scico(palette = "grayC", 10)[10])) +
  labs(title = "local authorities",
       subtitle = "TWIN LIFE EXPECTANCIES") +
  theme_ver()

##

lm(healthy_life_expectancy_for_females_2009_2013_years ~ life_expectancy_at_birth_for_females_2009_2013, 
   data = expectancy) %>%
  summary() %>%
  use_series("coef")

lm(healthy_life_expectancy_for_males_2009_2013 ~ life_expectancy_at_birth_for_males_2009_2013, 
     data = expectancy) %>%
  summary() %>%
  use_series("coef")

## Spatial data
authorities <-
  st_read("data/authorities.shp") %>%
  clean_names() %>%
  rename(area = st_rshp,
         leng = st_lngt) %>%
  select(code, area, leng) %>%
  filter(str_detect(code, "E|W|S")) %>%
  filter(code != "S12000027") %>%
  ms_simplify(keep = 0.005)

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

## Data
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

## Regressing

## Moransiing


##

box <- 
  st_transform(background, 27700) %>% 
  st_bbox() %>%
  st_as_sfc() %>%
  st_make_grid(cellsize = 50000) %>%
  st_as_sf() %>%
  rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname))

##

plot(box)

##









