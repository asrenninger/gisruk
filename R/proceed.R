source("R/help.R")
source("R/package.R")


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
  st_simplify()

background <- 
  authorities %>%
  st_union() %>%
  st_combine()

##

plot(background)

##







