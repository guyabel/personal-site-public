library(tidyverse)
library(treemapify)
library(countrycode)
library(gganimate)
library(magick)
library(wcde)

# download data
hc <- get_wcde(indicator = "pop", pop_edu = "four") 

# format data for plot
d <- hc %>%
  filter(education %in% c("Secondary ","Post Secondary"),
         country_code < 900) %>%
  group_by(name, country_code, year) %>%
  summarise(pop = sum(pop), .groups = "drop") %>%
  left_join(wic_locations) %>%
  rename(reg_name = region, 
         area_name = continent) %>%
  select(1:4, reg_name, area_name) %>%
  mutate(
    name_short = countrycode(sourcevar = country_code, origin = "iso3n", destination = "country.name"),
    # edit DRC and channel island name
    name_short = ifelse(country_code == 180, "DR Congo", name_short),
    name_short = ifelse(country_code == 830, name, name_short)
  ) %>%
  arrange(area_name, reg_name)
d

# pop_tree function - identical to pop_tree() used in UN data plot
pop_tree <- function(dd){
  dd %>%
    ggplot(mapping = aes(
      area = pop, subgroup = area_name, label = name_short, fill = fct_inorder(reg_name)
    )) +
    geom_treemap(layout = "fixed") +
    geom_treemap_subgroup_border(layout = "fixed") +
    geom_treemap_text(
      layout = "fixed", colour = "white", reflow = TRUE, min.size = 1
    ) +
    guides(fill =  "none") +
    theme(plot.title = element_text(size = 20),
          plot.subtitle = element_text(size = 15),
          plot.caption =  element_text(size = 15))
}

# function working on a single year
d %>%
  filter(year == 2020) %>%
  pop_tree() 

# to get pause at 2022 need to create two animations and then combine as 
# animate() does not accept frame specific pauses 

# animations
a1 <- d %>%
  filter(year <= 2020) %>%
  pop_tree() +
  transition_time(time = year) +
  labs(title = 'Global Human Capital Distribution: {round(frame_time)}', 
       subtitle = "Based on population with a completed secondary education",
       caption = "Data Reference: Wittgenstein Centre for Demography and Global Human Capital (WIC) Wittgenstein Centre Data Explorer. Version 2.0, 2018. Plot by @guyabelguyabel") 

a2 <- d %>%
  filter(year >= 2020) %>%
  pop_tree() +
  transition_time(time = year) +
  labs(title = 'Global Human Capital Distribution: {round(frame_time)} - SSP2 Projection', 
       subtitle = "Based on population with a completed secondary education",
       caption = "Data Reference: Wittgenstein Centre for Demography and Global Human Capital (WIC) Wittgenstein Centre Data Explorer. Version 2.0, 2018. Plot by @guyabelguyabel") 

# write animations to gif files. one frame per year, except first and last
a1 %>%
  animate(width = 1200, height = 720, 
          start_pause = 40, end_pause = 20, 
          nframes = 60 + 2020 - 1950, fps = 20) %>%
  anim_save(filename = "hc1.gif")

a2 %>%
  animate(width = 1200, height = 720, 
          start_pause = 20, end_pause = 40, 
          nframes = 60 + 2100 - 2020, fps = 20) %>%
  anim_save(filename = "hc2.gif")

# read in gif images then combine
h1 <- image_read(path = "hc1.gif")
h2 <- image_read(path = "hc2.gif")
h <- c(h1, h2)
image_write(image = h, path = "hc.gif")

# file.remove(c("hc1.gif", "hc2.gif", "hc.gif"))