library(tidyverse)
library(treemapify)
library(countrycode)
library(gganimate)
library(magick)
# devtools::install_github("PPgp/wpp2022")
library(wpp2022)

# load data
data(pop1dt) 
data(popproj1dt)
data(UNlocations)

# format data for plot
d <- pop1dt %>%
  bind_rows(popproj1dt) %>%
  left_join(UNlocations) %>%
  filter(country_code < 900,
         year >= 1950) %>%
  select(1:3, pop, reg_name, area_name) %>%
  mutate(
    name_short = countrycode(sourcevar = country_code, origin = "iso3n", destination = "country.name"),
    # edit kosovo, DRC and mynmar names
    name_short = ifelse(country_code == 412, "Kosovo", name_short),
    name_short = ifelse(country_code == 180, "DR Congo", name_short),
    name_short = ifelse(country_code == 104, name, name_short)
  ) 
d

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
  filter(year == 2022) %>%
  pop_tree() 

# to get pause at 2022 need to create two animations and then combine as 
# animate() does not accept frame specific pauses 

# animations
a1 <- d %>%
  filter(year <= 2022) %>%
  pop_tree() +
  transition_time(time = year) +
  labs(title = 'Global Population Distribution: {round(frame_time)}', fill = "Region",
       caption = "Data Reference: United Nations, Department of Economic and Social Affairs, Population Division (2022). World Population Prospects 2022. Plot by @guyabelguyabel")

a2 <- d %>%
  filter(year >= 2022) %>%
  pop_tree() +
  transition_time(time = year) +
  labs(title = 'Global Population Distribution: {round(frame_time)} - Median Projection', fill = "Region",
       caption = "Data Reference: United Nations, Department of Economic and Social Affairs, Population Division (2022). World Population Prospects 2022. Plot by @guyabelguyabel")

# write animations to gif files. one frame per year, except first and last
a1 %>%
  animate(width = 1200, height = 720, 
          start_pause = 40, end_pause = 20, 
          nframes = 60 + 2022 - 1950, fps = 20) %>%
  anim_save(filename = "pop1.gif")

a2 %>%
  animate(width = 1200, height = 720, 
          start_pause = 20, end_pause = 40, 
          nframes = 60 + 2100 - 2022, fps = 20) %>%
  anim_save(filename = "pop2.gif")

# read in gif images then combine
p1 <- image_read(path = "pop1.gif")
p2 <- image_read(path = "pop2.gif")
p <- c(p1, p2)
image_write(image = p, path = "pop.gif")

# image_write(p1[72], path = "animated-treemap.png")

# new UN API code taking too long to get the data...
# base_url = 'https://population.un.org/dataportalapi/api/v1'
# cc <- paste0(base_url, '/locations?sort=id&format=csv') %>%
#   read_delim(skip = 1) %>%
#   filter(Id < 900) %>%
#   pull(Id)
# d <- paste0(base_url,
#             '/data/indicators/', 49,
#             '/locations/', cc,
#             '/start/', 1950,
#             '/end/', 2100,
#             '/?format=csv') %>%
#   read_delim(skip = 1)