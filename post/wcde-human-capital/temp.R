library(tidyverse)
library(wcde)
library(countrycode)
library(gganimate)

g20 <- c("Australia", "Canada", "Saudi Arabia", "United States",
         "India", "Russia", "South Africa", "Turkey",  
         "Argentina", "Brazil", "Mexico",
         "France", "Germany", "Italy", "United Kingdom",
         "China", "Indonesia", "Japan", "South Korea")
          
g20_codes <- countrycode(sourcevar = g20, origin = "country.name", destination = "iso3n")

d0 <- past_epop %>%
  filter(country_code %in% g20_codes) %>%
  edu_group_sum(n = 6) %>%
  group_by(name, year, education) %>%
  summarise(epop = sum(epop))

n <- d0 %>%
  filter(year == 2020) %>%
  group_by(name) %>%
  mutate(p_u15 = epop[5:6]/sum(epop)) %>%
  arrange(p_u15) %>%
  distinct(name) %>%
  pull(name) %>%
  as.character()

g20_flags <-countrycode(sourcevar = g20, 
                        origin = "country.name", 
                        destination = "unicode.symbol")

library(ggtext)

d0 %>%
  mutate(name = factor(name, levels = n)) %>%
  arrange(name) %>%
  mutate(name = str_remove(string = name, pattern = " of Great Britain and Northern Ireland"),
         name = fct_inorder(name)
  ) %>%
  filter(year == 2020) %>%
  ggplot(mapping = aes(x = epop, y = name, 
                       fill = fct_rev(education))) +
  geom_bar(position = "fill", stat = "identity") +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(values = wic_col6, name = "Education") +
  scale_y_discrete(labels = g20_flags) +
  theme(
    axis.text.y = element_markdown()
  )
  
  
  transition_time(time = year) +
  labs(x = "Share of population", y = "",
       title = 'Education Distribuion {round(frame_time)} of G20 nations') +
  theme_bw()



d <- get_wcde(indicator = "epop", country_name = g20, scenario = 1:3)


d1 <- d %>%
  edu_group_sum(n = 6, strip_totals = FALSE) %>%
  filter(age == "All",
         sex == "Both",
         education != "Total")  %>%
  left_join(wic_scenarios)

d1 %>%
  mutate(name = factor(name, levels = n)) %>%
  arrange(name) %>%
  mutate(name = str_remove(string = name, pattern = " of Great Britain and Northern Ireland"),
         name = fct_inorder(name)) %>%
  # filter(year == 2100) %>%
  ggplot(mapping = aes(x = epop, y = name, 
                       fill = fct_rev(education))) +
  facet_wrap(facets = "scenario_name", nrow = 1) +
  geom_bar(position = "fill", stat = "identity") +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(values = wic_col6, name = "Education") +
  transition_time(time = year) +
  labs(x = "Share of population", y = "",
       title = 'Education Distribuion {round(frame_time)} of G20 nations') +
  theme_bw()


library(gganimate)
x %>%
  mutate(name = factor(name, levels = n)) %>%
  arrange(name) %>%
  mutate(name = str_remove(string = name, pattern = " of Great Britain and Northern Ireland"),
         name = fct_inorder(name)
  ) %>%
  ggplot(mapping = aes(x = epop, y = name, 
                       fill = fct_rev(education))) +
  geom_bar(position = "fill", stat = "identity") +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(values = wic_col6, name = "Education") +
  transition_time(time = year) +
  labs(x = "Share of population", y = "",
       title = 'G20 Education Distribuion {round(frame_time)}')
  theme_bw()



  

library(geofacet)
g <- africa_countries_grid1 %>%
  mutate(iso_no = countrycode(sourcevar = name, origin = "country.name", destination = "iso3n")) %>%
  rename(name_grid = name) %>%
  select(name_grid, iso_no)


x <- x %>%
  mutate(iso_no = countrycode(sourcevar = name, origin = "country.name", destination = "iso3n")) %>%
  left_join(g)

x = d %>%
  select(contains("name"), "iso_no") %>%
  distinct()

# library(lemon)
library(gganimate)
x %>%
  filter(year == 1950) %>%
  drop_na() %>%
  ggplot(mapping = aes(x = pop, y = age, fill = fct_rev(education))) +
  facet_geo(facets = "name_grid", grid = africa_countries_grid1) +
  geom_col(position="fill") +
  geom_vline(xintercept = 0, colour = "black") +
  scale_x_continuous(labels = abs) +
  # scale_x_symmetric(labels = abs) +
  scale_y_discrete(breaks = every_other(levels(d$age), n = 4)) +
  scale_fill_manual(values = wic_col6, name = "Education") +
  # transition_time(time = year) +
  labs(x = "Population (millions)", y = "Age") +
  theme_bw()
ggsave(filename = "temp.pdf", height = 10, width = 10)
file.show("temp.pdf")

geofacet::africa_countries_grid1

