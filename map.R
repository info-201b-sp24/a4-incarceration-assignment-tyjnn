# libraries
library(dplyr)
library(ggplot2)
library(maps)
library(mapproj)

# load data
pp_cs <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-pop.csv?raw=true")
jp_cs <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true")

# create data frame of percentages
map_data <- left_join(pp_cs, jp_cs) %>%
  filter(year == max(year)) %>%
  replace(., is.na(.), 0) %>%
  reframe(
    fips = fips,
    "Asian or\nPacific Islander" = (aapi_prison_pop + aapi_jail_pop) /
      (total_prison_pop + total_jail_pop),
    "Black" = (black_prison_pop + black_jail_pop) /
      (total_prison_pop + total_jail_pop),
    "Latino" = (latinx_prison_pop + latinx_jail_pop) /
      (total_prison_pop + total_jail_pop),
    "Native" = (native_prison_pop + native_jail_pop) /
      (total_prison_pop + total_jail_pop),
    "White" = (white_prison_pop + white_jail_pop) /
      (total_prison_pop + total_jail_pop),
  ) %>%
  na.omit() %>%
  mutate(
    `Largest % of Incarcerated Population` = colnames(.[2:6])[max.col(.[2:6])]
  )

# get county by fips
map_data <- inner_join(
  map_data,
  get("county.fips"),
  by = join_by(fips == fips)
) %>%
  mutate(
    region = sub(",.*", "", polyname),
    subregion = sub(".*,", "", polyname),
  ) %>%
  select(7, 9:10)

# join with long / lat values
county_shape <- map_data("county") %>%
  left_join(map_data, by = join_by(region == region, subregion == subregion))

# create map of data
map <- ggplot(county_shape) +
  geom_polygon(
    mapping = aes(
      x = long,
      y = lat,
      group = group,
      fill = `Largest % of Incarcerated Population`)
    ,
    color = "white",
    size = .1
  ) +
  coord_map() +
  labs(
    title = "Largest Incarcerated Population per County",
    x = "Longitude",
    y = "Latitude"
  )