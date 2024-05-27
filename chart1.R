# libraries
library(dplyr)
library(ggplot2)

# load data
pp_cs <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-pop.csv?raw=true")
jp_cs <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true")

# create data frame of populations with most complete data
inc_data <- left_join(pp_cs, jp_cs) %>%
  na.omit() %>%
  group_by(year) %>%
  summarise(
    aapi = sum(aapi_prison_pop + aapi_jail_pop) / sum(aapi_pop_15to64),
    black = sum(black_prison_pop + black_jail_pop) / sum(black_pop_15to64),
    latino = sum(latinx_prison_pop + latinx_jail_pop) / sum(latinx_pop_15to64),
    native = sum(native_prison_pop + native_jail_pop) / sum(native_pop_15to64),
    white = sum(white_prison_pop + white_jail_pop) / sum(white_pop_15to64)
  )

chart1 <- inc_data %>%
  ggplot(aes(year)) +
  geom_line(aes(y = aapi, colour = "Asian or\nPacific Islander")) +
  geom_line(aes(y = black, colour = "Black")) +
  geom_line(aes(y = latino, colour = "Latino")) +
  geom_line(aes(y = native, colour = "Native")) +
  geom_line(aes(y = white, colour = "White")) +
  labs(
    title = "Percent of Population Incarcerated Over Time Per Race",
    x = "Year (1990 - 2016)",
    y = "Percent of Population",
    colour = "Racial Group"
  ) +
  scale_y_continuous(labels = scales::percent_format())