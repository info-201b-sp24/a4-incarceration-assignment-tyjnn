# libraries
library(dplyr)
library(ggplot2)

# load data
pp_cs <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-pop.csv?raw=true")
jp_cs <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true")

# create main data frame of populations with most complete data
comp_data <- left_join(pp_cs, jp_cs) %>%
  na.omit() %>%
  filter(year == max(year)) %>%
  reframe(
    black_pop_pct = black_pop_15to64 / total_pop_15to64,
    black_inc_pct = (black_prison_pop + black_jail_pop) /
      (total_prison_pop + total_jail_pop)
  ) %>%
  filter(black_inc_pct <= 1)

# create chart
chart2 <- comp_data %>%
  ggplot(aes(x = black_pop_pct, y = black_inc_pct)) +
  geom_point() +
  xlim(0, 1) +
  ylim(0, 1) +
  labs(
    title = "Black Percentage of Total Population vs Incarcerated Population",
    x = "Percent of Total Population",
    y = "Percent of Incarcerated Population"
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_abline(color = "blue", intercept = 0, slope = 1)