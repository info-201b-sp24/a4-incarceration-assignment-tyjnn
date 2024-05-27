# libraries
library(dplyr)

# load data
pp_cs <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-pop.csv?raw=true")
jp_cs <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true")

# create main data frame of populations with most complete data
data <- left_join(pp_cs, jp_cs) %>%
  na.omit() %>%
  group_by(year) %>%
  summarise(
    total_pop = sum(total_pop_15to64),
    aapi_pop = sum(aapi_pop_15to64),
    black_pop = sum(black_pop_15to64),
    latino_pop = sum(latinx_pop_15to64),
    native_pop = sum(native_pop_15to64),
    white_pop = sum(white_pop_15to64),
    total_inc = sum(total_prison_pop + total_jail_pop),
    aapi_inc = sum(aapi_prison_pop + aapi_jail_pop),
    black_inc = sum(black_prison_pop + black_jail_pop),
    latino_inc = sum(latinx_prison_pop + latinx_jail_pop),
    native_inc = sum(native_prison_pop + native_jail_pop),
    white_inc = sum(white_prison_pop + white_jail_pop),
  )

# store most recent year in data for easy access
rct_yr <- data %>% filter(year == max(year)) %>% pull(year)

# get percent of each race's population and of total population incarcerated
recent_inc_pct <- data %>%
  filter(year == rct_yr) %>%
  summarize(
    total_pct = total_inc / total_pop,
    aapi_pop_pct = aapi_inc / aapi_pop,
    black_pop_pct = black_inc / black_pop,
    latino_pop_pct = latino_inc / latino_pop,
    native_pop_pct = native_inc / native_pop,
    white_pop_pct = white_inc / white_pop,
    aapi_ttl_pct = aapi_inc / total_inc,
    black_ttl_pct = black_inc / total_inc,
    latino_ttl_pct = latino_inc / total_inc,
    native_ttl_pct = native_inc / total_inc,
    white_ttl_pct = white_inc / total_inc
  )


# VALUE 1.1: TOTAL % OF POPULATION INCARCERATED (MOST RECENT YEAR)
total_pct <- recent_inc_pct %>%
  pull(total_pct) %>%
  round(digits = 4) * 100

# VALUE 1.2: SMALLEST RACE INCARCERATED / POPULATION RATIO (MOST RECENT YEAR)
min_pct <- recent_inc_pct %>%  # aapi
  select(2:6) %>%
  min() %>%
  round(digits = 4) * 100

# VALUE 1.3: LARGEST RACE INCARCERATED / POPULATION RATIO (MOST RECENT YEAR)
max_pct <- recent_inc_pct %>%  # black
  select(2:6) %>%
  max() %>%
  round(digits = 4) * 100


# get percentage of total population made up by each race (most recent year)
recent_pop_pct <- data %>%
  filter(year == rct_yr) %>%
  summarise(across(3:7, ~ . / total_pop))


# VALUE 2: % OF TOTAL POPULATION BLACK AMERICANS (MOST RECENT YEAR
black_pop_pct <- recent_pop_pct %>%
  pull(black_pop) %>%
  round(digits = 4) * 100


# VALUE 3: % OF PRISON POPULATION BLACK AMERICANS (MOST RECENT YEAR
black_inc_pct <- recent_inc_pct %>%
  pull(black_ttl_pct) %>%
  round(digits = 4) * 100


# find year of highest incarceration per race
max_year <- data %>%
  reframe(across(9:13, \(x)(filter(., x == max(x)) %>% select(year) %>% pull())))


# VALUE 4: YEAR OF MOST BLACK INCARCERATIONS
max_black_yr <- max_year %>%
  pull(black_inc)


# VALUE 5.1: MOST BLACK AMERICANS INCARCERATED IN A YEAR
max_black_inc <- data %>%
  filter(year == max_black_yr) %>%
  pull(black_inc) %>%
  round() %>%
  format(scientific = FALSE)

# VALUE 5.2: WHITE INCARCERATED POPULATION SAME YEAR
white_inc_same_yr <- data %>%
  filter(year == max_black_yr) %>%
  pull(white_inc) %>%
  round() %>%
  format(scientific = FALSE)