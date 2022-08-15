library(tidyverse)
library(patchwork)
library(usmap)
library(scales)
library(plotly)

incarceration_trends <- read.csv("incarceration_trends.csv")
inctrend16 <- filter(incarceration_trends, year == 2016)
inctrendwa <- filter(incarceration_trends, state == "WA")
inctrend3 <- filter(inctrendwa, fips == 53033 | fips == 53061 | fips == 53053)
inctrend3 <- filter(inctrend3, year > 1989 & year < 2017)
inctrendkingco <- filter(inctrendwa, fips == 53033 & year > 1989 & year < 2017)
inctrendsnoco <- filter(inctrendwa, fips == 53061)
inctrendpierceco <- filter(inctrendwa, fips == 53053)

# Summary Values

mean_whiterate2016 <- mean(na.omit(inctrend16$white_prison_pop_rate))
median_whiterate2016 <- median(na.omit(inctrend16$white_prison_pop_rate))
mean_blackrate2016 <- mean(na.omit(inctrend16$black_prison_pop_rate))
median_blackrate2016 <- median(na.omit(inctrend16$black_prison_pop_rate))
high_black_2016 <- max(na.omit(inctrend16$black_prison_pop_rate))
high_white_2016 <- max(na.omit(inctrend16$white_prison_pop_rate))

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

# Trends Over Time

# 1990 to now
kingco_chart <- ggplot(inctrendkingco, aes(x = year)) + 
                  geom_line(aes(y = black_prison_pop_rate, color = "Black")) +
                  geom_line(aes(y = white_prison_pop_rate, color = "White")) +
                  labs(x = "Year", y = "Incarcerated per 100,000", 
                       title = "Incarceration by race in King County, WA", 
                       caption = "Data from the Vera Institute of Justice") +
                  theme_bw() + theme(legend.position = "right") +
                  scale_color_manual(name = "Race", values = c("Black" = "blue", 
                                                               "White" = "darkgreen"))

# Relational
wa_chart <- ggplot(inctrend3, aes(x = year)) +
              geom_line(aes(y = black_prison_pop_rate, color = "county_name"))

# Maps


usablackmap2016 <- plot_usmap(data = filter(incarceration_trends, year == 2016), 
                         values = "black_prison_pop_rate",
                         regions = "counties", color = "white", 
                         size = 0) +
                   scale_fill_continuous(low = "blue", high = "red", 
                                         name = "Black prison population rate per 100,000", 
                                         label = scales::comma, limits = c(0, 40000)) 
  
usawhitemap2016 <- plot_usmap(data = filter(incarceration_trends, year == 2016), 
                              values = "white_prison_pop_rate",
                              regions = "counties", color = "white", 
                              size = 0) +
                   scale_fill_continuous(low = "blue", high = "red", 
                                         name = "White prison population rate per 100,000", 
                                         label = scales::comma, limits = c(0, 40000)) 
