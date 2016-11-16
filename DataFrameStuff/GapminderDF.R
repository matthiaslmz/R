install.packages("gapminder")
library(dplyr)
library(gapminder)
library(reshape2)
library(tidyr)
summary(gapminder)
head(gapminder)
aggregate(lifeExp ~ continent, gapminder, mean)

gapminder %>%
  filter(year >= 1952) %>%
  group_by(country) %>%
  summarise(gdpPercap = mean(gdpPercap))

gdp.delta <- gapminder %>%
  select(year, country, gdpPercap, life) %>%
  filter(year >= 1952) %>%
  group_by(country, year) %>%
  summarise(gdpPercap)


gdp.median <- dcast(gdp.delta, country ~ year, value.var="gdpPercap")

#library(tidyr)
#gapminder %>% 
  #select(country, year, gdpPercap, lifeExp) %>% 
  #spread(year, gdpPercap)
