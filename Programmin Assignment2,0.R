#Reading the CBS Data
data <- read.csv("~/GitHub/Housing-2.0/Programming.csv")
data2 <- read.csv("~/GitHub/Housing-2.0/Programming2.csv")
summary(data)
summary(data2)

#Installing Packages
install.packages("sf")
install.packages("cbsodataR")
install.packages("tidyverse")
library(tidyr)
library(dplyr)
library(ggplot2)
library(cbsodataR)
library(sf)

#Data Cleaning
Provence_blocks <- list()

for (i in seq(1, ncol(data), by = 4)) {
  temp <- data[, c(i, i + 1, i +2)]
  names(temp) <- c("Provence", "total_houses", "period")
  Provence_blocks[[i]] <- temp
}

data2$Provence[data2$Provence == "Friesland"] <- "Fryslân"

data2$Total.Houses <- as.numeric(gsub("\\.","", data2$Total.Houses))

data_long3 <- bind_rows(Provence_blocks)

data_long3 <- data_long3 %>%
  filter(!is.na(Provence), !is.na(total_houses), !is.na(period))

data_long3 <- data_long3 %>%
  arrange(Provence, period) %>%
  group_by(Provence) %>%
  mutate(House_growth = total_houses - lag(total_houses))

data_long3 <- data_long3 %>%
  arrange(Provence, period) %>%
  group_by(Provence) %>%
  mutate(total_houses_added = sum(House_growth, na.rm = TRUE))


data_long3 <- data_long3 %>%
  mutate(Percent_growth = (House_growth / lag(total_houses)) * 100)


#Plotting a Line Graph
ggplot(data_long3, aes(x = period, y = House_growth, color = Provence)) +
  geom_line(size = 0.5) +
  labs(
    title = "Housing Supply Over Time by Dutch Provence",
    x = "Year",
    y = "House Growth",
    color = "Provence"
  ) +
  theme_minimal()

#Plotting the Dutch Provence Map
prov_map <- cbs_get_sf(region = "provincie", year = 2023)

future_data <- data2 |>
  rename(statnaam = Provence,   
         supply_value = Total.Houses)   

future_map <- left_join(prov_map, future_data, by = "statnaam")

ggplot(future_map) +
  geom_sf(aes(fill = supply_value)) +
  labs(title = "Estimated Housing Supply (2023-2030)", fill = "supply") + 
  theme_void()