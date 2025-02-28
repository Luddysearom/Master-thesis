packages <- c("openxlsx", "tidyverse", "sf", "ggforce", "ggsci", "patchwork")

install_and_load_packages <- function(packages) {
  for(package in packages) {
    if(!require(package, character.only = TRUE)) {
      install.packages(package)
      library(package, character.only = TRUE)
    }
  }
}

install_and_load_packages(packages)

data <- read.xlsx("All_data.xlsx")

# "World Countries". Downloaded from http://tapiquen-sig.jimdo.com. 
# Carlos Efraín Porto Tapiquén. Orogénesis Soluciones Geográficas. Porlamar, Venezuela 2015.
# Based on shapes from Enviromental Systems Research Institute (ESRI). Free Distribuition.
countries <- read_sf("World_Countries.shp")
countries$COUNTRY[countries$COUNTRY == "New Caledonia (France)"] <- "New Caledonia" # just adjusting this country name

# Changing some country names in my dataset so it will fit with the "countries" object
data_map <- data  |> 
  mutate(country = gsub("USA", "United States", country)) |> 
  mutate(country = gsub("(England|Scotland|Wales|Ireland|Isle of Man)", "United Kingdom", country)) |> 
  rename(COUNTRY = country) |> 
  filter(COUNTRY != "Not applicable")

## uma ideia que eu tive eh esse bloco de codigo abaixo. Ele faz um barplot com a porcentagem que determinado pais teve de invasao. Ele mostra EUA com mais invasoes. Nao sei se eh importante.
data_map  |>
  group_by(COUNTRY)  |> 
  summarise(n = n())  |> 
  mutate(percent = n / sum(n) * 100) |> 
  ggplot(aes(x = reorder(COUNTRY, percent), y = percent)) +
  geom_col(color = "black", fill = "darkred") +
  labs(x = NULL, y = "Registers percent") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 25)) +
  coord_flip() +
  theme_bw(base_size = 15)

#Agora por número de espécies:
data_map  |>
  group_by(COUNTRY, psp, Spillmode)  |> 
  summarise(n = n(), .groups = "drop")  |>
  group_by(COUNTRY, Spillmode) |> 
  summarise(n = n()) |>
  ggplot(aes(x = reorder(COUNTRY, n), y = n, fill = Spillmode)) +
  geom_col(color = "black", position = "dodge") +
  labs(x = NULL, y = "Number of species") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 30)) +
  coord_flip() +
  scale_fill_jco() +
  theme_bw(base_size = 15)

# percent of spillmode by country:
spillcount <- data_map |>  
  count(Spillmode, COUNTRY) |> 
  group_by(COUNTRY) |> 
  mutate(percent = n / sum(n) * 100)

country_coords <- countries |>
  full_join(spillcount, join_by(COUNTRY)) |> 
  mutate(
    n = ifelse(is.na(n), 0, n),
    percent = ifelse(is.na(percent), 0, percent)
  )

# Taking the latitude and longitude for the center of each country (which will be the piechart positions)
centroids <- st_centroid(country_coords$geometry)
country_coords$lat <- st_coordinates(centroids)[,2]
country_coords$long <- st_coordinates(centroids)[,1]

# Adjust the USA and Canada coordinates was needed, so the piechart was displayied in its center +
# Adding the cummulative sum and total sum of the number of registers +
# Adding the start and end points of the charts.
country_coords <- country_coords |> 
  mutate(
    lat = ifelse(COUNTRY == "United States", 38.5, lat),
    long = ifelse(COUNTRY == "United States", -95.7129, long),
    lat = ifelse(COUNTRY == "Canada", 59.4371435, lat),
    long = ifelse(COUNTRY == "Canada", -105, long),
    cumsum = ave(n, Spillmode, FUN = cumsum),
    total = ave(n, Spillmode, FUN = sum),
    start = 2 * pi * cumsum(percent) / 100 - 2 * pi * percent / 100,
    end = 2 * pi * cumsum(percent) / 100,
    sum_n = ave(n, COUNTRY, FUN = sum)
  )

# world plot 
map_records <- ggplot() +
  geom_sf(data = country_coords, fill = "white") +
  geom_arc_bar(
    data = filter(country_coords, !is.na(Spillmode)), 
    aes(x0 = long, y0 = lat, r0 = 0, r = 1.5*log(sum_n+1),
        start = start,
        end = end,
        fill = Spillmode), 
    alpha = 0.7
  ) +
  scale_fill_jco(guide = guide_legend(nrow = 1)) +
  theme_void(base_size = 15) +
  theme(legend.position = "top") +
  guides(fill = guide_legend(title = NULL)) +
  labs(subtitle = "       A)")
map_records








# percent of spillmode by country:
spillcount2 <- data_map |>  
  group_by(COUNTRY, psp, Spillmode) |> 
  summarise(n = n(), .groups = "drop") |> 
  group_by(COUNTRY, Spillmode) |> 
  summarise(n = n(), .groups = "drop") |> 
  group_by(COUNTRY) |> 
  mutate(percent = n / sum(n) * 100) |> 
  ungroup()


country_coords2 <- countries |>
  full_join(spillcount2, join_by(COUNTRY)) |> 
  mutate(
    n = ifelse(is.na(n), 0, n),
    percent = ifelse(is.na(percent), 0, percent)
  )

# Taking the latitude and longitude for the center of each country (which will be the piechart positions)
centroids2 <- st_centroid(country_coords2$geometry)
country_coords2$lat <- st_coordinates(centroids2)[,2]
country_coords2$long <- st_coordinates(centroids2)[,1]

# Adjust the USA and Canada coordinates was needed, so the piechart was displayied in its center +
# Adding the cummulative sum and total sum of the number of registers +
# Adding the start and end points of the charts.
country_coords2 <- country_coords2 |> 
  mutate(
    lat = ifelse(COUNTRY == "United States", 38.5, lat),
    long = ifelse(COUNTRY == "United States", -95.7129, long),
    lat = ifelse(COUNTRY == "Canada", 59.4371435, lat),
    long = ifelse(COUNTRY == "Canada", -105, long),
    cumsum = ave(n, Spillmode, FUN = cumsum),
    total = ave(n, Spillmode, FUN = sum),
    start = 2 * pi * cumsum(percent) / 100 - 2 * pi * percent / 100,
    end = 2 * pi * cumsum(percent) / 100,
    sum_n = ave(n, COUNTRY, FUN = sum)
  ) 

# world plot 
map_species <- ggplot() +
  geom_sf(data = country_coords2, fill = "white") +
  geom_arc_bar(
    data = filter(country_coords2, !is.na(Spillmode)), 
    aes(x0 = long, y0 = lat, r0 = 0, r = 2*log(sum_n+1),
        start = start,
        end = end,
        fill = Spillmode), 
    alpha = 0.7
  ) +
  scale_fill_jco() +
  theme_void(base_size = 15) +
  theme(legend.position = "none") +
  guides(fill = guide_legend(title = NULL)) +
  labs(subtitle = "       B)")
map_species

map_records / map_species + plot_annotation(title = "       World map of parasites spilling over/back by amount of records (A) and number of species (B)")
