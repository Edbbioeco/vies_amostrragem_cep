# Pacotes ----

library(sf)

library(geobr)

library(tidyverse)

library(osmdata)

# Shapefile do CEP ----

## Importar ----

cep <- sf::st_read("cep.shp")

## Visualizar ----

cep

ggplot() +
  geom_sf(data = cep, color = "black")
