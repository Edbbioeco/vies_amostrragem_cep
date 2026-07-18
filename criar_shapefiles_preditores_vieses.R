# Pacotes ----

library(sf)

library(geobr)

library(tidyverse)

library(osmextract)

# Shapefile do CEP ----

## Importar ----

cep <- sf::st_read("cep.shp")

## Visualizar ----

cep

ggplot() +
  geom_sf(data = cep, color = "black")

# Unidades de conservação ----

## Importar ----

uc <- geobr::read_conservation_units(date = "202503")

## Visualiza ----

uc

ggplot() +
  geom_sf(data = uc, color = "darkgreen", fill = "forestgreen") +
  geom_sf(data = cep, color = "red", fill = "transparent")

## Recortar para o CEP ----

uc_rec <- uc |>
  sf::st_intersection(cep)

uc_rec

ggplot() +
  geom_sf(data = uc_rec, color = "darkgreen", fill = "forestgreen") +
  geom_sf(data = cep, color = "red", fill = "transparent")

## Exportar o shapefile ----

uc_rec |> sf::st_write("unidade_conservacao_cep.shp")
