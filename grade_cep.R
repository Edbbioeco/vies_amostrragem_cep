# Pacotes ----

library(sf)

library(tidyverse)

# Ácrea do CEP ----

## Importar ----

cep <- sf::st_read("cep.shp")

## Visualizar ----

cep

ggplot() +
  geom_sf(data = cep, color = "black")

# Grade do CEP ----

## Criar a grade ----

grade_cep <- cep |>
  sf::st_make_grid(cellsize = 0.0898316) |>
  sf::st_as_sf() |>
  sf::st_join(cep) |>
  tidyr::drop_na() |>
  dplyr::mutate(FID = 1:dplyr::n())

## Visualizar ----

grade_cep

ggplot() +
  geom_sf(data = grade_cep, color = "black") +
  geom_sf(data = cep, color = "red", fill = "transparent")
