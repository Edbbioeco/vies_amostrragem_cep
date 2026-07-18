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

# Áreas urbanas ----

## Importar ----

areas_urb <- geobr::read_urban_area(year = 2019)

# Rodovias ----

## Importar rodovias ----

purrr::map(list.files(pattern = ".zip$"),
           purrr::in_parallel(

             ~unzip(.x,
                    exdir = "./rodovias")

           ),
           .progress = TRUE)

rodovias <- purrr::map(list.files(path = "./rodovias",
                                  pattern = ".shp$",
                                  full.names = TRUE),
                       ~sf::st_read(.x)) |>
  dplyr::bind_rows()

## Visualizar ----

rodovias

ggplot() +
  geom_sf(data = rodovias)

## Recortar para o CEP ----

rodovias_cep <- rodovias |>
  sf::st_intersection(cep)

rodovias_cep

ggplot() +
  geom_sf(data = uc_rec, color = "darkgreen", fill = "forestgreen") +
  geom_sf(data = rodovias_cep, color = "black") +
  geom_sf(data = cep, color = "red", fill = "transparent")

## Exportar shapefile ----

rodovias_cep |> sf::st_write("rodovias.shp")
