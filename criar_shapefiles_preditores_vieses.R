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

## Localização das universidades ----

## Importar ----

estados_cep <- c("Pernambuco",
                 "Alagoas",
                 "Paraiba",
                 "Rio Grande do Norte")

universidades_estados <- purrr::map(estados_cep,
                                    purrr::in_parallel(
        ~osmextract::oe_get(
          place = .x,
          provider = "geofabrik",
          layer = "points",
          extra_tags = c("amenity", "office", "name"),
          query = "SELECT * FROM points WHERE
        amenity IN ('university', 'college') OR
        office = 'educational_institution' OR
        name LIKE '%Universidade Federal%' OR
        name LIKE '%UFPE%' OR name LIKE '%UFRPE%' OR name LIKE '%UFAL%' OR
        name LIKE '%UFPB%' OR name LIKE '%UFRN%' OR name LIKE '%UFC%'",
          force_download = FALSE,
          quiet = TRUE

          )
        ),
        .progress = TRUE) |>
  dplyr::bind_rows()

## Visualizar ----

universidades_estados

ggplot() +
  geom_sf(data = uc_rec, color = "darkgreen", fill = "forestgreen") +
  geom_sf(data = cep, color = "red", fill = "transparent") +
  geom_sf(data = universidades_estados, color = "black")
