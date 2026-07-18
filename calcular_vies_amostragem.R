# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

library(sampbias)

library(terra)

library(tidyterra)

library(ggtext)

library(ggview)

# Dados ----

## Registros de ocorrência ----

### Importar ----

registros <- readxl::read_xlsx("registros.xlsx")

### Visualizar ----

registros

registros |> dplyr::glimpse()

## CEP ----

### Importar ----

cep <- sf::st_read("cep.shp")

### Visualizar ----

cep

ggplot() +
  geom_point(data = registros,
             aes(Longitude, Latitude),
             size = 1) +
  geom_sf(data = cep, color = "black", fill = "transparent")

## Unidades de conservação ----

### Importar ----

uc <- sf::st_read("unidade_conservacao_cep.shp")

### Visualizar ----

uc

ggplot() +
  geom_point(data = registros,
             aes(Longitude, Latitude),
             size = 1) +
  geom_sf(data = cep, color = "black", fill = "transparent") +
  geom_sf(data = uc, color = "red", fill = "transparent")

# Calcular vies ----

## Lista das espécies com mais de 5 registros ----

sps <- registros$species |>
  unique()

sps

## Calcular vieses para cada espécie ----

vieses_sps <- purrr::map(
  sps,
  purrr::in_parallel(

    \(sps){

      tryCatch({

        registros |>
          dplyr::filter(species == sps) |>
          dplyr::rename("decimalLongitude" = Longitude,
                        "decimalLatitude" = Latitude) |>
          sampbias::calculate_bias(gaz = uc |>
                                     terra::vect() |>
                                     list(),
                                   res = 0.01,
                                   terrestrial = TRUE)

        },
        error = \(e){NULL})

      }

    ),
  .progress = TRUE)
