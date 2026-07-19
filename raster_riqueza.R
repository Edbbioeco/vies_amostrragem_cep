# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

library(terra)

library(tidyterra)

library(ggview)

# Dados ----

## Registros ----

### Importar ----

registros <- readxl::read_xlsx("registros.xlsx")

### Visualizar ----

registros

registros |> dplyr::glimpse()

ggplot() +
  geom_point(data = registros, aes(Longitude, Latitude))

## Shapéfile do CEP ----

### Importar ----

cep_grade <- sf::st_read("grade_cep.shp")

### Visualizar ----

cep_grade

ggplot() +
  geom_sf(data = cep_grade, color = 'black') +
  geom_point(data = registros, aes(Longitude, Latitude))

# Riqueza de registros ----

## Calcular riqueza de registros ----

riq <- registros |>
  dplyr::summarise(Riqueza = species |> dplyr::n_distinct(),
                   .by = FID)

riq

## Calcular a riqueza para todos os grids ----

cep_grade_riqueza <- cep_grade |>
  dplyr::left_join(riq,
                   by = "FID") |>
  dplyr::mutate(Riqueza = dplyr::case_when(

    Riqueza |> is.na() ~ 0,
    .default = Riqueza
  ))

cep_grade_riqueza

ggplot() +
  geom_sf(data = cep_grade_riqueza, aes(fill = Riqueza,
                                        color = Riqueza)) +
  scale_fill_viridis_c() +
  scale_color_viridis_c()

## Criar Raster ----

### Criar template ----

raster_riqueza_registros <- cep_grade_riqueza |>
  terra::vect() |>
  terra::rast(resolution = 0.0898316) %>%
  terra::rasterize(x = cep_grade_riqueza |>
                     terra::vect(),
                   y = .,
                   field = "Riqueza")

### Visualizar ----

raster_riqueza_registros

ggplot() +
  tidyterra::geom_spatraster(data = raster_riqueza_registros) +
  scale_fill_viridis_c(na.value = "transparent")
