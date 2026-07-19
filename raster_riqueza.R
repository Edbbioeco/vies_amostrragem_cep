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
