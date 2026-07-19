# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

library(terra)

library(tidyterra)

library(sampbias)

library(writexl)

library(ggtext)

library(ggview)

# Dados ----

## Registros de ocorrência ----

### Importar ----

registros <- readxl::read_xlsx("registros.xlsx")

### Visualizar ----

registros

registros |> dplyr::glimpse()

ggplot() +
  geom_point(data = registros, aes(Longitude, Latitude))

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

## Áreas urbanas ----

### Importar ----

areas_urb <- sf::st_read("areas_urb.shp")

### Visualizar ----

areas_urb

ggplot() +
  geom_point(data = registros,
             aes(Longitude, Latitude),
             size = 1) +
  geom_sf(data = cep, color = "black", fill = "transparent") +
  geom_sf(data = areas_urb, color = "red", fill = "transparent")

## rodovias ----

### Importar ----

rodovias <- sf::st_read("rodovias.shp")

### Visualizar ----

areas_urb

ggplot() +
  geom_point(data = registros,
             aes(Longitude, Latitude),
             size = 1) +
  geom_sf(data = cep, color = "black", fill = "transparent") +
  geom_sf(data = rodovias, color = "red", fill = "transparent")

## Raster de riqueza de registros ----

### Importar ----

riq_reg <- terra::rast("riqueza_registros.tif")

### Visualizar ----

riq_reg

ggplot() +
  tidyterra::geom_spatraster(data = riq_reg) +
  scale_fill_viridis_c(na.value = "transparent")
