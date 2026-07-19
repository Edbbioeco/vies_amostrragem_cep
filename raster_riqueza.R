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
