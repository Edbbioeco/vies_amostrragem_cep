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
