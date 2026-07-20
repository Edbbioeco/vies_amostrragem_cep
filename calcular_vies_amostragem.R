# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

library(terra)

library(tidyterra)

library(sampbias)

library(performance)

library(broom)

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

# Viés de amostragem ----

## Calcular viés ----

vies <- registros |>
  dplyr::rename("decimalLongitude" = Longitude,
                "decimalLatitude" = Latitude) |>
  sampbias::calculate_bias(gaz = list(uc |>
                                        terra::vect(),
                                      areas_urb |>
                                        terra::vect(),
                                      rodovias |>
                                        terra::vect()) |>
                             setNames(c("UC",
                                        "AU",
                                        "RO")),
                           inp_raster = riq_reg,
                           res = 0.0898316,
                           terrestrial = TRUE)

vies

## Pesos por fator ----

### Criar data frame ----

df_w <- vies$bias_estimate |>
  tidyr::pivot_longer(cols = dplyr::contains("w_"),
                      names_to = "Factor",
                      values_to = "Weight") |>
  dplyr::mutate(Factor = Factor |>
                  stringr::str_remove("w_") |>
                  stringr::str_replace_all("\\.", " "))

df_w

### Modelo ANOVA ----

#### Criar modelo ----

anova_w <- lm(Weight ~ Factor, data = df_w)

#### Ajuste do modelo ----

anova_w |> performance::check_model(check = c("qq",
                                              "normality"))

#### Data frame das estatísticas ----

anova_w |>
  anova() |>
  broom::tidy()

### Gráfico ----

df_w |>
  ggplot(aes(Factor, Weight)) +
  ggbeeswarm::geom_quasirandom() +
  theme_bw() +
  theme(axis.text = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 20, color = "black"),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black"),
        legend.position = "bottom",
        strip.text = element_text(size = 30, color = "black"),
        strip.background = element_rect(color = "black",
                                        linewidth = 1),
        panel.background = element_rect(linewidth = 1,
                                        color = "black"),
        plot.title = element_text(size = 20, color = "black"),
        plot.subtitle = element_text(size = 17.5, color = "black")) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "grafico_distribuição_pesos.png", height = 10, width = 12)
