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

library(patchwork)

library(spdep)

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

## Taxa de amostragem por MCMC ----

### Maior distância possível dentro do CEP ----

dist_cep <- cep |>
  sf::st_boundary() |>
  sf::st_cast("POINT") |>
  sf::st_coordinates() |>
  as.data.frame() |>
  dplyr::arrange(dplyr::desc(Y)) |>
  dplyr::slice(c(1, dplyr::n())) |>
  sf::st_as_sf(coords = c(1:2), crs = 4674) |>
  sf::st_distance() |>
  max() |>
  as.numeric() / 1e3

dist_cep

### Valores médios das estimativas do modelo ----

medias_vies <- vies$bias_estimate |> colMeans()

medias_vies

### Criar vetor de distâncias ----

dist_seq <- seq(0, dist_cep |> as.numeric(), length.out = 1000)

dist_seq

### Data frame dos valores preditos de sampling rate ----

df_sr <- purrr::map_dfr(
  medias_vies[5:7] |> names(),
  purrr::in_parallel(

    ~tibble::tibble(
      `Distance to factor (km)` = dist_seq,
      `Sampling rate` = medias_vies[["q"]] * exp(-medias_vies[[.x]] * `Distance to factor (km)`),
      Factor = .x |> stringr::str_remove("w_"))

    ),
  .progress = TRUE)

df_sr

### Gráfico ----

df_sr |>
  ggplot(aes(`Distance to factor (km)`, `Sampling rate`, color = Factor)) +
  geom_line(linewidth = 3) +
  scale_color_manual(values = c("darkorange4", "gold2", "forestgreen")) +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5)) +
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

ggsave(filename = "grafico_sampling_rate.png", height = 10, width = 12)

## Projeção espacial ----

### Calcular projeção ----

raster_proj <- vies |>
  sampbias::project_bias() |>
  terra::crop(cep) |>
  terra::mask(cep)

raster_proj |>
  terra::set.names(raster_proj |>
                     terra::names() |>
                     stringr::str_replace("_", " "))

### Visualizar ----

raster_proj

purrr::map(
  1:terra::nlyr(raster_proj),
  purrr::in_parallel(

    ~ggplot() +
      tidyterra::geom_spatraster(data = raster_proj[[.x]]) +
      facet_wrap(~lyr) +
      scale_fill_viridis_c(na.value = "transparent",
                           guide = guide_colourbar(

                             title.position = "top",
                             title.hjust = 0.5,
                             barwidth = 15,
                             frame.colour = "black",
                             ticks.colour = "black")) +
      theme_bw() +
      theme(axis.text = element_text(size = 20, color = "black"),
            axis.title = element_text(size = 20, color = "black"),
            legend.text = element_text(size = 20, color = "black"),
            legend.title = element_text(size = 20, color = "black"),
            legend.position = "bottom",
            strip.text = element_text(size = 20, color = "black"),
            strip.background = element_rect(color = "black",
                                            linewidth = 1),
            panel.background = element_rect(linewidth = 1,
                                            color = "black"),
            plot.title = element_text(size = 20, color = "black"),
            plot.subtitle = element_text(size = 17.5, color = "black")) +
      ggview::canvas(height = 10, width = 12)

  ),
  .progress = TRUE) |>
  patchwork::wrap_plots(nrow = 1) +
  ggview::canvas(height = 20, width = 24)

ggsave(filename = "grafico_projecoes.png", height = 20, width = 24)

### Calcular agregação espacial ----

moran_global <- purrr::map(
  1:terra::nlyr(raster_proj),
  purrr::in_parallel(

    \(lyr){

      valores <- raster_proj|>
        terra::as.points() |>
        as.data.frame()  %>%
        .[, lyr]

      lw <- raster_proj|>
        terra::crds() |>
        as.data.frame() |>
        spdep::knearneigh(k = 8) |>
        spdep::knn2nb() |>
        spdep::nb2listw()

      spdep::moran.test(valores, lw)

      }

    ),
  .progress = TRUE) |>
  setNames(raster_proj |>
             terra::names()) |>
  purrr::imap_dfr(~.x |>
                    broom::tidy() |>
                    dplyr::mutate(Factor = .y))

moran_global

### Tabelas das estatísticas ----

moran_flex <- moran_global |>
  dplyr::relocate(Factor, .before = 1) |>
  dplyr::select(2, 5:6) |>
  dplyr::mutate(p.value = dplyr::case_when(

    p.value < 0.01 ~ "< 0.01",
    .default = p.value |> as.character())) |>
  dplyr::rename("Moran's I" = 1,
                "z" = 2,
                "p" = 3)
