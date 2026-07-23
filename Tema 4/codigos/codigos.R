
# LECTURA DE ARCHIVOS DE DATOS --------------------------------------------

library(purrr)
library(readr)
library(stringr)

amazonas <- read.csv('datos/originales/Congresal__AMAZONAS.csv', skip = 9, nrows = 17)

ruta <- "datos/originales"

archivos_csv <- list.files(path = ruta,
                           pattern = "\\.csv$",
                           full.names = TRUE)
archivos_csv


## Usando for
datos_lista <- list()
for (i in seq_along(archivos_csv)) {
  datos_lista[[i]] <- read.csv(archivos_csv[i], skip = 9)
}
datos_lista[[1]]

departamentos <- archivos_csv |>
  basename() |>
  str_remove("\\.csv$") |>
  str_remove("Congresal__")

datos_lista |> names() <- departamentos

datos_lista$AMAZONAS

## Usando map
datos_lista_b <- map(archivos_csv, read.csv, skip = 9)
datos_lista_b[[1]]
datos_lista_b |> names() <- departamentos
datos_lista_b$AMAZONAS

## ¿Son iguales?
identical(datos_lista, datos_lista_b)


# GUARDADO DE ARCHIVOS DE DATOS -------------------------------------------

library(dplyr)
library(readr)
library(writexl)

Partidos <- c("FRENTE POPULAR AGRICOLA FIA DEL PERU - FREPAP",
              "PARTIDO NACIONALISTA PERUANO",
              "EL FRENTE AMPLIO POR JUSTICIA, VIDA Y LIBERTAD",
              "PARTIDO MORADO",
              "VICTORIA NACIONAL",
              "ACCION POPULAR",
              "PODEMOS PERU",
              "JUNTOS POR EL PERU",
              "PARTIDO POPULAR CRISTIANO - PPC",
              "FUERZA POPULAR",
              "UNION POR EL PERU",
              "RENOVACION POPULAR",
              "RENACIMIENTO UNIDO NACIONAL",
              "PARTIDO DEMOCRATICO SOMOS PERU",
              "PARTIDO POLITICO NACIONAL PERU LIBRE",
              "DEMOCRACIA DIRECTA",
              "ALIANZA PARA EL PROGRESO")

datos_lista_mod <- list()
for (i in seq_along(datos_lista)) {
  datos_lista_mod[[i]] <- datos_lista[[i]] |>
    select(1: 3) |>
    rename(PARTIDO = 1,
           VOTOS_TOTAL = 2,
           VOTOS_N1 = 3) |>
    filter(PARTIDO %in% Partidos) |> 
    mutate(VOTOS_TOTAL = parse_number(VOTOS_TOTAL),
           VOTOS_N1    = parse_number(VOTOS_N1))
}

datos_lista_mod |> names() <- departamentos
datos_lista_mod$APURIMAC

for (i in seq_along(datos_lista_mod)) {
  nombre_archivo_csv <- paste0(names(datos_lista_mod)[i], ".csv")
  write.csv(datos_lista_mod[[i]],
            file = file.path("datos/procesados", nombre_archivo_csv),
            row.names = FALSE)
}

for (i in seq_along(datos_lista_mod)) {
  nombre_archivo_xlsx <- paste0(names(datos_lista_mod)[i], ".xlsx")
  write_xlsx(datos_lista_mod[[i]],
            path = file.path("datos/procesados",nombre_archivo_xlsx))
}

# GUARDADO DE ARCHIVOS RDS ------------------------------------------------

modelo <- lm(VOTOS_TOTAL ~ VOTOS_N1, datos_lista_mod[[1]])
modelo

modelos <- list()
for(i in 1:length(datos_lista_mod)){
  modelos[[i]] <-  lm(VOTOS_TOTAL ~ VOTOS_N1, datos_lista_mod[[i]])
}
modelos[[1]] |> summary()

saveRDS(modelos, "resultados/modelos.RDS")

library(ggplot2)
library(ggrepel)
library(scales)

graficos <- list()
for(i in 1:length(datos_lista_mod)){
graficos[[i]] <- datos_lista_mod[[i]] |> 
ggplot(
  aes(
    x = VOTOS_N1,
    y = VOTOS_TOTAL,
    label = PARTIDO
  )
) +
  geom_point(size = 3) +
  geom_text_repel(
    size = 3,
    max.overlaps = Inf,
    box.padding = 0.5,
    point.padding = 0.3
  ) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Relación entre los votos totales y los votos por el N.° 1",
    subtitle = paste("Resultados electorales de", (datos_lista_mod |> names())[i]),
    y = "Votos totales del partido",
    x = "Votos por el candidato N.° 1"
  ) +
  theme_minimal()
}

saveRDS(graficos, "resultados/graficos.RDS")

# LECTURA DE ARCHIVOS RDS -------------------------------------------------

rm(list=ls())

modelos_leidos <- readRDS("resultados/modelos.RDS")
modelos_leidos[[1]] |> summary()

graficos_leidos <- readRDS("resultados/graficos.RDS")
graficos_leidos[[1]]
graficos_leidos[[2]]
graficos_leidos[[3]]
graficos_leidos[[4]]
