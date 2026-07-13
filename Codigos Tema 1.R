

library(readr)
datos = read_csv2('Resultados por mesa de la Segunda Elección Presidencial 2021.csv')
                

# Función select ----------------------------------------------------------

# 1. Seleccionar la columna DEPARTAMENTO.

datos |> select(DEPARTAMENTO)
datos |> select(2)

# 2. Seleccionar las columnas PROVINCIA y MESA_DE_vOTACION

datos |> select(PROVINCIA,MESA_DE_VOTACION)
datos |> select(3,6)

# 3. Seleccionar todas las columnas excepto DISTRITO.

datos |> select(-DISTRITO)
datos |> select(-4)

# 4. Seleccionar las columnas que comiencen con "VOTOS"

datos |> select_at(vars(starts_with("VOTOS")))
datos |> select(starts_with("VOTOS"))

# 5. Seleccionar las columnas que terminen con "O"

datos |> select_at(vars(ends_with("O")))
datos |> select(ends_with("O"))

# 6. Seleccionar las columnas que contengan "_"

datos |> select_at(vars(contains("_")))
datos |> select(contains("_"))

# 7. Seleccionar todas las columnas numéricas

datos |> select_if(is.numeric)
datos |> select(where(is.numeric))

# 8. Seleccionar las columnas numéricas con algún valor perdido

datos |> select(where(~ is.numeric(.x) && any(is.na(.x))))
  

# Función pull ------------------------------------------------------------

# 1. Seleccionar la variable DEPARTAMENTO, como vector.

datos |> pull(DEPARTAMENTO)
datos |> pull(2)

# 2. Seleccionar la última variable del data frame, como vector.

datos |> pull(-1)


# Función mutate ----------------------------------------------------------

# 1. Cree la variable VOTOS_NO_VALIDOS como la suma de votos blancos, nulos e 
# impugnados.

datos |> mutate(VOTOS_NO_VALIDOS = VOTOS_VB + VOTOS_VN)

# 2. Crear la variable UBICACIÓN, concatenando DEPARTAMENTO, PROVINCIA y 
# DISTRITO, dejando un espacio entre valor y valor.

datos |> mutate(UBICACIÓN = paste(DEPARTAMENTO, PROVINCIA, DISTRITO)) 

# 3.  Se dice que una mesa es CONCURRIDA si más del 80% de sus electores asiste 
# a votar. Crear la variable CONCURRIDA que tome los valores SI o NO.

datos |> mutate(CONCURRIDA = ifelse(N_CVAS/N_ELEC_HABIL>0.80, "SI","NO")) 

# 4. Se dice que una mesa es CONCURRIDA si más del 80% de sus electores asiste 
# a votar, mientras que es AUSENTE si menos del 20% asiste a votar. Crear la 
# variable GRADO_CONCURRENCIA, que tome los valores AUSENTE, NORMAL y CONCURRIDA

datos |> 
  mutate(GRADO_CONCURRENCIA = case_when(N_CVAS / N_ELEC_HABIL < 0.20 ~ "AUSENTE",
                                        N_CVAS / N_ELEC_HABIL > 0.80 ~ "CONCURRIDA",
                                        TRUE ~ "NORMAL"))

# 5. Crear dos nuevas columnas, que contengan el porcentaje de votos obtenido 
# por cada candidato

datos |> 
  mutate_at(vars(VOTOS_P1, VOTOS_P2), ~.x / N_CVAS * 100)

datos |> 
  mutate(across(c(VOTOS_P1,VOTOS_P2), ~.x / N_CVAS * 100))

# 6. Transformar todas las variables que empiezan con VOTOS, reemplazando los
# NA por 0

datos |> 
  mutate_at(vars(starts_with("VOTOS")), ~replace_na(.x, 0))

datos |> 
  mutate(across(starts_with("VOTOS"), ~ replace_na(.x, 0)))

# 7. Redondear todas las variables numéricas

datos |> mutate_if(is.numeric, round)

datos |> mutate(across(where(is.numeric), round))

# 8. Convertir todas las columnas a texto

datos |> mutate_all(as.character)

datos |> mutate(across(everything(), as.character))
    

# Función rename ----------------------------------------------------------

# 1. Cambiar el nombre de DEPARTAMENTO a DEPTO y PROVINCIA a PROV

datos |> 
  rename(DEPTO = 2,
         PROV  = 3)

datos |> 
  rename(DEPTO = "DEPARTAMENTO",
         PROV  = "PROVINCIA")

# 2. Cambiar el nombre de todas las variables a minúsculas

datos |> rename_with(tolower)

datos |> rename_all(tolower)

# 3. Cambiar el nombre de DEPARTAMENTO y PROVINCIA a minúsculas

datos |> rename_at(vars(DEPARTAMENTO, PROVINCIA), tolower)

datos |> rename_with(tolower, c(DEPARTAMENTO, PROVINCIA))

# 4. Cambiar el nombre de las variables numéricas agregándole NUM delante

datos |> rename_if(is.numeric, ~ paste0("NUM_", .x))

datos |> rename_with(~ paste0("NUM_", .x), where(is.numeric))


# Función filter ----------------------------------------------------------

# 1. Filtrar las mesas del departamento de AMAZONAS

datos |> filter(DEPARTAMENTO == "AMAZONAS")

# 2. Filtrar las mesas con más de 250 electores hábiles

datos |> filter(N_ELEC_HABIL > 250)

# 3. Filtrar las mesas donde los votos nulos fueron diferentes de cero

datos |> filter(VOTOS_VN != 0)

# 4. Filtrar las mesas de Bagua con más de 250 electores hábiles

datos |> filter(PROVINCIA == "BAGUA" & N_ELEC_HABIL > 250)

# 5. Filtrar las mesas donde P2 obtuvo más votos que P1 y hubo menos de 10 votos nulos

datos |> filter(VOTOS_P2 > VOTOS_P1, VOTOS_VN < 10)

# 6. Filtrar las mesas donde P1 o P2 obtuvo más de 150 votos

datos |> filter(VOTOS_P1 > 150 | VOTOS_P2 > 150)

# 7. Filtrar las mesas de la provincia de LIMA o HUARAL

datos |> filter(PROVINCIA == "LIMA" | PROVINCIA == "HUARAL")

datos |> filter(PROVINCIA %in% c("LIMA", "HUARAL"))

# 8. Filtrar las mesas que tienen entre 200 y 300 electores hábiles

datos |> filter(N_ELEC_HABIL >= 200, N_ELEC_HABIL <= 300)

datos |> filter(between(N_ELEC_HABIL, 200, 300))

# 9. Filtrar las mesas donde ambas variables de votos blancos o nulos tienen datos

datos |> filter(!is.na(VOTOS_VB), !is.na(VOTOS_VN))

# 10. Filtrar las mesas donde P1 obtuvo más del 50% de los votos emitidos

datos |> filter(VOTOS_P1 / N_CVAS > 0.50)

# 11. Filtrar las mesas donde existe una inconsistencia entre los votos y el número de ciudadanos que votaron

datos |> filter(VOTOS_P1 + VOTOS_P2 + VOTOS_VB + VOTOS_VN != N_CVAS)


# Función arrange ---------------------------------------------------------

# 1. Ordenar las mesas de menor a mayor cantidad de electores hábiles

datos |> arrange(N_ELEC_HABIL)

# 2. Ordenar las mesas de mayor a menor cantidad de electores hábiles

datos |> arrange(desc(N_ELEC_HABIL))

datos |> arrange(-N_ELEC_HABIL)

# 3. Ordenar las mesas alfabéticamente según la provincia

datos |> arrange(PROVINCIA)

# 4. Ordenar por departamento, provincia  y distrito

datos |> arrange(DEPARTAMENTO, PROVINCIA, DISTRITO)

# 5. Ordenar por distrito y, dentro de cada distrito, de mayor a menor cantidad de votos de P1

datos |> arrange(DISTRITO, desc(VOTOS_P1))

