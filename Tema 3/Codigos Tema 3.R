
# Estructuras de datos ----------------------------------------------------

## Vectores ----------------------------------------------------------------

# Asignar datos a un vector

x1 <- c(1,4,2,5,3,5,6,9,-5,4,2,3,4,-5)
x1

x2 <- c(3, T, "abc#@4")
x2

# Crear un vector secuencial

x3 <- 1:5
x3

x4 <- seq(4, 20, 2)
x4

x5 <- seq(30, 5, -6)
x5

# Crear un vector con datos repetidos

x6 <- rep("a", 6)
x6

# Acceder a los elementos de un vector

x1[3]
x1[c(2,5)]
x1[-1]
x1[x1>5]

# Operaciones sobre vectores

x1 |> length()
x1 |> sum()
x1 |> mean()
x1 |> min()
x1 |> max()
x1 |> sort()
x1 |> sort(decreasing = TRUE)
x1 |> which.max()
x1 |> which.min()
x1 |> unique()
x1 |> duplicated()
(x1 > 0) |> any()
(x1 > 0) |> all()

## Listas ------------------------------------------------------------------

# Asignar datos a una lista

y1 <- list(3,4,0,1,2)
y1

y2 <- list("a#", 6:3, T, 3i)
y2

y3 <- list(p = 33, q = c(3,4,5,6,1,3), r = sqrt(55))
y3

# Acceder a los elementos de una lista

y3[[1]]

y3$p

y3[1]

# Operaciones sobre listas

y3 |> length()
y3 |> names()  
y3 |> unlist()
y3 |> lapply(sum)
y3 |> sapply(sum)


## Matrices ----------------------------------------------------------------

# Asignar datos a una matriz

m1 <- matrix(c(3,8,5,6,2,1), ncol = 2)
m1

m2 <- diag(4)*5
m2

# Acceder a los elementos de una matriz

m1[1,]
m1[,1]
m1[1,1]

# Operaciones con matrices

m1 |> length()
m1 |> dim()
m1 |> nrow()
m1 |> ncol()
m1 |> rowSums()
m1 |> colSums()
m1 |> apply(1,sum)
m1 |> apply(2,sum)
m1 |> apply(2,var)
m2 |> diag()
m2 |> solve()
m2 |> det()


# Estructuras de control --------------------------------------------------

## if ----------------------------------------------------------------------

### Ejemplo 1
edad <- 20
if(edad >= 18){print("Mayor de edad")}

### Ejemplo 2
notas <- data.frame(Alumno = c("001","002","008","015","025"),
                    Nota   = c(12,10,15,18,19))
if(all(notas$Nota > 10)){
  print("Todas las notas son aprobatorias")
  }

### Ejemplo 3
x <- c(4, 7, 9, 12)
if(!is.numeric(x)){
  stop("El argumento x debe ser numérico")
}
mean(x)

### Ejemplo 4
x <- letters
if(!is.numeric(x)){
  stop("El argumento x debe ser numérico")
}

## if else -----------------------------------------------------------------

### Ejemplo 5
edad <- 16
if (edad >= 18){
  print("Mayor de edad")
  }else{
    print("Menor de edad")
    }

### Ejemplo 6
notas <- data.frame(Alumno = c("001","002","008","015","025"),
                    Nota   = c(12,10,15,18,19))
if(any(notas$Nota > 10)){
  print("Existe al menos una nota mayor que 10")
  }else{
    print("Ningún estudiante obtuvo una nota mayor que 10")
  }

### Ejemplo 7
m <- c(3, 5, 7, 9, 11)
if(length(m) %% 2 == 0){
  print("El vector tiene una cantidad par de elementos")
  }else{
    print("El vector tiene una cantidad impar de elementos")
  }

### Ejemplo 8
m <- matrix(c(12, 8, 15, 10, 17, 14), nrow = 2)
if(nrow(m) == ncol(m)){
  print("La matriz es cuadrada")
  }else{
    print("La matriz no es cuadrada")
  }

## ifelse() -----------------------------------------------------------------

### Ejemplo 9
edad <- c(15,18,20,16,35,14,20)
ifelse(edad >= 18, "Mayor", "Menor")

### Ejemplo 10
matriz <- matrix(c(1,5,7,2,3,5,2,5,5), ncol = 3)
ifelse(matriz <= 3, "Menor a 4", "Mayor a 3")

### Ejemplo 11
notas <- data.frame(Alumno = c("001","002","008","015","025"),
                    Nota   = c(12,10,15,18,19))
library(dplyr)
notas |> mutate(Calificacion = ifelse(Nota >= 10.5, "Aprobado", "Desaprobado"))

# Estructuras repetitivas -------------------------------------------------

## for ---------------------------------------------------------------------

### Ejemplo 12
for(i in 1:5){
  print(i)}

### Ejemplo 13
x <- c(5,8,2,7)
for(i in x){
  print(i+10)}

### Ejemplo 14
x <- 1:5
resultado <- numeric(length(x))
for(i in seq_along(x)){
  resultado[i] <- x[i]^2 + 2*x[i]}
resultado

### Ejemplo 15
x <- c(-5, 8, -2, 7, 0, 12)
resultado <- numeric(length(x))
for(i in seq_along(x)){
  if(x[i] < 0){
    resultado[i] <- 0}
  else{
    resultado[i] <- x[i]}
  }
resultado

### Ejemplo 16
notas <- data.frame(Alumno = c("001","002","008","015","025"),
                    Nota   = c(12,10,15,18,19))
contador <- 0
for(i in seq_along(notas$Nota)){
  if(notas$Nota[i] >= 10.5){
    contador <- contador + 1}
  }
contador

### Ejemplo 17
x <- c(14, 8, 23, -2, 17, 5, 20)
mayor <- x[1]
posicion <- 1
for(i in 2:length(x)){
  if(x[i] > mayor){
    mayor <- x[i]
    posicion <- i}
  }
mayor
posicion



## while -------------------------------------------------------------------

### Ejemplo 18
i <- 1
while(i <= 5){
  print(i)
  i <- i + 1
}

### Ejemplo 19
x <- 1
while(x <= 200){
  print(paste("El valor de x es ",x))
  x <- x*3
}

### Ejemplo 20
numero       <- 16
aproximacion <- 1
error        <- 1
tolerancia   <- 0.0001
while(error > tolerancia){
  nueva_aproximacion <- (aproximacion + numero/aproximacion)/2
  error              <- abs(nueva_aproximacion - aproximacion)
  aproximacion       <- nueva_aproximacion
  print(paste0("Aproximación = ", aproximacion))}
aproximacion


# Estructuras de control de ciclos ----------------------------------------

## break -------------------------------------------------------------------

### Ejemplo 21
for(i in 1:10){
  if(i == 6){
    break
    }
  print(i)
}

### Ejemplo 22
x <- c(4, 8, 12, 8, 20, 25)
valor_buscado <- 8
for(i in seq_along(x)){
  print(paste0("x[",i,"]=",x[i]))
  if(x[i] == valor_buscado){
    posicion <- i
    print(paste("El valor buscado se encuentra en la posición ", posicion))
    break
    }
}

## next --------------------------------------------------------------------

### Ejemplo 23
for(i in 1:10){
  if(i %% 3 == 0){
    next
    }
  print(i)
}

### Ejemplo 24
x <- c(9, -4, 16, -1, 25, 0)
raices <- numeric(length(x))
for(i in seq_along(x)){
  if(x[i] < 0){
    next
  }
  raices[i] <- sqrt(x[i])
}
raices


# Funciones ---------------------------------------------------------------

### Ejemplo 25
primera <- function(){
  print("Esta es mi primera función")
}
primera()

### Ejemplo 26
area_circulo <- function(r){
  pi*r^2
  }
area_circulo(5)

### Ejemplo 27
potencia <- function(x, p = 2){
  x^p
}
potencia(5)
potencia(5,3)
potencia(p = 3, x = 5)

### Ejemplo 28
resumir_vector <- function(x, remove_na = TRUE){
  if(!is.numeric(x)){
    stop("x debe ser un vector numérico")
  }
  if(any(x < 0 | x > 20)){
    stop("Las notas deben estar entre 0 y 20")
  } 
  resultado <- c(
    minimo  = min(x, na.rm = remove_na),
    media   = mean(x, na.rm = remove_na),
    mediana = median(x, na.rm = remove_na),
    maximo  = max(x, na.rm = remove_na))
  return(resultado)
}
notas <- c(1,8,9,6,15,14,11,NA,12)
resumir_vector(notas, TRUE)
resumir_vector(notas, FALSE)

resumir_vector <- function(x, remove_na = TRUE){
  if(!is.numeric(x)){
    stop("x debe ser un vector numérico")
  }
  if(any(is.na(x))){
    warning("Existen valores perdidos")
  }
  if(any(x < 0 | x > 20, na.rm = TRUE)){
    stop("Las notas deben estar entre 0 y 20")
  } 
  resultado <- list(
    datos   = x,
    n       = length(x),
    minimo  = min(x, na.rm = remove_na),
    media   = mean(x, na.rm = remove_na),
    mediana = median(x, na.rm = remove_na),
    maximo  = max(x, na.rm = remove_na))
  return(resultado)
}
notas <- c(1,8,9,6,15,14,11,NA,12)
resumir_vector(notas, TRUE)
resumir_vector(notas, FALSE)
