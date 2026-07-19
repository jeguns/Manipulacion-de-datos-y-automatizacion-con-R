
# Intervalos de confianza -------------------------------------------------

## Intervalo de confianza para la media ------------------------------------

# Ejemplo 1

hojas <- c(7.8, 8.7, 7.2, 8.0, 8.3, 7.9, 7.6, 8.1)
library(magrittr)
hojas |> t.test() |> use_series(conf.int)
hojas |> t.test(conf.level = 0.90) |> use_series(conf.int) # para otro nivel de confianza

n     <- hojas |> length()
x_bar <- hojas |> mean()
s     <- hojas |> sd()
gl    <- n - 1
alpha <- 0.05
tcrit <- qt(1 - alpha/2, df = gl)
se    <- s / sqrt(n) # error estándar
li    <- x_bar - tcrit * se
ls    <- x_bar + tcrit * se
c(li,ls)

# Ejemplo 2

tiempos = c(78, 82, 85, 80, 77, 83, 79, 81, 86, 80, 82, 84, 78, 87, 
            81, 79, 83, 82, 80, 84, 81, 79, 85, 80, 82, 78, 86, 81, 80, 83, 77, 
            79, 84, 82, 80, 78, 86, 83, 79, 80, 82, 81, 77, 85, 80, 79, 83, 82, 
            78, 84, 81, 79, 80, 77, 86, 82, 80, 81, 83, 78)
tiempos |> t.test() |> use_series(conf.int)

n     <- tiempos |> length()
x_bar <- tiempos |> mean()
s     <- tiempos |> sd()
gl    <- n - 1
alpha <- 0.05
tcrit <- qt(1 - alpha/2, df = gl)
se    <- s / sqrt(n) # error estándar
li    <- x_bar - tcrit * se
ls    <- x_bar + tcrit * se
c(li,ls)


## Intervalo de confianza para la varianza ---------------------------------

# Ejemplo 3

sodio = c(43, 39, 45, 48, 46, 50, 44, 49, 47, 46, 42, 41)
library(EnvStats)
sodio |>
  varTest(conf.level = 0.95)|>
  use_series(conf.int) 

n        <- sodio |> length()
s2       <- sodio |> var()
alpha    <- 0.05
gl       <- n - 1
chi2_inf <- qchisq(1 - alpha/2, df = gl)
chi2_sup <- qchisq(alpha/2, df = gl)
li_var   <- (gl * s2) / chi2_inf
ls_var   <- (gl * s2) / chi2_sup
c(li_var, ls_var)
c(li_var, ls_var) |> sqrt() # IC para desviación estándar

## Intervalo de confianza para la proporción -------------------------------

# Ejemplo 4

library(binom)
binom.confint(x = 36, n = 150, methods = "asymptotic")

n     <- 150
x     <- 36
p     <- x/n
alpha <- 0.05
zcrit <- qnorm(1-alpha/2)
li    <- p - zcrit*sqrt(p*(1-p)/n)
ls    <- p + zcrit*sqrt(p*(1-p)/n)
c(li,ls)

# Ejemplo 5

binom.confint(x = 2, n = 44, methods = "wilson")
prop.test(x = 2, n = 44, correct = FALSE)$conf.int

n      <- 44
x      <- 2
p      <- x/n
alpha  <- 0.05
zcrit  <- qnorm(1-alpha/2)
num_li <- (p+zcrit**2/(2*n)-zcrit*sqrt(p*(1-p)/n+zcrit**2/(4*n**2)))
num_ls <- (p+zcrit**2/(2*n)+zcrit*sqrt(p*(1-p)/n+zcrit**2/(4*n**2)))
li_wilson <- num_li/(1+zcrit**2/n)
ls_wilson <- num_ls/(1+zcrit**2/n)
c(li_wilson,ls_wilson)

binom.confint(x, n, methods = "exact")
binom.test(x = x, n = n) |> use_series(conf.int)

li_exacto <- qbeta(alpha/2, x, n - x + 1)
ls_exacto <- qbeta(1 - alpha/2, x + 1, n - x)
c(li_exacto, ls_exacto)

# Ejemplo 6

x = 1
n = 18

binom.confint(x, n, methods = "asymptotic") # Aproximación Normal

binom.confint(x, n, methods = "wilson") # Aproximación de Wilson

binom.confint(x, n, methods = "exact") # Método exacto


# Prueba de Hipótesis -----------------------------------------------------

## Prueba de hipótesis para una media ----------------------------------------

# Ejemplo 8

tiempo <- c(2.8, 3.9, 4.2, 2.1, 3.7, 4.5, 3.8, 4.1, 4.6, 3.6, 4.3, 4.0, 
            2.9, 1.4, 3.9, 4.5, 4.2, 4.0, 3.8, 1.0, 4.7, 3.5, 2.1, 2.2, 
            3.6, 4.3, 4.8, 4.2, 3.9, 3.7, 4.4, 3.0, 3.8, 4.6, 2.5)
(tcalc <- (mean(tiempo) - 4)/(sd(tiempo)/sqrt(length(tiempo))))
(tcrit <- qt(p = 0.10, df = length(tiempo)-1))
t.test(x = tiempo, mu = 4, alternative = "less")

library(BSDA)
tsum.test(
  mean.x = 3.617,
  s.x = 0.95,
  n.x = 35,
  mu = 4,
  alternative = "less")


## Prueba de hipótesis para una varianza -------------------------------------

# Ejemplo 9

(chicalc = (length(tiempo)-1)*var(tiempo)/1)
(chicrit = qchisq(p = 0.90, df = length(tiempo)-1))
library(EnvStats)

varTest(x = tiempo, sigma.squared = 1, alternative = "greater")


## Prueba de hipótesis para una proporción -----------------------------------

# Ejemplo 10

p <- 71/100
(Z_calc <- (p - 0.8) / sqrt(0.8 * (1 - 0.8) / 100))
(Z_crit1 <- qnorm(0.025))
(Z_crit2 <- qnorm(0.975))

prop.test(x=71, n=100, p=0.80, alternative = "two.sided", correct = F)


## Prueba de hipótesis para dos varianzas ----------------------------------

# Ejemplo 11

A <- c(8.1, 7.9, 8.3, 7.8, 8.0, 8.2, 7.7)
B <- c(7.5, 7.2, 7.1, 7.4, 7.3, 7.6, 7.2, 7.5)
(Fcalc <- var(A)/var(B))
(Fcrit1 <- qf(0.05, 6, 7))
(Fcrit2 <- qf(0.95, 6, 7))

var.test(A, B, alternative = "two.sided", ratio = 1)


## Prueba de hipótesis para dos medias independientes ----------------------

# Ejemplo 12

E1 = c(2500, 2700, 2600, 2800, 2900)
E2 = c(3000, 2100, 3200, 2300, 3400)
var.test(E1, E2, alternative = "two.sided", ratio = 1)
t.test(E1, E2, alternative = "two.sided", mu = 0, var.equal = F, paired = F)

# Ejemplo 13

A <- c(820, 830, 815, 860, 825, 835, 822)
B <- c(800, 805, 798, 810, 802, 799, 803, 777, 789, 815)
var.test(A, B, alternative = "two.sided", ratio = 1)
t.test(A, B, alternative = "greater", mu = 20, var.equal = T, paired = F)


## Prueba de hipótesis para dos medias pareadas ----------------------------

# Ejemplo 14

antes   = c(120, 122, 121, 119, 118, 123, 121, 120, 122, 119, 115, 123)
despues = c(125, 127, 126, 124, 123, 129, 126, 125, 128, 123, 116, 129)
t.test(despues, antes, mu = 3, alternative = "greater", paired = T)


## Prueba de hipótesis para dos proporciones -------------------------------

# Ejemplo 15

x1 <- 90; n1 <- 120; (p1 <- x1/n1) 
x2 <- 80; n2 <- 110; (p2 <- x2/n2)
(p <- (x1+x2)/(n1+n2))
(zcalc <- (p1-p2)/sqrt(p*(1-p)*(1/n1+1/n2)))
(zcrit1 <- qnorm(0.025))
(zcrit2 <- qnorm(0.975))
prop.test(x = c(x1, x2), n = c(n1, n2), alternative = "two.sided", correct = F)

# Ejemplo 16

x1 <- 70; n1 <- 100; (p1 <- x1/n1) 
x2 <- 45; n2 <- 100; (p2 <- x2/n2)
(zcalc <- (p1-p2-0.20)/sqrt(p1*(1-p1)/n1+p2*(1-p2)/n2))
(zcrit2 <- qnorm(0.95))
(pv <- 1-pnorm(zcalc))


