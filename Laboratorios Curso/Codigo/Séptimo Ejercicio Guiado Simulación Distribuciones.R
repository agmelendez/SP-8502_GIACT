#https://rpubs.com/tereom/va_simulacion

#Distribución Binomial
rm()
xy <- data.frame(x = 0:20)
ggplot(xy) +
  geom_bar(aes(x = x, y = dbinom(x, size = 20, prob = 0.5)), stat = "identity")

xy <- data.frame(x = 0:20)
ggplot(xy) +
  geom_bar(aes(x = x, y = dbinom(x, size = 20, prob = 0.1)), stat = "identity")

#Distribución Poisson

xy <- data.frame(x = 0:20)
ggplot(xy) +
  geom_bar(aes(x = x, y = dpois(x, lambda = 2)), stat = "identity")

xy <- data.frame(x = 0:12)
ggplot(xy) +
  geom_bar(aes(x = x, y = dgeom(x, prob = .5)), stat = "identity")

#Variables aleatorias continuas

xy <- data.frame(x = -5:5, y = seq(0, 1, length.out = 11))
ggplot(xy, aes(x = x, y = y)) +
  stat_function(fun = dnorm, color = "orange") + 
  stat_function(fun = dnorm, args = list(mean = 1), color = "steelblue") +
  stat_function(fun = dnorm, args = list(sd = 2), color = "darkgray") +
  labs(y = "", title = "fdp's normales")

ggplot(xy, aes(x = x, y = y)) +
  stat_function(fun = pnorm, color = "orange") + 
  stat_function(fun = pnorm, args = list(mean = 1), color = "steelblue") +
  stat_function(fun = pnorm, args = list(sd = 2), color = "darkgray") +
  labs(y = "", title = "FDA's normales")

xy <- data.frame(x = seq(0.001, 0.999, length.out = 11), y = -5:5)
ggplot(xy, aes(x = x, y = y)) +
  stat_function(fun = qnorm, color = "orange") + 
  stat_function(fun = qnorm, args = list(mean = 1), color = "steelblue") +
  stat_function(fun = qnorm, args = list(sd = 2), color = "darkgray") +
  labs(y = "", title = "Funciones de cuantiles normales")

#Distribución Exponencial
#Distribución Gamma


xy <- data.frame(x = 0:12, y = seq(0, 1, length.out = 13))
ggplot(xy, aes(x = x, y = y)) +
  stat_function(fun = dgamma, args = list(shape = 1), color = "orange") + 
  stat_function(fun = dgamma, args = list(scale = 0.5, shape = 2), color = "steelblue") +
  stat_function(fun = dgamma, args = list(scale = 3, shape = 4), color = "darkgray") +
  labs(y = "", title = "fdp's gamma")

#Distribución Beta
xy <- data.frame(x = 0:1, y = 0:1)
ggplot(xy, aes(x = x, y = y)) +
  stat_function(fun = dbeta, args = list(shape1 = 1, shape2 = 3), color = "orange") + 
  stat_function(fun = dbeta, args = list(shape1 = 5, shape2 = 1), color = "steelblue") +
  stat_function(fun = dbeta, args = list(shape1 = 0.5, shape2 = 0.5), color = "darkgray") +
  labs(y = "", title = "fdp's Beta")


#SIMULACION
library(plyr) # raply
library(reshape2) # melt
library(dplyr)
library(ggplot2) # ggplot

## construyamos sucesiones de longitud n usando el algoritmo de rand
sucesion <- function(n = 1500, semilla = runif(1, 0, 2 ^ 31 - 1)){
  x <- rep(NA, n)
  u <- rep(NA, n)
  x[1] <- semilla # semilla
  u[1] <- x[1] / (2 ^ 31 - 1) # transformamos al (0, 1)
  for(i in 2:n){
    x[i] <- (7 ^ 5 * x[i - 1]) %% (2 ^ 31 - 1)
    u[i] <- x[i] / (2 ^ 31 - 1)
  }
  u
}


sucesiones <- t(raply(12, sucesion())) # generamos 12 sucesiones con 1500 entradas cada una
head(sucesiones)
sucesiones_m <- melt(sucesiones) # cambiamos los datos a forma "larga"
head(sucesiones_m)
ggplot(sucesiones_m, aes(x = Var1, y = value)) + 
  geom_point(alpha = 0.5, size = 1.5) +     # alpha controla la transparencia
  facet_wrap(~ Var2)
ggplot(sucesiones_m, aes(x = value)) + 
  geom_histogram(aes(y = ..density..), binwidth = 0.1) +
  facet_wrap(~ Var2)

