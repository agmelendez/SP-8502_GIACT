#Crear una función que simule n observaciones del siguiente modelo que tiene el vector de parámetros  Θ=(β0=4,β1=−6,σ=4)⊤.
# Tomado: https://fhernanb.github.io/libro_regresion/


gen_dat <- function(n) {
  varianza <- 16
  x <- runif(n=n, min=-5, max=6)
  media <- 4 - 6 * x
  y <- rnorm(n=n, mean=media, sd=sqrt(varianza))
  marco_datos <- data.frame(y=y, x=x)
  return(marco_datos)
}
datos <- gen_dat(n=55)
datos

datos <- gen_dat(n=1000)

library(ggplot2)
ggplot(datos, aes(x=x, y=y)) +
  geom_point() + theme_light()

mod <- lm(y ~ x, data=datos)
theta_hat <- c(coef(mod), sigma=summary(mod)$sigma)
theta_hat


datos <- cars[1:5, ]
mod <- lm(dist ~ speed, data=datos)
simulate(object=mod, nsim=1, seed=1234)

