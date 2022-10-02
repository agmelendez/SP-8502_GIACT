# Laboratorio para Simulación de variables según distribuciones de probablidad
# Curso Métodos y mediciones
# Maestría del GIACT - UCR 
# Año 2022
# Profesor: MSI. Agustín Gómez Meléndez
# Correo: agustin.gomez@ucr.ac.cr 
# Tomado de: https://bookdown.org/content/944ffa0f-050e-47cb-afaa-4dff15a9ed00/software.html

install.packages(c("tidyverse", "simmer", "simmer.bricks", "simmer.plot", "diagram", "markovchain", "queuecomputer", "queuecomputer", "rootSolve", "sjPlot", "gridExtra", "kableExtra"))
install.packages(c("diagram", "markovchain"))

library(tidyverse)
library(simmer)
library(simmer.bricks)
library(simmer.plot)
library(diagram)
library(markovchain)
library(queueing)
library(queuecomputer)
library(rootSolve)
# Librerías de entorno gráfico
library(sjPlot)
library(gridExtra)
library(kableExtra)


#Configuramos además el tema de los gráficos para que tengan un aspecto más limpio y más fácil de 
#exportar en formato pdf o word. Para ellos utilizamos la función theme_set().     

theme_set(theme_sjplot2())

# En la situación del ejemplo 1.1, supongamos que disponemos de un conjunto de 200 simulaciones del número de 
#huevos defectuosos en 200 cajas distribuídas. Queremos descubrir, utilizando exclusivamente esas simulaciones:
#La probabilidad de que una caja tenga más de 3 huevos defectuosos,  
#La probabilidad de que una caja tenga a lo sumo 3 huevos defectuosos,  
#La probabilidad de que una caja no tenga ningún huevo defectuoso,  
#La probabilidad de que la proporción de huevos defectuosos en una caja sea inferior al 1%,  


defectos <- c(2, 2, 0, 0, 0, 2, 1, 2, 1, 4, 1, 0, 0, 2, 4, 
              0, 0, 0, 0, 1, 1, 1, 2, 2, 3, 1, 0, 4, 3, 1, 0, 
              2, 2, 2, 3, 1, 0, 2, 2, 2, 3, 1, 0, 1, 0, 1, 2, 
              0, 0, 2, 3, 2, 3, 2, 4, 4, 0, 1, 1, 3, 0, 0, 3, 
              2, 0, 0, 0, 3, 0, 1, 4, 1, 1, 2, 1, 1, 4, 1, 1, 
              1, 0, 1, 0, 1, 2, 2, 1, 3, 1, 2, 1, 2, 3, 1, 2, 
              5, 1, 1, 1, 1, 0, 1, 1, 1, 2, 1, 0, 0, 1, 2, 2, 
              1, 1, 1, 1, 0, 3, 1, 1, 1, 1, 4, 4, 0, 6, 6, 1, 
              1, 1, 0, 2, 3, 1, 0, 0, 2, 0, 2, 1, 1, 1, 2, 1, 
              1, 1, 1, 2, 5, 0, 1, 3, 1, 1, 4, 1, 2, 1, 1, 0, 
              2, 1, 2, 1, 3, 3, 2, 0, 3, 0, 1, 3, 0, 1, 2, 0, 
              1, 0, 0, 2, 2, 1, 2, 0, 0, 0, 1, 1, 2, 3, 1, 0, 
              1, 0, 1, 1, 1, 1, 1, 5, 3)
# Número de simulaciones/observaciones

plot(defectos)
boxplot(defectos)
summary(defectos)

nsim=length(defectos)
# Tamaño de la caja
tamaño <- rep(144,200)

# Conjunto de datos
huevos <- data.frame(tamaño, defectos)
# Pr(X > 3)
sel <- dplyr::filter(huevos, defectos >3)
prob <- nrow(sel)/nsim
cat("Probabilidad estimada [Pr(X > 3)]: ", prob)

# Pr(X <= 3)

sel <- dplyr::filter(huevos, defectos <= 3)
prob <- nrow(sel)/nsim
cat("Probabilidad estimada [Pr(X <= 3)]: ", prob)

# Pr(X = 0)

sel <- dplyr::filter(huevos, defectos == 0)
prob <- nrow(sel)/nsim
cat("Probabilidad estimada [Pr(X = 0): ", prob)

# Pr(X/144 <=0.01)

sel <- dplyr::filter(huevos, defectos <= (0.01*144))
prob <- nrow(sel)/nsim
cat("Probabilidad estimada [Pr(X/144 <=0.01): ", prob)

#El error asociado a la estimación de la probabilidad anterior, lo calculamos aplicando (??), y también el intervalo de confianza al 95% con (1.1).

I.a=(huevos$defectos <= 1.44)*1
prob=mean(I.a)
cat("prob.estim=",prob)

vn=sum((I.a-prob)^2)/(nsim^2)
error=sqrt(vn)
cat("Error Estimado=",round(error,3))

# límites del IC redondeados a 3 cifras decimales
ic.low=round(prob-qnorm(0.975)*error,3)
ic.up=round(prob+qnorm(0.975)*error,3)
cat("IC(95%)[AproxMC(prob.estim)]=[",ic.low,",",ic.up,"]")


# Con los datos del ejemplo anterior, queremos saber cuál es el número aproximado de 
#huevos que se rompen en cada caja, conocer su dispersión y tener así mismo un intervalo 
#de confianza para la media.


# media
media=mean(huevos$defectos)

# dispersión

varianza=var(huevos$defectos)
desvtip=sd(huevos$defectos)

# ic para la media

error=sqrt(sum((huevos$defectos-media)^2)/(nsim^2))

cat("\n Error Estimado (media)=",round(error,3))

# límites del IC redondeados a 3 cifras decimales

ic.low=round(media-qnorm(0.975)*error,3)
ic.up=round(media+qnorm(0.975)*error,3)

cat("IC(95%)[AproxMC(media)]=[",ic.low,",",ic.up,"]")



# Binomial

#Estamos revisando en una empresa el comportamiento de las bajas laborales. En base al histórico, 
#se tiene que cada día aproximadamente el 3% de los trabajadores faltan al trabajo alegando una baja laboral. 
#Si el número de trabajadores de la empresa es de 150, queremos saber qué porcentaje de días vamos a tener al menos 3 trabajadores de baja.

#Denotemos por  
#    N
#a la variable aleatoria que indica el número de trabajadores de baja en un día cualquiera, e identifiquemos el
#“éxito” por el hecho de “estar de baja.” Así, podemos asumir que la distribución de  
#    N
#es binomial, con tamaño 50 y probabilidad 0.03.

xs <- 0:50
n=50
p=0.03

# Data frame
datos <- data.frame(xs = xs, probs = dbinom(xs, n,p), 
                    probsacum = pbinom(xs, n,p))

# función de masa de probabilidad
g1 <- ggplot(datos, aes(x=xs, y=probs)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  ylim(0,0.5) +
  labs(x ="x", y = "Probabilidad puntual. Pr(N=x)")

# función de distribución

g2 <- ggplot(datos, aes(xs, probsacum)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(breaks = scales::breaks_extended(10)) +
  labs(x ="x", y ="Probabilidad acumulada. Pr(N<=x)")
grid.arrange(g1, g2, nrow = 1)


nsim=1000
n=50
p=0.03
# valor real de la probabilidad
prob=1-pbinom(2,n,p)
cat("Pr(N>=3)=",round(prob,3))


set.seed(1234)
# simulaciones
I.a=(rbinom(nsim,n,p)>=3)*1  # función indicatriz para la probabilidad requerida
prob=mean(I.a)
cat("AproxMC=",prob)

#El error estimado de esta aproximación, que calculamos con la raíz cuadrada de (??) es

error=sqrt(sum((I.a-prob)^2)/(nsim^2))
cat("Error.AproxMC=",round(error,3))

# límites del IC redondeados a 3 cifras decimales
ic.low=round(prob-qnorm(0.975)*error,3)
ic.up=round(prob+qnorm(0.975)*error,3)
cat("IC(95%)[AproxMC]=[",ic.low,",",ic.up,"]")


#Geométrica

# Valores de N
xs <- seq(0, 60, 1)
# Data frame
datos <- data.frame(xs = xs, probs = dgeom(xs, 0.1), 
                    probsacum = pgeom(xs, 0.1))

# función de masa de probabilidad

g1 <- ggplot(datos, aes(xs, probs)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  ylim(0,0.12) +
  labs(x ="x", y = "Probabilidad puntual. Pr(N=x)")

# función de distribución

# Generate some sample data, then compute mean and standard deviation
# in each group

g2 <- ggplot(datos, aes(xs, probsacum)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(breaks = scales::breaks_extended(10)) +
  labs(x ="x", y = "Probabilidad acumulada Pr(N<=x)")
grid.arrange(g1, g2, nrow = 1)

set.seed(1970)
nsim <- 10000
prob <- 0.1
media<-(1-prob)/prob
cat("E(N)=",media)

# Valores simulados
datos <- rgeom(nsim, prob)
# Aproximación MC del valor esperado
m=round(mean(datos),0)
cat("AproxMC=",m)

# Error MC
error=sqrt(sum((datos-m)^2)/(nsim^2))
# límites del IC redondeados a 3 cifras decimales
ic.low=round(m-qnorm(0.975)*error,3)
ic.up=round(m+qnorm(0.975)*error,3)
cat("IC(95%)[AproxMC]=[",ic.low,",",ic.up,"]")
## IC(95%)[AproxMC]=[ 8.811 , 9.189 ]

#Poisson

# Valores de N
lambda=4
xs <- seq(0, 10, 1)
# Data frame
datos <- data.frame(xs = xs, probs = dpois(xs, lambda), 
                    probsacum = ppois(xs, lambda))
# función de masa de probabilidad
g1 <- ggplot(datos, aes(xs, probs)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_continuous(breaks = 0:10, labels = 0:10) +
  ylim(0,0.3) +
  labs(x ="x", y = "Probabilidad puntual. Pr(N=x)")
# función de distribución
g2 <- ggplot(datos, aes(xs, probsacum)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_continuous(breaks = 0:10, labels = 0:10) +
  scale_y_continuous(breaks = scales::breaks_extended(10)) +
  labs(x ="x", y = "Probabilidad acumulada Pr(N<=x)")
grid.arrange(g1, g2, nrow = 1)

lambda=4
# Probabilidad buscada P(N=3) para la poisson con media 2
media=dpois(3,lambda)
cat("Pr(N=4)=",round(media,3))

nsim <- 10000
# Simulamos de la poisson y evaluamos la función indicatriz para la prob de interés
set.seed(1970)
I.a <- (rpois(nsim, lambda)==3)*1
# Realizamos la aproximación MC
m=mean(I.a)
cat("AproxMC=",m)
## AproxMC= 0.1934
# Error MC
error=sqrt(sum((I.a-m)^2)/(nsim^2))
# límites del IC redondeados a 3 cifras decimales
ic.low=round(m-qnorm(0.975)*error,3)
ic.up=round(m+qnorm(0.975)*error,3)
cat("IC(95%)[AproxMC]=[",ic.low,",",ic.up,"]")



lambda=4
# Probabilidad buscada P(N=3) para la poisson con media 2
media=dpois(3,lambda)
cat("Pr(N=4)=",round(media,3))

nsim <- 10000*10
# Simulamos de la poisson y evaluamos la función indicatriz para la prob de interés
set.seed(1970)
I.a <- (rpois(nsim, lambda)==3)*1
# Realizamos la aproximación MC
m=mean(I.a)
cat("AproxMC=",m)
# Error MC
error=sqrt(sum((I.a-m)^2)/(nsim^2))
# límites del IC redondeados a 3 cifras decimales
ic.low=round(m-qnorm(0.975)*error,3)
ic.up=round(m+qnorm(0.975)*error,3)
cat("IC(95%)[AproxMC]=[",ic.low,",",ic.up,"]")


# Paso 1
set.seed(1970)
nsim <- 5000
lams <- seq(0.1, 5, 0.01) # valores de lambda
nlams <- length(lams)     # número de lambdas para evaluar
prob <- c()  # vector de probabilidades

# Pasos 2 y 3
for(i in 1:nlams){
  datos <- rpois(nsim, lams[i])    
  prob[i] <- mean(datos >= 3)   
}

# Paso 4. Resultado del problema
lambda=lams[min(which(prob >= 0.8))];lambda

# Pintamos los resultados de la simulación realizada
dat=data.frame(lams=lams,prob=prob)
ggplot(dat,aes(x=lams,y=prob))+
  geom_point()+
  geom_hline(yintercept=0.8)+
  geom_vline(xintercept=lambda)+
  labs(x="lambda",y="Pr(N>=3)")


# Exponencial

lambda <- 1/16
# Data frame para la representación gráfica
sec <- seq(0, 80, by = 0.01)
datos<- data.frame(sec = sec, densidad = dexp(sec,lambda))
# Gráfico función de densidad
ggplot(datos, aes(sec, densidad)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(0,80,5), labels = seq(0,80,5)) +
  scale_y_continuous(breaks = scales::breaks_extended(10)) +
  geom_vline(xintercept = 20, col = "red") +
  labs(x ="Tiempo de vida del marcapasos (en años)", 
       y = "Función de densidad")

# Probabilidad real
lambda=1/16
p=pexp(20,lambda)
cat("Pr(T<=20)=",round(p,3))

# Parámetros de la simulación
set.seed(123)
nsim <- 5000
# Simulaciones
datos <- rexp(nsim, lambda)
# Probabilidad de interés
pMC=mean(datos <= 20)
cat("Aprox.MC[Pr(T<=20)]=",round(pMC,3))


# Un motor eléctrico tiene una vida media de 6 años y se modeliza con una distribución exponencial. 
#¿Cuál debe ser el tiempo de garantía que debe tener el motor si se desea que a lo sumo el 15 % de los motores fallen antes de que expire su garantía?
# Si   T es la variable aleatoria que indica el tiempo de vida del producto tenemos que:

#  T∼Exp(λ=1/6).

# En este caso estamos interesados en encontrar el tiempo para que podamos garantizar que el 85% de los motores siguen funcionando, 
#es decir, buscamos el cuantil 0.15 de la distribución de  
# T. Planteamos un análisis de simulación para estimar dicho valor.

# Calculamos el valor real para el periodo de garantía
lambda <- 1/6
q=qexp(0.15,lambda)
cat("Periodo de garantía recomendado=",round(q,2))

# Parámetros de la simulación
set.seed(123)
nsim <- 5000
# simulaciones
datos <- rexp(nsim, lambda)
# cuantil de interés
qMC=quantile(datos, 0.15)
cat("Periodo de garantía aproximado=",round(qMC,2))

# cuantil de interés
probs <- seq(0.05, 0.95, by = 0.05)
cuantiles <- quantile(datos, probs)
datoscuan <- data.frame(probs, cuantiles)
# Gráfico
ggplot(datoscuan, aes(probs,cuantiles)) + 
  geom_line() +
  scale_x_continuous(breaks = probs, labels = probs) +
  scale_y_continuous(breaks = scales::breaks_extended(10)) +
  geom_vline(xintercept = 0.15, col = "red") +
  labs(x ="Probabilidad", y = "Tiempo a la reparación (en años)")


#Gamma

# Función para generar "nsim" simulaciones de una Erlang 
# con parámetros k (entero) y beta>0
rerlang <- function(nsim, k, beta)
{
  # verificamos que k es entero
  if(k%%1 == 0)
  {
    # parámetro de la exponencial
    lambda <- beta/k
    # Generamos y almacenamos datos exponenciales
    datosexp <- matrix(rexp(nsim*k, lambda), nrow = nsim)
    # Obtenemos la muestra de la Erlang
    datoserl <- apply(datosexp, 1, sum)
    return(datoserl)
  }
  else{
    cat("k debe ser entero")
  }
}

estima.weibull <- function(m, s)
{
  #m=media, s=desviación típica
  library(rootSolve)
  # Función para optimizar alpha
  fun.alpha <- function(a, m, s)
  {
    res<- 1 + (s/m)^2 - gamma(1+2/a)/(gamma(1+1/a))^2
    return(res)
  }
  # Obtención de alpha
  alpha <- round(uniroot(fun.alpha, c(0.1, 10000),m=m,s=s)$root,2)
  # Obtención de beta
  beta <- round(m/gamma(1+1/alpha), 2)
  # Devolvemos alpha y beta
  return(c(alpha, beta))
}

# Datos de ejemplo
m <- 80     # media
s <- sqrt(50)  # desviación típica
# Estimación
res=estima.weibull(m,s)
cat("Weibull alpha=",res[1],", Weibull beta=",res[2])


#Normal

x=seq(-10,10,0.1)
y1=dnorm(x)
y2=dnorm(x,0,3)
y3=dnorm(x,2,1)
y4=dnorm(x,2,3)
datos=as.tibble(cbind(x,y1,y2,y3,y4))
levels=c("N(0,1)"="y1","N(0,3)"="y2","N(2,1)"="y3","N(2,3)"="y4")
datos=datos %>%
  pivot_longer(cols=2:5,names_to="tipo",values_to="valor") 
datos$tipo=fct_recode(datos$tipo,!!!levels)

ggplot(datos,aes(x=x,y=valor,color=tipo))+
  geom_line()+
  labs(color="Distribuciones",y="Función de densidad")

#T-Student

x=seq(-5,5,0.1)
y1=dt(x,2)
y2=dt(x,5)
y3=dt(x,10)
y4=dnorm(x)
datos=as.tibble(cbind(x,y1,y2,y3,y4))
levels=c("St(2)"="y1","St(5)"="y2","St(10)"="y3","N(0,1)"="y4")
datos=datos %>%
  pivot_longer(cols=2:5,names_to="tipo",values_to="valor") 
datos$tipo=fct_recode(datos$tipo,!!!levels)

ggplot(datos,aes(x=x,y=valor,color=tipo))+
  geom_line()+
  labs(color="Distribuciones",y="Función de densidad")


x=seq(0,200,0.1)
y1=dchisq(x,5)
y2=dchisq(x,10)
y3=dchisq(x,50)
y4=dchisq(x,100)
datos=as.tibble(cbind(x,y1,y2,y3,y4))
levels=c("Chi2(5)"="y1","Chi2(10)"="y2","Chi2(50)"="y3","Chi2(100)"="y4")
datos=datos %>%
  pivot_longer(cols=2:5,names_to="tipo",values_to="valor") 
datos$tipo=fct_recode(datos$tipo,!!!levels)

ggplot(datos,aes(x=x,y=valor,color=tipo))+
  geom_line()+
  labs(color="Distribuciones",y="Función de densidad")

x=seq(0,5,0.01)
y1=df(x,5,5)
y2=df(x,1,5)
y3=df(x,50,10)
y4=df(x,100,200)
datos=as.tibble(cbind(x,y1,y2,y3,y4))
levels=c("F(5,5)"="y1","F(1,5)"="y2","F(50,10)"="y3","F(100,200)"="y4")
datos=datos %>%
  pivot_longer(cols=2:5,names_to="tipo",values_to="valor") 
datos$tipo=fct_recode(datos$tipo,!!!levels)

ggplot(datos,aes(x=x,y=valor,color=tipo))+
  geom_line()+
  labs(color="Distribuciones",y="Función de densidad")



########
#######
#https://r-coder.com/distribucion-poisson-r/

dpois(x,           # Valores del eje X (x = 0, 1, 2, ...)
      lambda,      # Número medio de eventos que ocurren en el intervalo
      log = FALSE) # Si TRUE, las probabilidades se devuelven como log
dpois(0:10, lambda = 5)
dpois(5, lambda = c(5, 10))


# Rejilla de valores del eje X
x <- 0:50

#-----------
# lambda: 5
#-----------
lambda <- 5
plot(dpois(x, lambda), type = "h", lwd = 2,
     main = "Función de masa de probabilidad",
     ylab = "P(X = x)", xlab = "Número de eventos")

#-----------
# lambda: 10
#-----------
lambda <- 10
lines(dpois(x, lambda), type = "h", lwd = 2, col = rgb(1,0,0, 0.7))

#-----------
# lambda: 20
#-----------
lambda <- 20
lines(dpois(x, lambda), type = "h", lwd = 2, col = rgb(0, 1, 0, 0.7))

# Leyenda
legend("topright", legend = c("5", "10", "20"),
       title = expression(lambda), title.adj = 0.75,
       lty = 1, col = 1:3, lwd = 2, box.lty = 0)


ppois(q,                 # Cuantil o vector de cuantiles
      lambda,            # Media o vector de medias
      lower.tail = TRUE, # Si TRUE, las probabilidades son P(X <= x), o P(X > x) en otro caso
      log.p = FALSE)     # Si TRUE, las probabilidades se devuelven como log
ppois(5, lambda = 10)
sum(dpois(0:5, lambda = 10))
ppois(10, lambda = 15) # 0.1184644 o 11.8%
1 - ppois(10, lambda = 15, lower.tail = FALSE) # Equivalente
sum(dpois(0:10, lambda = 15)) # Equivalente


# lambda: media
# lb: límite inferior de la suma
# ub: límite supeior de la suma
# col: color
# lwd: ancho de línea
pois_sum <- function(lambda, lb, ub, col = 4, lwd = 1, ...) {
  x <- 0:(lambda + lambda * 2)
  
  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }
  
  plot(dpois(x, lambda = lambda), type = "h", lwd = lwd, ...)
  
  if(lb == min(x) & ub == max(x)) {
    color <- col
  } else {
    color <- rep(1, length(x))
    color[(lb + 1):ub ] <- col
  }
  
  lines(dpois(x, lambda = lambda), type = "h",
        col =  color, lwd = lwd, ...)
}

pois_sum(lambda = 10, lb = 10, ub = 15, lwd = 2,
         col = 2, ylab = "P(X = x)", xlab = "Número de eventos")

pois_sum(lambda = 15, ub = 10, lwd = 2,
         ylab = "P(X = x)", xlab = "Visitas por hora")

ppois(20, lambda = 15, lower.tail = FALSE) # 0.08297091 o 8.3%
1 - ppois(20, lambda = 15)        # Equivalente
1 - sum(dpois(0:20, lambda = 15)) # Equivalente

pois_sum(lambda = 15, lb = 20, lwd = 2,
         ylab = "P(X = x)", xlab = "Visitas por hora")

ppois(14, lambda = 15, lower.tail = FALSE) # 0.5343463 o 53.43%
1 - sum(dpois(0:15, lambda = 15)) # Equivalente

pois_sum(lambda = 15, ub = 14, lwd = 2,
         ylab = "P(X = x)", xlab = "Visitas por hora")

ppois(20, lambda = 15) - ppois(10, lambda = 15) # 0.7985647 o 79.86%
sum(dpois(11:20, lambda = 15)) # Equivalente

pois_sum(lambda = 15, lb = 10, ub = 20, lwd = 2,
         ylab = "P(X = x)", xlab = "Visitas por hora")

# Rejilla de valores del eje X
x <- 0:50

#-----------
# lambda: 5
#-----------
lambda <- 5
plot(ppois(x, lambda), type = "s", lwd = 2,
     main = "Función de distribución",
     xlab = "Número de eventos", ylab = "F(x)")

#-----------
# lambda: 10
#-----------
lambda <- 10
lines(ppois(x, lambda), type = "s", lwd = 2, col = 2)

#-----------
# lambda: 20
#-----------
lambda <- 20
lines(ppois(x, lambda), type = "s", lwd = 2, col = 3)

# Legend
legend("bottomright", legend = c("5", "10", "20"),
       title = expression(lambda), title.adj = 0.75,
       lty = 1, col = 1:3, lwd = 2, box.lty = 0)


qpois(p,                 # Probabilidad o vector de probabilidades
      lambda,            # Media o vector de medias
      lower.tail = TRUE, # Si TRUE, las probabilidades son P(X <= x), o P(X > x) en otro caso
      log.p = FALSE)     # Si TRUE, las probabilidades se devuelven como log
qpois(0.5, lambda = 10) # 10
#-----------
# lambda: 20
#-----------
plot(qpois(seq(0, 1, 0.001), lambda = 20),
     main = "Función cuantil",
     ylab = "Q(p)", xlab = "p",
     type = "s", col = 3, xaxt = "n")

axis(1, labels = seq(0, 1, 0.1), at = 0:10 * 100)

#-----------
# lambda: 10
#-----------
lines(qpois(seq(0, 1, 0.001), lambda = 10), type = "s", col = 2)

#-----------
# lambda: 5
#-----------
lines(qpois(seq(0, 1, 0.001), lambda = 5), type = "s")

# Legend
legend("topleft", legend = c("5", "10", "20"),
       title = expression(lambda), title.adj = 0.75,
       lty = 1, col = 1:3, lwd = 2, box.lty = 0)

rpois(n,      # Número de observaciones a ser generadas
      lambda) # Media o vector de medias
rpois(10, lambda = 4)
set.seed(10)
rpois(10, lambda = 4)
