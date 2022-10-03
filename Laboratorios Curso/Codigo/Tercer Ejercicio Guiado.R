setwd("~/Desktop/Laboratorios Curso/DATOS/")

datos <- read.csv("Lab_2.csv", ";", header=T)

library(readxl)
Lab_2 <- read_excel("Lab 2.xlsx", sheet = "Hoja1")
View(Lab_2)

##PROBLEMA
#A partir del archivo anterior cree los siguientes vectores, una vez 
#listos cree una matriz con dichos resultados.
#1.	Var3 = (var1^2-var2^3)
#2.	Var4 = raíz(var1)/sumatotal(var2)
#3.	Var5 = var4*var3/var1
###

x<- datos$Var1
y <- datos$Var2

table(x)

creacion <- function(x,y)
{
  sumay<-0
  rx <- x
  ry <- y
  for (i in 1:length(y))
  {
    sumay<-sumay+y[i]
  }
  var3 <- c()
  var4 <- c()
  var5 <- c()
  u <- length(y)
  o <- 1
  b <-1
  for(i in 1:u)
  {
    var3[i] <- ((rx[i]*rx[i])-(ry[i]*ry[i]*ry[i]))
    var4[i] <- sqrt(rx[i])/sumay
    o <- o+1
  }
  for(i in 1:u)
  {
    var5[i] <- ((var4[i]*var3[i])/rx[i])
    b <- b+1
  }
  
  vec <- c(rx,ry,var3,var4,var5)
  #mat <- matrix(vec,nrow=23,ncol=5,byrow = TRUE)
  return(vec)
}

creacion(x,y)

