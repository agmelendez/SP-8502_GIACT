#setwd("C:/Users/UNED/Dropbox/escuela de estad?stica/Cursos de Sistemas Informaci?n 2009-2015/XS-2210 Estad?stica Computacional I 2017/Evaluaciones")
datos <- read.csv("Lab_2.csv", ";", header=T)
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