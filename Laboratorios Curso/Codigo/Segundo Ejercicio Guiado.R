setwd("~/Desktop/Laboratorios Curso/DATOS/")

### datos <- read.csv("Lab_2.csv", ";", header=T)

library(readxl)
Lab_2 <- read_excel("Lab 2.xlsx", sheet = "Hoja1")
View(Lab_2)


datos <- Lab_2
ordenar<- function(vector)  #Funci?n burbuja para ordenar datos.
{
  c<-TRUE
  while(c)
  {
    c<-FALSE
    for(i in 1:(length(vector)-1))
    {
      if(vector[i]>vector[i+1])
      {
        w<-vector[i+1]
        vector[i+1]<-vector[i]
        vector[i]<-w
        c<-TRUE
      }
    }
    
  }
  return(vector)
}
compara <- function(x) # Esta funci?n dejar s?lo los valores ?nicos dentro de un conjunto de datos
{
  m <- length(x)
  contar <- 1
  w <- c()
  j<-x
  
  for(i in 1:(m-1))
  {
    if (x[i]!=x[i+1])
    {
      w[contar]<- x[i]
      contar<- contar+1
    }
  }
  w2 <- c(w,x[m])
  return(w2)  
}

int <- function(x,y)  #Esta Funci?n hace la intersecci?n de los dos conjuntos de datos
{
  w <- c()
  o <-1
  for(i in 1:(length(x)))
  { 
    for(j in 1:(length(y)))
    {
      if(x[i]==y[j])
      {
        w[o]<-y[j]
        o<-o+1
      }  
    }
  }
  print(w)
}

proceso <- function(x,y)
{
  xa <- ordenar(x)
  yb <- ordenar(y)
  x1 <- compara(xa)
  y1 <- compara(yb)
  print("La intersección de datos es:")
  int(x1,y1)
  uni <- c(x1,y1)
  print("E vector Final es:")
  r <- ordenar(uni)
  rc <- compara(r)
  print(rc)
}
x<- datos$Var1
y <- datos$Var2
proceso(x,y)

