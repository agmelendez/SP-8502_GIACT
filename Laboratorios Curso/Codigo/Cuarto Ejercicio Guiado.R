setwd("~/Desktop/Laboratorios Curso/DATOS/")

datos <- read.csv("Lab_2.csv", ";", header=T)

library(readxl)
Lab_2 <- read_excel("Lab 2.xlsx", sheet = "Hoja1")
View(Lab_2)

##PROBLEMA
#Tomando como insumo los datos var1 y var2, cree tres matriz de 3x3 y calcule las diferencias de las 3 matrices en una nueva es decir MF = MA-MB-MC
###

x<- datos$Var1
y <- datos$Var2

MA <- c()
MB <- c()
MC <- c()
MF <- c()

for (i in 1:9)
{
  MA[i] <- x[i]
  MB[i] <- y[i]
  MC[i] <- x[i+7]
  MF[i] <- 0
}

MA <- matrix(MA,nrow=3,ncol=3,byrow = TRUE, dimnames=list(c("A","B","C"),c("D","E","F")))
MB <- matrix(MB,nrow=3,ncol=3,byrow = TRUE, dimnames=list(c("A","B","C"),c("D","E","F")))
MC <- matrix(MC,nrow=3,ncol=3,byrow = TRUE, dimnames=list(c("A","B","C"),c("D","E","F")))
MF <- matrix(MF,nrow=3,ncol=3,byrow = TRUE, dimnames=list(c("A","B","C"),c("D","E","F")))

for(i in length(MA) )  
{
  for(j in length(MA))
  {
    MF[][] <- MA[][]-MB[][]-MC[][]
    
  }
}
MF

