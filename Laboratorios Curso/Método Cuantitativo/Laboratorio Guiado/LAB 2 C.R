#setwd("C:/Users/UNED/Dropbox/escuela de estad?stica/Cursos de Sistemas Informaci?n 2009-2015/XS-2210 Estad?stica Computacional I 2017/Evaluaciones")
datos <- read.csv("Lab_2.csv", ";", header=T)
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

