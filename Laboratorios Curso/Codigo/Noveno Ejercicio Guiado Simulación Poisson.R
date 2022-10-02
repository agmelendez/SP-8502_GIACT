#https://rpubs.com/JavierMtzG/ProcesoPoisson

ProcesoPois<- function(t,lambda){
  N<- rpois(1,t*lambda) #Paso 1
  C<- sort(runif(N,0,t)) #Paso 2 y 3 
  data.frame(x=c(0,0,C),y=c(0,0:N)) 
}
P<-ProcesoPois(3,3)
qplot(x,y,data=P,xlab="Tiempo",ylab="N(t)",main="Proceso de Poisson",geom=c("step","point"))
library(plyr)
NPois<-function(n,t,rate){
  C<- lapply(1:n, function(n) data.frame(ProcesoPois(t,rate),simulacion=n)) #Genera N dataframes con los procesos
  C<-ldply(C, data.frame) #Une en una sola dataframe
  C$simulacion<-factor(C$simulacion) #Convierte en factores
  qplot(x,y,data=C,geom=c("step","point"),color=simulacion,xlab="Tiempo",ylab="N(t)",main=sprintf("%d Simulaciones del Proceso de Poisson de Intensidad %.2f",n,rate))
}
NPois(5,10,2) #Intensidad = 2
NPois(10,10,0.1) #Intesidad Baja
NPois(5,1000,1) #Largo Plazo 
