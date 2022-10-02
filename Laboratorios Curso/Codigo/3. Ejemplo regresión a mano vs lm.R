library(faraway)
attach(savings)
ahorro<-savings[,-(2:4)]

### sr= tasa de ahorro (ahorros personales dividido por el ingreso disponible)
### ddpi= porcentaje de la tasa de crecimiento del ingreso disponible per capita

### Análisis gráfico (preliminar)
plot(sr~ddpi, xlab=" % tasa crecimiento ingreso per capita", ylab="Tasa de ahorro")

##Se crea la matriz X
n<-dim(ahorro)[1]
X<-as.matrix(cbind(rep(1,n),ahorro[,2]))
pred<-dim(X)[2]

##Se identifica la variable respuesta
Y<-ahorro[,1]
Y

##Estimación de los coeficientes
b<-solve(t(X)%*%X)%*%t(X)%*%Y
b


##Usando las funciones programadas de R 

##El modelo
mod<-lm(sr~ddpi)
summary(mod)
