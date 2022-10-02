

library(faraway)
data(gala)
names(gala)
modelo<-lm(gala$Species~gala$Endemics)
modelo
lm(formula = gala$Species ~ gala$Endemics)
summary(modelo)
lm(formula = gala$Species ~ gala$Endemics)

  
  

gala$fitted <- fitted(modelo)
gala$residuals <- residuals(modelo)
gala$rstudent <- rstudent(modelo)
gala$cooks <- cooks.distance(modelo)

plot(gala$fitted)
plot(gala$residuals)
plot(gala$rstudent)
plot(gala$cooks)


library(readxl)
nkd <- read_excel("nkd.xlsx")
View(nkd)

library(foreign)
factorial <- lm(Produccion  ~ D +K +N +D *K +D *N +K *N +D *K *N , data=nkd)
summary(factorial)

lm(formula = Produccion ~ D + K + N + D * K + D * N + K * N + D * 
     K * N, data = nkd)
anova(factorial)



