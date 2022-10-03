
library(haven)
setwd("~/Desktop/Laboratorios Curso/DATOS/")
antropomexicano <- read_sav("antropomexicano.sav")
View(antropomexicano)
attach(antropomexicano)
names(antropomexicano)

#1.1 Prueba t-student

#La prueba "t" de Student es un tipo de estadística deductiva.
#Se utiliza para determinar si hay una diferencia significativa entre las medias de dos grupos. 
#Con toda la estadística deductiva, asumimos que las variables dependientes tienen una distribución normal
#. Especificamos el nivel de la probabilidad (nivel de la alfa, nivel de la significación, p) que estamos
#dispuestos a aceptar antes de que cerco datos (p < .05 es un valor común se utiliza que).

plot(edad)
boxplot(edad)
summary(edad)
a <- summary(edad)

mean(edad)
sd(edad)
t.test(edad)
t.test(edad ,mu=60.5)
t.test(edad,mu=55.5, alternative="less")

table(urbano)
t.test(urbano, mu=0.65)

#1.2 Prueba Kolmogorov Smirnov

#El procedimiento Prueba de Kolmogorov-Smirnov para una muestra compara la función de distribución acumulada observada de una variable con una
#distribución teórica determinada, que puede ser la normal, la uniforme, la de Poisson o la exponencial. La Z de Kolmogorov-Smirnov se calcula a
#partir de la diferencia mayor (en valor absoluto) entre las funciones de distribución acumuladas teórica y observada.
#Esta prueba de bondad de ajuste contrasta si las observaciones podrían razonablemente proceder de la distribución especificada.

ks.test(edad,"pnorm",mean(edad),sd(edad))
summary(edad)
sd(edad)
ks.test(edad,"pnorm",60.31,10.64586)

#1.3 Prueba Shapiro-Wilks

#El test de Shapiro-Wilks plantea la hipótesis nula que una muestra proviene de una distribución normal. Eligimos un nivel de significanza, 
#por ejemplo 0,05, y tenemos una hipótesis alternativa que sostiene que la distribución no es normal.

#Tenemos:
  
#H:0: La distribución es normal

#H1: La distribución no es normal,

#Ahora el test Shapiro-Wilks intenta rechazar la hipotesis nula a nuestro nivel de significanza. Para realizar el test usamos la función shapiro.test en R:
  
shapiro.test(edad)
ks.test(edad,"pexp",1/mean(edad))
hist(edad)

#1.4 Prueba de Jarque Bera

#En estadística, la prueba de Jarque-Bera es una prueba de bondad de ajuste para comprobar si una muestra de datos tiene la asimetría y 
#la curtosis de una distribución normal. La prueba recibe el nombre de Carlos Jarque y Anil K. Bera.

#install.packages("tseries")
library(tseries)
jarque.bera.test(edad)

#1.5 Contrastes para proporciones

urbano.vector<-table(urbano)
urbano.2<-1-urbano
urbano.vector<-table(urbano.2)
urbano.vector

binom.test(urbano.vector, p=0.65)
prop.test(1706, n=2573, p=0.65)

#1.6 Test de Wilcoxon

#El test de Mann–Whitney–Wilcoxon (WMW), también conocido como Wilcoxon rank-sum test o u-test, 
#es un test no paramétrico que contrasta si dos muestras proceden de poblaciones equidistribuidas.

#La idea en la que se fundamenta este test es la siguiente: si las dos muestras comparadas proceden de 
#la misma población, al juntar todas las observaciones y ordenarlas de menor a mayor, cabría esperar que 
#las observaciones de una y otra muestra estuviesen intercaladas aleatoriamente. Por lo contrario, si una de las 
#muestras pertenece a una población con valores mayores o menores que la otra población, al ordenar las observaciones, 
#estas tenderán a agruparse de modo que las de una muestra queden por encima de las de la otra.

median(edad)
wilcox.test(edad, mu=59)


#. Generación de muestras de una población con diferentes distribuciones de probabilidad. Bootstrap.

#install.packages("bootstrap")
library(bootstrap)
x<-rnorm(20)
theta<-function(x) {mean(x)}
results<-bootstrap(x,100,theta)
results

summary(results$thetastar)
hist(results$thetastar)

#2.2 Distribución muestral de una proporción con n=20

y<-rbinom(1000, size=1, prob=0.5)
theta<-function(y) {mean(y)}
results<-bootstrap(y,100,theta)
results

summary(results$thetastar)
hist(results$thetastar)

#2.3 Distribución muestral del percentil 95

perc95<-function(x) {quantile(x, .95)}
results<-bootstrap(x,100,theta, func=perc95)
results
summary(results$thetastar)

hist(results$thetastar)

results2<-boott(x,perc95)
results2

results3<-boott(x,theta)
results3


#3. Análisis de varianza de una vía

jacaranda<-c(4.5, 4.8, 3.9, 3.3, 4.2, 4.2, 4.2, 3.6, 3.9, 3.6, 3.6, 3.9, 3.3, 3.0, 3.6, 3.9, 4.5, 3.6, 3.6, 3.0, 3.9, 4.2, 3.0, 3.3, 3.3)
grupofactor<-as.factor(c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,
                         2,3,4,5))
bloque<-as.factor(c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,
                    5,5,5))
jacar<-data.frame(cbind(jacaranda,grupofactor,bloque) )

#Análisis exploratorio de variabilidad y normalidad
#Variabilidad

#install.packages("car")
#install.packages("carData")

library(car)
library(carData)

leveneTest(jacaranda, grupofactor)
bartlett.test(jacaranda, grupofactor)

#Normalidad

shapiro.test(jacaranda[grupofactor==1])
shapiro.test(jacaranda[grupofactor==2])
shapiro.test(jacaranda[grupofactor==3])
shapiro.test(jacaranda[grupofactor==4])
shapiro.test(jacaranda[grupofactor==5])

par(mfrow=c(2,3))

qqnorm(jacaranda[grupofactor==1])
qqline(jacaranda[grupofactor==1])

qqnorm(jacaranda[grupofactor==2])
qqline(jacaranda[grupofactor==2])

qqnorm(jacaranda[grupofactor==3])
qqline(jacaranda[grupofactor==3])

qqnorm(jacaranda[grupofactor==4])
qqline(jacaranda[grupofactor==4])

qqnorm(jacaranda[grupofactor==5])
qqline(jacaranda[grupofactor==5])

#Análisis de varianza de una vía

#Con el comando lm

anova1<-lm(jacaranda~grupofactor)
anova1

summary(anova1)
anova(anova1)

#Análisis de residuos

qqnorm(anova1$residuals)
qqline(anova1$residuals)

shapiro.test(anova1$residuals)
plot(anova1$fitted.values,anova1$residuals, xlab="Ajustados", ylab="Residuales")

#El otro commando es el aov (Permite utilizar el comando TukeyHSD)

anova2<-aov(jacaranda~grupofactor)
anova(anova2)

#Análisis cuando se tienen bloques

anova3<-lm(jacaranda~grupofactor+bloque)
anova(anova3)

#3.2 Comparaciones múltiples

#Tukey

#El método de Tukey se utiliza en ANOVA para crear intervalos de confianza para todas las diferencias en parejas entre 
#las medias de los niveles de los factores mientras controla la tasa de error por familia en un nivel especificado. 
#Es importante considerar la tasa de error por familia cuando se hacen comparaciones múltiples, porque la probabilidad de 
#cometer un error de tipo I para una serie de comparaciones es mayor que la tasa de error para cualquier comparación individual. 
#Para contrarrestar esta tasa de error más elevada, el método de Tukey ajusta el nivel de confianza de cada intervalo individual 
#para que el nivel de confianza simultáneo resultante sea igual al valor que usted especifique.

TukeyHSD(anova2)

#3.3 ANDEVA no paramétrico (Kruskal Wallis)

boxplot(jacaranda~grupofactor, xlab="Técnica", ylab="Altura")
kruskal.test(jacaranda~grupofactor)
jacaranda.rango<-rank(jacaranda, ties.method="average")
anova4<-(lm(jacaranda.rango~grupofactor))
anova(anova4)


#3.4 Prueba de aleatoriedad

#En estadística y análisis preliminar de datos, las pruebas de aleatoriedad (o test de aleatoriedad), 
#son pruebas estadísticas usadas para decidir si una determinada muestra o conjuntos de datos responde a un 
#patrón o puede considerarse aleatoria. En modelización estocástica, y algunas ciencia de la computación, 
#es deseable que algunos datos de entrada sean aleatorios y que dicha aleatoriedad pueda ser verificada por una 
#prueba cuantitativa de aleatoriedad, para mostrar que la simulación se realizó usando datos aleatorios y por 
#tanto representativos de una cierta distribución. En algunos casos, los datos muestran un patrón claramente 
#no aleatorio (por ejemplo si una variable debe presentar valores aleatorios que sean enteros entre 0 y 9,
#la secuencia "4 3 2 1 0 4 3 2 1..." es poco probable ya que en ningún caso los valores exceden el valor 4). 
#Si un conjunto de datos no pasa la prueba de aleatoriedad, entonces puede ser sustituida por otra serie de datos aleatorizados que pase el test de aleatoriedad.

median(jacaranda)
#install.packages("lawstat")
library(lawstat)

runs.test(jacaranda)

#4. Medidas de asociación 

load("mhasbasico.Rdata")
names(mhasbasico)

attach(mhasbasico)
summary(a1.1)
library(car)
escolaridad<-recode(escola, "99=NA")
summary(escolaridad)

cor(escola, a1.1, use="pairwise.complete.obs")
cor(escola,a1.1, use="pairwise.complete.obs", method="pearson")
cor(escola,a1.1, use="pairwise.complete.obs", method="spearman")
cor(escola,a1.1, use="pairwise.complete.obs", method="kendall")
cor.test(escola,a1.1, use="pairwise.complete.obs", method="pearson")
cor.test(escola,a1.1, use="pairwise.complete.obs", method="spearman")
cor.test(escola,a1.1, use="pairwise.complete.obs", method="kendall")
table(a25)
migracion<-recode(a25, "1=1; 2=0; 8=NA")
t(t(table(migracion)))
cor(migracion, a1.1, use="pairwise.complete.obs")

#4.2 Correlación biserial punctual

mean(a1.1, na.rm=TRUE)
sd(a1.1, na.rm=TRUE)
tapply(a1.1, migracion, mean, na.rm=TRUE)
t(t(table(migracion)))
corbp<-(8088.205-4235.739)/27364.34*(263*2666/(2929^2))^(0.5)
corbp

#4.3 Prueba X2

#La prueba chi-cuadrado, también llamada Ji cuadrado (Χ2), se encuentra dentro de las pruebas pertenecientes a la estadística descriptiva, concretamente la estadística descriptiva aplicada al estudio de dos variables. Por su parte, la estadística descriptiva se centra en extraer información sobre la muestra. En cambio, la estadística inferencial extrae información sobre la población.
#El nombre de la prueba es propio de la distribución Chi-cuadrado de la probabilidad en la que se basa. Esta prueba fue desarrollada en el año 1900 por Karl Pearson.
#La prueba chi-cuadrado es una de las más conocidas y utilizadas para analizar variables nominales o cualitativas, es decir, para determinar la existencia o no de independencia entre dos variables. Que dos variables sean independientes significa que no tienen relación, y que por lo tanto una no depende de la otra, ni viceversa.
#Así, con el estudio de la independencia, se origina también un método para verificar si las frecuencias observadas en cada categoría son compatibles con la independencia entre ambas variables.

median(a1.1, na.rm=TRUE)
ingreso.median<-recode(a1.1, "-2000000:1252.083=0; 1252.083:100000000=1")

t(t(table(ingreso.median)))
ingreso.por.migracion<-table(ingreso.median,migracion)

ingreso.por.migracion
chisq.test(ingreso.por.migracion)

#4.4 Coeficiente de contingencia

#El coeficiente de contingencia C (de Karl Pearson) es una medida de relación estadística. El coeficiente de contingencia de Pearson expresa la intensidad de la relación entre dos (o más) variables cualitativas. Se basa en la comparación de las frecuencias efectivamente calculadas de dos características con las frecuencias que se hubiesen esperado con independencia de estas características.

C<-sqrt(prueba.X2$statistic/(prueba.X2$statistic+length(ingreso.median)))
C

#4.5 V de Cramer

#La V de Cramér es una medida del tamaño del efecto para la prueba chi-cuadrado de la independencia. En él se mide la forma en que están asociados dos campos categóricos. El efecto del efecto es el de Cramér. Determinar qué campo tiene el número más alto de categorías

V<-sqrt(prueba.X2$statistic/(length(ingreso.median)))
V

#4.6 Prueba exacta Fisher

#La prueba de Fisher es el método exacto utilizado cuando se quiere estudiar si existe asociación entre dos variables cualitativas, es decir, si las proporciones de una variable son diferentes en función del valor de la otra variable

fisher.test(ingreso.median,migracion)
fisher.test(ingreso.por.migracion)
#Comparando
table(migracion)
prop.test(263,(2666+263),p=0.10)
binom.test(263,2929,p=0.10)
ingreso.por.migracion
prop.test(c(127,136),c(1464,1463))

#4.7 Odds ratio
OR<-(0.09295967/(1-.09295967))/(0.08674863/(1-0.08674863))
OR

#4.8 Razón de correlación o eta

#En Estadística, la razón de correlación es una medida de la relación entre la dispersión estadística entre categorías individuales y la dispersión entre la muestra o la población completa. es decir la varianza ponderada de las medias categóricas dividida por la varianza de todas la muestras .

religion<-recode(a39, "8:9=NA")
religion2<-as.factor(religion)

anova(lm(a1.1~religion2))
eta<-sqrt(1486100000/(1486100000+1727500000000))
eta

