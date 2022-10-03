#Día Laboratorio Curso SP-8502 “Técnicas de análisis estadístico”.
#SP-8502 “Métodos de análisis cuantitativos y cualitativos”
#PROGRAMA DE POSGRADO EN
#GESTION INTEGRADA DE AREAS COSTERAS TROPICALES (GIACT)


#Prof. Agustín Gómez Meléndez
#Correo: agustin.gomez@ucr.ac.cr


###Ejercicio 1. Manimuplación de Datos y archivos 

rm(list = ls())

setwd("~/Desktop/Laboratorios Curso/DATOS/")
load("energía.Rdata") #obtenidos de la cuenta de energía

#Verifique la dimensión de base

dim(base)
head(base)

#obtenga los nombres de las variables 

names(base)

#Explore la variable actividad

levels(base$actividad) #debería tener niveles

base$actividad<-as.factor(base$actividad) #convertir en factor

levels(base$actividad)

#Obtenga la media y desviación estandar de Gasolina

mean(base$Gasolina)
sd(base$Gasolina)
mean(base$Diésel)

#Obtenga la posición de las filas en las que el valor de gasolina es mayor a la media

which(base$Gasolina>mean(base$Gasolina))

#Obtenga los años en los que el valor de gasolina es mayor a la media

subset(base, Gasolina>mean(Gasolina))

#Haga lo mismo pero obteniendo solo el nombre de la actividad y el valor de gasolina

subset(base, Gasolina>mean(Gasolina), select=c("actividad", "Gasolina"))
plot(base$Gasolina)
boxplot(base$Gasolina)
hist(base$Gasolina)

######### MANIPULACIÓN DE DATOS ESPACIALES.

rm(list = ls())
#Referencia del código: 
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html


install.packages(c("cowplot", "googleway", "ggrepel", 
                   "ggspatial", "libwgeom", "rnaturalearth", "rnaturalearthdata"))

library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf()

ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))

ggplot(data = world) + 
  geom_sf(color = "black", fill = "lightgreen")

ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = "+init=epsg:3035")

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = st_crs(3035))

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)

library("ggspatial")
ggplot(data = world) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97))


ggsave("map.pdf")
ggsave("map_web.png", width = 6, height = 6, dpi = "screen")

####
library(dplyr)
library(EpiEstim)
library(readxl)
library(plotly)
library(radiant)
rm(list = ls())
excel_sheets("Datos_Zooplancton_Marino_Ballena.xlsx")
Datos <- read_excel("Datos_Zooplancton_Marino_Ballena.xlsx", sheet = "datos")
Datos
radiant()
