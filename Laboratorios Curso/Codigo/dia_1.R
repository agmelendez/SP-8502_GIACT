#Día Laboratorio Curso SP-8502 “Técnicas de análisis estadístico”.
#SP-8502 “Métodos de análisis cuantitativos y cualitativos”
#PROGRAMA DE POSGRADO EN
#GESTION INTEGRADA DE AREAS COSTERAS TROPICALES (GIACT)
#Calendario: 	10 de agosto al 30 de noviembre de 2020: Lecciones virtuales
#30 de noviembre, 09:00 Horas: Examen Final Sesión Virtual
#Horario: 	De lunes, de las 09:00 a las 11:00 Sesión Virtual por Zoom


#Prof. Agustín Gómez Meléndez
#Correo: agustin.gomez@ucr.ac.cr


###Ejercicio 1. Manimuplación de Datos y archivos 

rm(list = ls())
setwd("~/Dropbox/Universidad de Costa Rica/GIACT CIMAR 2018/Cursos/d1")
load("energia.Rdata") #obtenidos de la cuenta de energía

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

#Obtenga la posición de las filas en las que el valor de gasolina es mayor a la media

which(base$Gasolina>mean(base$Gasolina))

#Obtenga los años en los que el valor de gasolina es mayor a la media

subset(base, Gasolina>mean(Gasolina))

#Haga lo mismo pero obteniendo solo el nombre de la actividad y el valor de gasolina

subset(base, Gasolina>mean(Gasolina), select=c("actividad", "Gasolina"))

###Ejercicio 1 
#Obtenga los años para los que el valor de Jet es menor a la media 


#Remueva los valores que son igual a 0



###Ejercicio Manipulación de Información Geoespacial 
rm(list = ls())
library(geojsonio)
library(sp)
spdf <- geojson_read("Areas_conservacion.json",  what = "sp")
plot(spdf)
library(ggplot2)
library(mapproj)
ggplot() +
        geom_polygon(data = spdf, aes( x = long, y = lat), fill="#69b3a2", color="white") +
        theme_void() +
        coord_map()

library(sf)
aoi_boundary_HARV <- st_read(
        "Areas_conservacion.shp")
st_geometry_type(aoi_boundary_HARV)
st_crs(aoi_boundary_HARV)
st_bbox(aoi_boundary_HARV)
aoi_boundary_HARV
ggplot() + 
        geom_sf(data = aoi_boundary_HARV, size = 3, color = "black", fill = "cyan1") + 
        ggtitle("AOI Boundary Plot") + 
        coord_sf()

#########
rm(list = ls())
#Fuente: https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html

library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
(sites <- data.frame(longitude = c(-80.144005, -80.109), latitude = c(26.479005, 
                                                                              26.83)))
ggplot(data = world) +
        geom_sf() +
        geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, 
                   shape = 23, fill = "darkred") +
        coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)
(sites <- st_as_sf(sites, coords = c("longitude", "latitude"), 
                   crs = 4326, agr = "constant"))
ggplot(data = world) +
        geom_sf() +
        geom_sf(data = sites, size = 4, shape = 23, fill = "darkred") +
        coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)
library("maps")
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)
states <- cbind(states, st_coordinates(st_centroid(states)))
library("tools")
states$ID <- toTitleCase(states$ID)
head(states)
ggplot(data = world) +
        geom_sf() +
        geom_sf(data = states, fill = NA) + 
        geom_text(data = states, aes(X, Y, label = ID), size = 5) +
        coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)
states$nudge_y <- -1
states$nudge_y[states$ID == "Florida"] <- 0.5
states$nudge_y[states$ID == "South Carolina"] <- -1.5
ggplot(data = world) +
        geom_sf() +
        geom_sf(data = states, fill = NA) + 
        geom_label(data = states, aes(X, Y, label = ID), size = 5, fontface = "bold", 
                   nudge_y = states$nudge_y) +
        coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("florida", counties$ID))
counties$area <- as.numeric(st_area(counties))
head(counties)
ggplot(data = world) +
        geom_sf() +
        geom_sf(data = counties, fill = NA, color = gray(.5)) +
        coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)


####
library(dplyr)
library(EpiEstim)
library(readxl)
library(plotly)
library(readxl)
library(radiant)
rm(list = ls())
excel_sheets("Datos_Zooplancton_Marino_Ballena.xlsx")
Datos <- read_excel("Datos_Zooplancton_Marino_Ballena.xlsx", sheet = "datos")
Datos
radiant()