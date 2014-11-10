
rm(list = ls())
setwd("/Users/loredp/Documents/ITAM/11vo/PAMI/Trabajo/Bases de datos/INP")

# Bibliotecas
library(tidyr)
library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)

# Lectura de bases de datos. Cada número se refiere a un mes del ano 2014.
# Se tiene información de los primeros 5 bimestres.

datos1 <- read.csv("INP_PreciosPromedio12.CSV", header = T)
datos2 <- read.csv("INP_PreciosPromedio34.CSV", header = T)
datos3 <- read.csv("INP_PreciosPromedio56.CSV", header = T)
datos4 <- read.csv("INP_PreciosPromedio78.CSV", header = T)
datos5 <- read.csv("INP_PreciosPromedio910.CSV", header = T)

# Unir las bases de datos en una sola

datos <- rbind(datos4, datos5)

# Se añade una columna de fecha en lugar de las columna de mes y año
datos$Fecha <- paste(datos$Mes, datos$Año, sep = "/")
datos <- datos[, c(3:7)]

# Cambiar los nombres para mayor facilidad de manejo de datos
names(datos)<- c("Clave", "Ciudad", "Prenda", "Precio", "Fecha")

# Agrupar los tipos de prendas en 11 categorías más básicas

# TRAJE
# Se reemplazaron las entradas relacionadas con traje para almacenarlas en una sola categoría
# Incluye traje 2 piezas, traje de lino, traje sastre y traje para caballero

datos$Prenda <- gsub("LAVADO Y PLANCHADO, DE TRAJE, 2 PZAS", "TRAJE", datos$Prenda)
datos$Prenda <- gsub("LAVADO Y PLANCHADO, TRAJE, 2 PZAS", "TRAJE", datos$Prenda)
datos$Prenda <- gsub("LAVADO  Y PLANCHADO, DE TRAJE, 2 PZAS", "TRAJE", datos$Prenda)
datos$Prenda <- gsub("LAVADO Y PLANCHADO, DE TRAJE SASTRE, EN SECO", "TRAJE", datos$Prenda)
datos$Prenda <- gsub("LAVADO Y PLANCHADO, DE TRAJE SASTRE", "TRAJE", datos$Prenda)
datos$Prenda <- gsub("LAVADO Y PLANCHADO, TRAJE 2 PZAS", "TRAJE", datos$Prenda)
datos$Prenda <- gsub("LAVADO Y PLANCHADO, TRAJE DE LINO, 2 PZAS", "TRAJE", datos$Prenda)
datos$Prenda <- gsub("LAVADO Y PLANCHADO, DE TRAJE, P/CABALLERO, 2 PZAS", "TRAJE", datos$Prenda)

# CHAMARRA
# Se reemplazaron las entradas relacionadas con chamarra.
# Incluye chamarra sencilla, chammara de pluma de ganso

unique(grep("CHAMARRA",datos$Prenda, value = T)) 

datos$Prenda <- gsub("LAVADO Y PLANCHADO, DE CHAMARRA SENCILLA", "CHAMARRA", datos$Prenda)
datos$Prenda <- gsub("LAVADO Y PLANCHADO, DE CHAMARRA, PLUMA DE GANSO", "CHAMARRA", datos$Prenda)
datos$Prenda <- gsub("LAVADO Y PLANCHADO, DE CHAMARRA", "CHAMARRA", datos$Prenda)

#VESTIDO: Se crean 2 categorías: vestidoy vestido de noche. 
# La primera incluye vestido, vestido sencillo, vestido corto, y vestido corto sencillo
# La segunda incluye vestido de noche y vestido largo

unique(grep("VESTIDO",datos$Prenda, value = T)) 

datos$Prenda <- gsub("LAVADO Y PLANCHADO, DE VESTIDO SENCILLO", "VESTIDO", datos$Prenda)
datos$Prenda <- gsub("LAVADO Y PLANCHADO, DE VESTIDO CORTO, SENCILLO", "VESTIDO", datos$Prenda)
datos$Prenda <- gsub("LAVADO Y PLANCHADO, DE VESTIDO CORTO", "VESTIDO", datos$Prenda)
datos$Prenda <- gsub("LAVADO Y PLANCHADO, DE VESTIDO", "VESTIDO", datos$Prenda)
datos$Prenda <- gsub("VESTIDO LARGO", "VESTIDO DE NOCHE", datos$Prenda)

# EDREDÓN Y COLCHA

unique(grep("EDREDON",datos$Prenda, value = T)) 

datos$Prenda <- gsub("LAVADO Y PLANCHADO, DE EDREDON KING SIZE", "EDREDONES/COLCHAS", datos$Prenda)
datos$Prenda <- gsub("LAVADO Y PLANCHADO, DE EDREDON INDIVIDUAL", "EDREDONES/COLCHAS", datos$Prenda)
datos$Prenda <- gsub("LAVADO Y SECADO, DE EDREDON KING SIZE", "EDREDONES/COLCHAS", datos$Prenda)
datos$Prenda <- gsub("LAVADO Y PLANCHADO, DE EDREDON MATRIMONIAL", "EDREDONES/COLCHAS", datos$Prenda)
datos$Prenda <- gsub("LAVADO Y PLANCHADO, DE EDREDON QUEEN SIZE", "EDREDONES/COLCHAS", datos$Prenda)
datos$Prenda <- gsub("LAVADO Y PLANCHADO, DE COLCHA MATRIMONIAL", "EDREDONES/COLCHAS", datos$Prenda)

# PANTALÓN

unique(grep("PANTALON",datos$Prenda, value = T))  

datos$Prenda <- gsub("LAVADO Y PLANCHADO, DE PANTALON, PAGO ANTICIPADO", "PANTALON", datos$Prenda)
datos$Prenda <- gsub("LAVADO Y PLANCHADO, DE PANTALON, P/CABALLERO", "PANTALON", datos$Prenda)
datos$Prenda <- gsub("LAVADO Y PLANCHADO, DE PANTALON", "PANTALON", datos$Prenda)

# Quitar el lavado y planchado del nombre para sólo dejar el nombre de la prenda

datos$Prenda <- gsub("LAVADO Y PLANCHADO, DE ", "", datos$Prenda)

#SACO

datos$Prenda <- gsub("LAVADO Y PLANCHADO, SACO, DE CABALLERO", "SACO", datos$Prenda)
datos$Prenda <- gsub("LAVADO Y PLANCHADO,  DE SACO", "SACO", datos$Prenda)

#CAMISA
datos$Prenda <- gsub("CAMISA, MANGA CORTA", "CAMISA", datos$Prenda)

# Nos quedamos con las 11 categorías más importantes:
# Hacemos un subconjunto con los datos que contienen estas prendas
prendas <- c("ABRIGO", "BLUSA", "CAMISA", "CHAMARRA", "EDREDONES/COLCHAS", "FALDA", "PANTALON", "SACO", "TRAJE", "VESTIDO", "VESTIDO DE NOCHE")
datos_s <- subset(datos[datos$Prenda %in% prendas, ])

# Podemos  hacer resúmenes del precio promedio de las prendas de acuerdo a la ciudad 
# y al tipo de prenda a que se refieren. 

datos_m <- melt(datos_s, id=c("Clave","Ciudad", "Fecha","Prenda"), measure.vars=c("Precio"))
m_data <- dcast(datos_m, Prenda + Clave ~variable, mean)
m_data2 <- dcast(datos_m, Prenda ~variable, mean)
m_data3 <- dcast(datos_m, Prenda + Fecha ~variable, mean)

m_data$Clave = as.factor(m_data$Clave)
ggplot(m_data, aes(x=Precio, fill = Clave)) + 
    facet_wrap(~Prenda, ncol = 2)+ coord_fixed(ratio = 5) + ylim(0,8) +
    geom_dotplot(binwidth = 5, stackgroups = T, method = "histodot", binpositions = "dotdensity") +
    ggtitle("Precio promedio de prendas") + 
    ylab("Frecuencia") +
    guides(fill=guide_legend(ncol=4))

