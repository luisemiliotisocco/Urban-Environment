# Cargamos las librerías que estaremos usando en este análisis
library(tidyverse)
library(sf)
library(ggplot2)
library(leaflet)

arbolado_calle <- read.csv("data/BAdata/arbolado-publico-lineal-2017-2018/arbolado-publico-lineal-2017-2018.csv", 
                          stringsAsFactors = TRUE, encoding = "UTF-8") %>% 
    drop_na(long, lat)

arbolado_parques <- read.csv("data/BAdata/arbolado-en-espacios-verdes/arbolado-en-espacios-verdes.csv", 
                           stringsAsFactors = TRUE, encoding = "UTF-8")

# Consolidar ambos shapes en una unica base    

names(arbolado_calle)
names(arbolado_parques)

arbolado_calle <- arbolado_calle %>% 
    select(nro_registro, nombre_cientifico, altura_arbol, long, lat) %>% 
    transmute(id=as.integer(nro_registro), 
              especie=nombre_cientifico, 
              altura=altura_arbol, 
              long, lat)

arbolado_parques <- arbolado_parques %>% 
    select(id_arbol, nombre_cie, altura_tot, long, lat) %>% 
    transmute(id=as.integer(id_arbol),
              especie=nombre_cie, 
              altura=altura_tot,
              long, lat)

arbolado <- full_join(arbolado_calle, arbolado_parques)

unique(arbolado$especie)

arbolado <- st_as_sf(arbolado, coords = c("long", "lat"), crs=4623)

ggplot()+
    geom_sf(data=arbolado)








