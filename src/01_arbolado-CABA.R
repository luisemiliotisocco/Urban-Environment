# Cargamos las librerías que estaremos usando en este análisis
library(tidyverse)
library(sf)
library(ggplot2)
library(leaflet)
proj <- 4326
#proj <- "+proj=laea +lat_0=-40 +lon_0=-60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

#sf::sf_use_s2(FALSE)

arbolado_calle <- read.csv("data/BAdata/arbolado-publico-lineal-2017-2018/arbolado-publico-lineal-2017-2018.csv", 
                          stringsAsFactors = TRUE, encoding = "UTF-8") %>% 
    drop_na(long, lat)

arbolado_parques <- read.csv("data/BAdata/arbolado-en-espacios-verdes/arbolado-en-espacios-verdes.csv", 
                           stringsAsFactors = TRUE, encoding = "UTF-8")

manzanas <- st_read("data/BAdata/manzanas/manzanas.geojson") %>% 
    st_transform(proj) %>% 
    select(SM)

EV <- st_read("data/BAdata/espacio-verde-publico/espacio-verde-publico.shp") %>% 
    st_transform(proj)

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

arbolado <- st_as_sf(arbolado, coords = c("long", "lat"), crs=proj)

#ggplot()+
#    geom_sf(data=manzanas, fill="grey80", color=NA) +
#    geom_sf(data=EV, fill="grey60", color=NA)+
#    theme_void()

manzanas_buffer <- manzanas %>% 
    st_buffer(1000) 

arbolado2 <- arbolado  %>% 
    sample_n(10) %>% 
    mutate(lat = unlist(map(geometry,2)),
           long = unlist(map(geometry,1)))

manzanas_buffer_arbolado <- st_join (arbolado2, manzanas_buffer)

ggplot()+
    geom_sf(data=manzanas_buffer)+
    geom_sf(data=arbolado2)

manzanas_buffer_df <- manzanas_buffer_arbolado %>% 
    as.data.frame() %>% 
    group_by(SM) %>% 
    summarise(cantidad_arboles=n())
