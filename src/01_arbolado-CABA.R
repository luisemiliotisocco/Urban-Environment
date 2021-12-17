# Cargamos las librerías que estaremos usando en este análisis
library(tidyverse)
library(sf)
library(ggplot2)
library(leaflet)
options(scipen = 999)
proj <- 4326
#proj <- "+proj=laea +lat_0=-40 +lon_0=-60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

#sf::sf_use_s2(FALSE)

arbolado_calle <- read.csv("data/BAdata/arbolado-publico-lineal-2017-2018/arbolado-publico-lineal-2017-2018.csv", 
                          stringsAsFactors = TRUE, encoding = "UTF-8") %>% 
    drop_na(long, lat)

arbolado_parques <- read.csv("data/BAdata/arbolado-en-espacios-verdes/arbolado-en-espacios-verdes.csv", 
                           stringsAsFactors = TRUE, encoding = "UTF-8")

EV <- st_read("data/BAdata/espacio-verde-publico/espacio-verde-publico.shp") %>% 
    st_transform(proj)

radios <- st_read("data/INDEC/cabaxrdatos.shp") %>% 
    st_transform(proj) %>% 
    dplyr::select(PAIS0210_I, TOT_POB) %>% 
    rename(ID=PAIS0210_I) %>% 
    mutate(AREA=as.numeric(st_area(.)*0.0001))

manzanas <- st_read("data/BAdata/manzanas/manzanas.geojson") %>% 
    st_transform(proj) %>% 
    select(SM)

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

arbolado2 <- arbolado  %>% 
    mutate(lat = unlist(map(geometry,2)),
           long = unlist(map(geometry,1)))

ggplot()+
    geom_sf(data=radios)+
    stat_density_2d(data=arbolado2, aes(x=long, y=lat, fill = stat(level)), 
                    color = "grey20", size=0.8, linetype="dashed", geom = "polygon", alpha=.5)+
    scale_fill_viridis_c(direction = -1, option = "magma")+
    theme_void()


#### 

manzanas_buffer_arbolado <- st_join (arbolado, radios)


cantidad_arbolado_parcela <- manzanas_buffer_arbolado %>% 
    as.data.frame() %>% 
    group_by(ID) %>%
    summarise(cant_arbolado=n())


radios <- radios %>% 
    left_join(cantidad_arbolado_parcela, by = "ID", na.rm=TRUE) %>% 
    mutate(arboles_por_poblacion=cant_arbolado/TOT_POB) %>% 
    mutate(arboles_por_area=cant_arbolado/AREA) 

ggplot()+
    geom_sf(data=radios %>% dplyr::filter(ID!=c(83, 2247)), aes(fill=arboles_por_poblacion))+
    scale_fill_viridis_c(option = "magma", direction = -1)+
    theme_void()


