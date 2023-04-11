library(readxl)
library(tidyverse)
library(magrittr)
library(writexl)
library(dplyr)
library(haven)
library(lubridate)
library(glue)
library(janitor)

options(scipen = 9999)

#Turistas por origen

turistas_dest_total <- read.csv("entradas/evyth/tur_int_turistas_residentes_destino_serie.csv") %>% 
  mutate(anio=year(indice_tiempo))

graf_1 <- turistas_dest_total %>%
  group_by(indice_tiempo) %>%
  summarise(turistas=sum(turistas)) %>% 
  ungroup() %>% 
  mutate(indice_tiempo=as.Date(indice_tiempo)) %>% 
  mutate(turistas=turistas/1000)


#Turistas por trim:

ggplot(data = graf_1,aes(x=indice_tiempo,y=turistas))+
  geom_line(linewidth=2,color="red", alpha = 0.4)+
  geom_point(size=3,color="red", alpha = 0.9)+
  geom_hline(yintercept = 0, color = "grey", alpha =0.7, size = 0.5)+
  labs(title = "Turistas residentes según trimestre",
       x = "",
       y = "Turistas (en miles)",
       colour="",
       caption = "Fuente: EVyTH") +
  theme(text = element_text(size = 12),
        plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = "grey"),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.major.y =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth = 1),
        panel.grid.major.x =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth  = 1),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90))


# Turistas por destino

graf_2 <- turistas_dest_total %>%
  #filter(anio==2019) %>%
  group_by(anio,region_destino) %>% 
  summarise(turistas=sum(turistas)) %>% 
  mutate(dist=prop.table(turistas)) %>% 
  mutate(dist=round(dist*100,1)) %>% 
  ungroup() %>% 
  select(anio,region_destino,dist) %>% 
  mutate(anio=as.character(anio))

# Distribución de turistas por destino según año

ggplot(data = graf_2,aes(x=anio,y=dist,fill=region_destino,label=glue("{dist} %")))+
  geom_col()+
  geom_text(position = position_stack(vjust = .5))+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  labs(title = "Distribución de los turistas segun región de destino",
       subtitle = "GAU, 2012-2022.",
       x = "",
       y = "",
       colour="",
       caption = "Fuente: EVyTH") +
  theme(text = element_text(size = 12),
        plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = "grey29"),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        legend.position = "top",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

rm(graf_1,graf_2,turistas_dest_total)

#Turistas por origen:

turistas_orig_total <- read.csv("entradas/evyth/tur_int_turistas_residentes_origen_serie.csv") %>% 
  mutate(anio=year(indice_tiempo))


graf_3 <- turistas_orig_total %>%
  #filter(anio==2019) %>%
  group_by(anio,region_origen) %>% 
  summarise(turistas=sum(turistas)) %>% 
  mutate(dist=prop.table(turistas)) %>% 
  mutate(dist=round(dist*100,1)) %>% 
  ungroup() %>% 
  select(anio,region_origen,dist) %>% 
  mutate(anio=as.character(anio))


# Distribución de turistas por destino según año

ggplot(data = graf_3,aes(x=anio,y=dist,fill=region_origen,label=glue("{dist} %")))+
  geom_col()+
  geom_text(position = position_stack(vjust = .5))+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  labs(title = "Distribución de los turistas segun región de origen",
       subtitle = "GAU, 2012-2022.",
       x = "",
       y = "",
       colour="",
       caption = "Fuente: EVyTH") +
  theme(text = element_text(size = 12),
        plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = "grey29"),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        legend.position = "top",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

rm(graf_3)

#Edad

turistas_edad_total <- read.csv("entradas/evyth/tur_int_turistas_residentes_edad_serie.csv") %>% 
  mutate(anio=year(indice_tiempo))

graf_4 <- turistas_edad_total %>%
  #filter(anio==2019) %>%
  group_by(anio,edad) %>% 
  summarise(turistas=sum(turistas)) %>% 
  mutate(dist=prop.table(turistas)) %>% 
  mutate(dist=round(dist*100,1)) %>% 
  ungroup() %>% 
  select(anio,edad,dist) %>% 
  mutate(anio=as.character(anio))

#Gráfico

ggplot(data = graf_4 %>% 
         mutate(edad=factor(edad,levels=c("Menos de 14 años",
                                            "14 a 29 años",
                                            "30 a 44 años",
                                            "45 a 59 años",
                                            "60 años o más",
                                            "Ns./ Nr."))),aes(x=anio,y=dist,fill=edad,label=glue("{dist} %")))+
  geom_col()+
  geom_text(position = position_stack(vjust = .5))+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  labs(title = "Distribución de los turistas segun edad.",
       subtitle = "GAU, 2012-2022.",
       x = "",
       y = "",
       colour="",
       caption = "Fuente: EVyTH") +
  theme(text = element_text(size = 12),
        plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = "grey29"),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        legend.position = "top",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

rm(graf_4,turistas_edad_total)

#Sexo

turistas_sexo <- read.csv("entradas/evyth/tur_int_turistas_residentes_sexo_serie.csv") %>% 
  mutate(anio=year(indice_tiempo))

graf_5 <- turistas_sexo %>%
  #filter(anio==2019) %>%
  group_by(anio,sexo) %>% 
  summarise(turistas=sum(turistas)) %>% 
  mutate(dist=prop.table(turistas)) %>% 
  mutate(dist=round(dist*100,1)) %>% 
  ungroup() %>% 
  select(anio,sexo,dist) %>% 
  mutate(anio=as.character(anio))

#Gráfico

ggplot(data = graf_5 ,aes(x=anio,y=dist,fill=sexo,label=glue("{dist} %")))+
  geom_col()+
  geom_text(position = position_stack(vjust = .5))+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  labs(title = "Distribución de los turistas segun sexo.",
       subtitle = "GAU, 2012-2022.",
       x = "",
       y = "",
       colour="",
       caption = "Fuente: EVyTH") +
  theme(text = element_text(size = 12),
        plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = "grey29"),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        legend.position = "top",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

rm(graf_5,turistas_sexo)

#Quintil

turistas_quintil <- read.csv("entradas/evyth/tur_int_turistas_residentes_quintil_serie.csv") %>% 
  mutate(anio=year(indice_tiempo))

graf_6 <- turistas_quintil %>%
  #filter(anio==2019) %>%
  group_by(anio,quintil) %>% 
  summarise(turistas=sum(turistas)) %>% 
  mutate(dist=prop.table(turistas)) %>% 
  mutate(dist=round(dist*100,1)) %>% 
  ungroup() %>% 
  select(anio,quintil,dist) %>% 
  mutate(anio=as.character(anio))

#Gráfico

ggplot(data = graf_6 %>% mutate(quintil=factor(quintil,levels = c("Quintiles 1 y 2",
                                                                  "Quintiles 3 y 4",
                                                                  "Quintil 5"))) ,aes(x=anio,y=dist,fill=quintil,label=glue("{dist} %")))+
  geom_col()+
  geom_text(position = position_stack(vjust = .5))+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  labs(title = "Distribución de los turistas segun quintil de ingresos.",
       subtitle = "GAU, 2012-2022.",
       x = "",
       y = "",
       colour="",
       caption = "Fuente: EVyTH") +
  theme(text = element_text(size = 12),
        plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = "grey29"),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        legend.position = "top",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

rm(graf_6,turistas_quintil)


#Motivo

turistas_motivo <- read.csv("entradas/evyth/tur_int_turistas_residentes_motivo_serie.csv") %>% 
  mutate(anio=year(indice_tiempo))

graf_7 <- turistas_motivo %>%
  #filter(anio==2019) %>%
  group_by(anio,motivo) %>% 
  summarise(turistas=sum(turistas)) %>% 
  mutate(dist=prop.table(turistas)) %>% 
  mutate(dist=round(dist*100,1)) %>% 
  ungroup() %>% 
  select(anio,motivo,dist) %>% 
  mutate(anio=as.character(anio))

#Gráfico

ggplot(data = graf_7,aes(x=anio,y=dist,fill=motivo,label=glue("{dist} %")))+
  geom_col()+
  geom_text(position = position_stack(vjust = .5))+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  labs(title = "Distribución de los turistas segun quintil de ingresos.",
       subtitle = "GAU, 2012-2022.",
       x = "",
       y = "",
       colour="",
       caption = "Fuente: EVyTH") +
  theme(text = element_text(size = 12),
        plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = "grey29"),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        legend.position = "top",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

rm(graf_7,turistas_motivo)

#Alojamiento

turistas_alojamiento <- read.csv("entradas/evyth/tur_int_turistas_residentes_tipo_alojamiento_serie.csv") %>% 
  mutate(anio=year(indice_tiempo))

graf_8 <- turistas_alojamiento %>%
  #filter(anio==2019) %>%
  group_by(anio,tipo_alojamiento) %>% 
  summarise(turistas=sum(turistas)) %>% 
  mutate(dist=prop.table(turistas)) %>% 
  mutate(dist=round(dist*100,1)) %>% 
  ungroup() %>% 
  select(anio,tipo_alojamiento,dist) %>% 
  mutate(anio=as.character(anio))

#Gráfico (cambiar colores)

ggplot(data = graf_8,aes(x=anio,y=dist,fill=tipo_alojamiento,label=glue("{dist} %")))+
  geom_col()+
  geom_text(position = position_stack(vjust = .5))+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  labs(title = "Distribución de los turistas segun tipo de alojamiento.",
       subtitle = "GAU, 2012-2022.",
       x = "",
       y = "",
       colour="",
       caption = "Fuente: EVyTH") +
  theme(text = element_text(size = 12),
        plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = "grey29"),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        legend.position = "top",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

rm(graf_8,turistas_alojamiento)


#Transporte

turistas_transporte <- read.csv("entradas/evyth/tur_int_turistas_residentes_tipo_transporte_serie.csv") %>% 
  mutate(anio=year(indice_tiempo))

graf_9 <- turistas_transporte %>%
  #filter(anio==2019) %>%
  group_by(anio,tipo_transporte) %>% 
  summarise(turistas=sum(turistas)) %>% 
  mutate(dist=prop.table(turistas)) %>% 
  mutate(dist=round(dist*100,1)) %>% 
  ungroup() %>% 
  select(anio,tipo_transporte,dist) %>% 
  mutate(anio=as.character(anio))

#Gráfico

ggplot(data = graf_9,aes(x=anio,y=dist,fill=tipo_transporte,label=glue("{dist} %")))+
  geom_col()+
  geom_text(position = position_stack(vjust = .5))+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  labs(title = "Distribución de los turistas segun tipo de transporte utilizado.",
       subtitle = "GAU, 2012-2022.",
       x = "",
       y = "",
       colour="",
       caption = "Fuente: EVyTH") +
  theme(text = element_text(size = 12),
        plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = "grey29"),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        legend.position = "top",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

rm(graf_9,turistas_transporte)

###

#Estadía promedio

em <- read.csv("entradas/evyth/tur_int_turistas_residentes_estadia_media_anual_destino_serie.csv") %>% 
  mutate(anio=as.character(indice_tiempo)) %>% 
  mutate(region_destino=case_when(region_destino=="PBA - Partidos del GBA"~"Part.GBA",
                                  region_destino=="Patagonia"~"Pgonia",
                                  region_destino=="Córdoba"~"Cord.",
                                  TRUE~region_destino))

#Gráfico:

ggplot(data = em,aes(x=anio, y=estadia_promedio_anual,
                     color=region_destino,label=region_destino))+ 
  geom_text()+
  labs(title = "Estadía promedio según región de destino.",
       subtitle = "GAU, 2012-2022.",
       x = "",
       y = "",
       colour="",
       caption = "Fuente: EVyTH") +
  theme(text = element_text(size = 12),
        plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = "grey29"),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        legend.position = "")

###

#GPD

gpd <- read.csv("entradas/evyth/tur_int_turistas_residentes_gasto_promedio_anual_destino_serie.csv") %>% 
  mutate(anio=as.character(indice_tiempo)) %>%
  filter(anio %in%c("2017","2018","2019")) %>% 
  mutate(region_destino=case_when(region_destino=="PBA - Partidos del GBA"~"Part.GBA",
                                  region_destino=="Patagonia"~"Pgonia",
                                  region_destino=="Córdoba"~"Cord.",
                                  TRUE~region_destino))

#Gráfico

ggplot(data = gpd,aes(x=anio, y=gasto_promedio_anual,
                     color=region_destino,label=region_destino))+ 
  geom_text()+
  labs(title = "Estadía promedio según región de destino.",
       subtitle = "GAU, 2012-2022.",
       x = "",
       y = "",
       colour="",
       caption = "Fuente: EVyTH") +
  theme(text = element_text(size = 12),
        plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = "grey29"),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        legend.position = "")

rm(gpd)

