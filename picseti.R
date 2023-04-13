#.left[EOH: análisis de la información disponible]

#```{r fig.height = 8, fig.width=12, fig.align='center'}


eti <- read.csv("entradas/eti/turistas_no_residentes_por_motivo_de_viaje_segun_paso_trimestral.csv") %>% 
  mutate(anio = str_sub(indice_tiempo,1,4),
         mes = str_sub(indice_tiempo,6,7),
         anio_mes =glue("{anio}-{mes}-01"),
         anio_mes=as.Date(anio_mes)) %>% 
  filter(anio %in% c("2019","2018","2017") &	paso=="Ezeiza y Aeroparque")

eti <- eti %>% 
  group_by(anio,motivo) %>% 
  summarise(turistas=sum(turistas_no_residentes,na.rm=TRUE)) %>% 
  mutate(dist=prop.table(turistas)) %>% 
  mutate(dist=(round(dist,3)*100))


#Gráfico

ggplot(data = eti %>% mutate(motivo=factor(motivo,levels = c("Visita flia/amigos","Vacaciones/ocio/recreación",
                                                             "Negocios/congreso/conferencia","Otros"))),
       aes(x=anio,y=dist,fill=motivo,label=glue("{dist} %")))+
  geom_col()+
  geom_text(position = position_stack(vjust = .5))+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  labs(title = "Distribución de los turistas no residentes\n segun motivo",
       subtitle = "Ezeiza y Aeroparque, 2017-2019.",
       x = "",
       y = "",
       colour="",
       caption = "Fuente: EVyTH") +
  theme(text = element_text(size = 12),
        plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = "grey29"),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        legend.position = "right",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


ggsave("pics/eti1.png")

rm(eti)


##########################################

eti <- read.csv("entradas/eti/turistas_no_residentes_por_tipo_de_alojamiento_segun_paso_trimestral.csv") %>% 
  mutate(anio = str_sub(indice_tiempo,1,4),
         mes = str_sub(indice_tiempo,6,7),
         anio_mes =glue("{anio}-{mes}-01"),
         anio_mes=as.Date(anio_mes)) %>% 
  filter(anio %in% c("2019","2018","2017") &	paso=="Ezeiza y Aeroparque")

eti <- eti %>% 
  group_by(anio,alojamiento) %>% 
  summarise(turistas=sum(turistas_no_residentes,na.rm=TRUE)) %>% 
  mutate(dist=prop.table(turistas)) %>% 
  mutate(dist=(round(dist,3)*100))


#Gráfico

ggplot(data = eti %>% mutate(alojamiento=factor(alojamiento,levels = c("Casa flia./amigos","Hotel 1,2, y 3 estrellas",
                                                             "Hotel 4 y 5 estrellas","Otros"))),
       aes(x=anio,y=dist,fill=alojamiento,label=glue("{dist} %")))+
  geom_col()+
  geom_text(position = position_stack(vjust = .5))+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  labs(title = "Distribución de los turistas no residentes\n segun alojamiento",
       subtitle = "Ezeiza y Aeroparque, 2017-2019.",
       x = "",
       y = "",
       colour="",
       caption = "Fuente: EVyTH") +
  theme(text = element_text(size = 12),
        plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = "grey29"),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        legend.position = "right",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


ggsave("pics/eti2.png")

rm(eti)