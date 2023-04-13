etie <- readRDS("entradas/eti/eti_nr_2009_2023.rds") %>% 
  filter(vis==2 & wpf>0) 




tabla <- etie %>%
  filter(paso_final=="Ezeiza y Aeroparque" &p3_3==2019) %>%
  group_by(p3_3,orig_eya) %>%
  summarise(turistas=sum(viajeros*wpf,na.rm = TRUE),
            pernoctes=sum(totaln*wpf,na.rm = TRUE),
            gasto=sum(gastoest2*wpf,na.rm = TRUE)) %>% 
  filter(turistas>0) %>%
  ungroup() %>% 
  select(-p3_3) %>% 
  adorn_totals() %>% 
  mutate(em=pernoctes/turistas,
         gpd=gasto/pernoctes,
         gpt=gasto/turistas) %>% 
  mutate(orig_eya=factor(orig_eya,levels = c("Total","Bolivia",
                                             "Brasil","Chile",
                                             "Paraguay","EE.UU y Canadá",
                                             "Uruguay","Resto América",
                                             "Europa","Resto del Mundo"))) %>% 
  arrange(orig_eya)

  

