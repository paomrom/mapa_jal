#Instalación de librerias
pacman::p_load(tidyverse, dplyr, readr, sf, gganimate, gifski, stringr, rebus, scales, gridExtra)

#Esto es únicamente para SO de Mac, y es para añadir la tipografía Nutmeg
#https://rpubs.com/Juve_Campos/fuentesEnRStudio
fuentes_Sys <- list.files("~/Library/Fonts/") %>% 
  str_remove_all(pattern = "." %R% capture(one_or_more(WRD)) %R% END)

#Selecciona ruta de trabajo
setwd("~/Google Drive")

#Shapefile delJalisco con sus 125 municipios
shp <- st_read(dsn = "R-LadiesGDL/14m.shp", layer = "14m")

#Pintar shapefile
st_geometry(shp)
ggplot(data = shp)+
  geom_sf()

#Archivo csv con el registro anual de violación por municipio
viola <- read_csv("R-LadiesGDL/violacion.csv")

#Unir shapefile con csv
sf_viola <- merge(shp, viola, by.x = "NOMGEO", by.y = "Municipio", all.x = TRUE)

#Transformación a datos formato ts
violacion<-gather(data = sf_viola, key = "Anio", value = "Nivel", 13:18)
View(violacion)

violacion <- violacion %>%
  mutate(Nivel=factor(Nivel,
                      levels=c("Alta", "Alta - media", "Media","Baja", "Nula")))

levels(violacion$Nivel)

##Determinar y nombrar los graficos que van ser la secuencia de tiempo y la interacción
ggplot(data = violacion)+
  geom_sf(aes(fill = Nivel))+
  theme_void()

a<- ggplot(violacion %>%  filter(Anio == "2015")) +
  geom_sf(aes(fill = Nivel)) +
  labs(subtitle = "Año: 2015")+
  theme_void()

b<- ggplot(violacion %>%  filter(Anio == "2016")) +
  geom_sf(aes(fill = Nivel)) +
  labs(subtitle = "Año: 2016")+
  theme_void()

c <- ggplot(violacion %>%  filter(Anio == "2017")) +
  geom_sf(aes(fill = Nivel)) +
  labs(subtitle = "Año: 2017")+
  theme_void()

d <- ggplot(violacion %>%  filter(Anio == "2018")) +
  geom_sf(aes(fill = Nivel)) +
  labs(subtitle = "Año: 2018")+
  theme_void()

e <- ggplot(violacion %>%  filter(Anio == "2019")) +
  geom_sf(aes(fill = Nivel)) +
  labs(subtitle = "Año: 2019")+
  theme_void()


f <- ggplot(violacion %>%  filter(Anio == "2020")) +
  geom_sf(aes(fill = Nivel)) +
  labs(subtitle = "Año: 2020")+
  theme_void()

###[a,b,c,d,e,f] integran la secuencia del mapa en uno
grid.arrange(a,b,c,d,e,f)

#Mapa dinamico
g<-ggplot(violacion)+
  geom_sf(aes(fill=Nivel))+
  transition_manual(Anio)+
  labs(subtitle = "", size=40,
       title="Prevalencia del delito de Violación en el año {current_frame}",
       caption = "Elaborado a partir de los datos abiertos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP),
  con corte a octubre 2020")+
  scale_fill_manual(values = c("#733299", "#ba4fe0", "#c079d9", "#a284b5", "#d7c8e0"))+
  theme_void()+ 
  theme(
    plot.title=element_text(family="Nutmeg-Black", size=24, hjust=0.5, face="bold", colour="#733299", vjust=1),
    plot.caption = element_text(family="Nutmeg-Light", size=10, hjust = 0),
    legend.title = element_text(family="Nutmeg-Light", size=14),
    legend.text = element_text(family="Nutmeg-Light", size=14),
    plot.margin = margin(1, 1, 1, 1, "cm"))

animate(g, 450, fps = 45, duration = 10, detail=15 ,width = 800, height = 600)
