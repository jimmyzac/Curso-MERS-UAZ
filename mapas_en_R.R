library(sf)
library(tidyverse)

zacatecas <-read_sf("C:\\Users\\Jaime Escobedo\\Downloads\\shapefile_zacatecas\\zac_municipio.shp")
zacatecas

class(zacatecas)

st_geometry(zacatecas)


ggplot(data = zacatecas) +
        geom_sf()


villanueva <- zacatecas %>% filter(NOMGEO=='Villanueva')

ggplot(data = villanueva) + 
        geom_sf()

villa_jerez <- zacatecas %>% filter(NOMGEO=='Villanueva'| NOMGEO=='Jerez')

ggplot(data = villa_jerez) + 
        geom_sf()

(zacatecas$CVE_MUN)

zacatecas <- zacatecas %>%
        mutate(region = case_when(
                CVE_MUN %in% c("027", "007", "041", "026", "051") ~ "Norte",
                CVE_MUN %in% c("029", "022", "014", "039", "006") ~ "Noroeste",
                CVE_MUN %in% c("042","040", "009", "021") ~ "Oeste",
                CVE_MUN %in% c("010", "013","005","032", "037", "056", "050", "017", "057", "012") ~ "Centro",
                CVE_MUN %in% c("049","031") ~ "Suroeste",
                CVE_MUN %in% c("055", "020", "046", "043") ~ "Centro sur",
                CVE_MUN %in% c("008","036","016","025","053","024","035","054","052","038") ~ "Sureste",
                CVE_MUN %in% c("015","044","030","003","048","018","045","019","034","002","004","058","001","047","023","011","028","033") ~ "Sur"
                )
        )

ggplot(data = zacatecas)+
        geom_sf(aes(fill = region))+
        labs(fill = "Región")


regiones_copladez <- zacatecas %>% 
        group_by(region) %>% 
        summarise(geometry = st_union(geometry)) %>%
        ungroup()

ggplot(regiones_copladez) +
        geom_sf(aes(fill = region))+
        labs(fill = "Región")

pob_zac <- read.csv("https://raw.githubusercontent.com/jimmyzac/Curso-MERS-UAZ/main/pob_zacatecas.csv", colClasses = c(clave = "character"))

head(pob_zac)

head(zacatecas)

pob_20_zac <- pob_zac %>% select(clave, población)

zacatecas

ggplot(zacatecas) +
        geom_sf(aes(fill= población))


cortes <- c(0,5000,10000, 25000, 50000, 100000, max(zacatecas$población)+1)


zacatecas<- zacatecas %>% mutate(grupo_pob=cut(población, breaks = cortes, right = F))

library(ggthemes)

ggplot(zacatecas) +
        geom_sf(aes(fill= grupo_pob), size=0.2, colour='grey7')+
        ## cambiamos el color
        scale_fill_brewer("Población total", palette = "Oranges", labels = c("0-5,000", "5,000-10,000","10,000-25,000", "25,000-50,000","50,000-100,000","más de 100,000"))+
        # agrega título
        labs(title = "Población por municipio, Zacatecas",
     subtitle = "Censo de 2020",
     caption = "Datos: www.inegi.org.mx") +
     # retirar el fondo gris
        theme(
        axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
             rect = element_blank()
             )      
cortes
