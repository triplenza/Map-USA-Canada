#Bubble Map for Sample 
#Author: Nik
#Date: 9/9/2022


#install library
library(tidyverse) # loads in ggplot and a few other packages that are very useful for wrangling code/making figures
library(sf) # "simple format", needed to work with maps/geometry objects
library(here) # again, quality of life package
library(readxl) # to read in excel based documents
library(cowplot) # to bring multiple figures into a single, multi-pane, figure
library(ggplot2)
library(dplyr)
library(maps)
library(ggrepel)
library(ggpubr)
library(factoextra)
library(RColorBrewer)
library(scatterpie)
library(conflicted)

setwd("D:/MSU/Master/Research/Map for USA/May-24-2022-main")

#load USA-Ontario map
usc <- here::here(
  "USA map",
  "bound_p.shp"
) %>%
  st_read() # download the shapefiles for US states from the 'USA map shapefile'

#filter only for usa & ontario only
(usa_ontario <- usc %>%
    dplyr::filter(!(NAME %in% c("Kalaallit Nunaat", "Nunavut", "Northwest Territories / Territoires du Nord-Ouest", "Russia / Rusia / Russie", "Alaska", "Jan Mayen", "Yukon Territory / Territoire du Yukon", "Iceland / Islandia / Islande", "Quebec / Qu�bec", "F�royar", "United Kingdom / Reino Unido / Royaume-Uni", "Newfoundland and Labrador / Terre-Neuve-et-Labrador","British Columbia / Colombie-Britannique", "Alberta", "Saskatchewan", "Manitoba", "Ireland", "New Brunswick / Nouveau-Brunswick", "Saint-Pierre et Miquelon", "Prince Edward Island / �le-du-Prince-�douard", "Nova Scotia / Nouvelle-�cosse", "New Brunswick / Nouveau-Brunswick", "Bermuda", "Baja California", "Chihuahua", "Sonora", "Nuevo Le�n", "Tamaulipas", "Baja California Sur", "Sinaloa", "Bahamas", "Durango", "Zacatecas", "San Luis Potos�", "Cuba", "Nayarit", "Jalisco", "Veracruz-Llave", "Aguascalientes", "Arrecife Alacr�n", "Hawaii", "Turks and Caicos Islands", "Guanajuato", "Quer�taro de Arteaga", "Yucat�n", "Quintana Roo", "Hidalgo", "Puebla", "Campeche", "Michoac�n de Ocampo", "M�xico", "Haiti / Hait� / Ha�ti", "Dominican Republic / Rep�blica Dominicana / R�publique Dominicaine", "Cayman Islands", "Tlaxcala", "Distrito Federal", "Colima", "Morelos", "British Virgin Islands", "Oaxaca", "Tabasco", "Jamaica /Jama�que", "Puerto Rico", "United States Virgin Islands", "Navassa Island", "Anguilla", "Belize / Belice", "Saint-Martin", "Sint Maarten (Nederlandse Antillen)", "Chiapas", "Saint-Barth�l�my", "Guatemala", "Antigua and Barbuda / Antiguay y Barbuda / Antigua-et-Barbud", "Islas Santanilla", "Saint Kitts and Nevis / Saint Kitts y Nevis / Saint-Kitts-et-Nevis", "Montserrat", "Guadeloupe","Honduras", "Nicaragua", "Dominica / Dominique", "Martinique", "El Salvador", "Saint Lucia / Santa Luc�a / Sainte-Lucie", "Colombia / Colombie", "Saint Vincent and the Grenadines / San Vicente y Les Granadinas / Saint-Vincent-et-Grenadines","Barbados", "Aruba", "Cura�ao (Nederlandse Antillen)", "	Bonaire (Nederlandse Antillen)", "Grenada / Granada / Grenade", "Venezuela", "Nicaragua", "Trinidad and Tobago / Trinidad y Tabago / Trinit�-et-Tobago", "Costa Rica", "Coahuila de Zaragoza", "Guerrero", "Bonaire (Nederlandse Antillen)", "Panama / Panam�", "Brazil / Brazil / Br�sil", "water/agua/d'eau"))))

sf_use_s2(FALSE)

# Get a data frame with longitude, latitude, and size of bubbles (a bubble = a city)
fields_sampled <- read_excel("03312022 First Trial Info gps.xlsx", 
                             col_types = c("text", "text",
                                           "numeric", "numeric", "text", "numeric"))

fields_sampled <- na.omit(fields_sampled) # removing missing GPS data points

fields_sf <- st_as_sf(fields_sampled, coords = c("Longitude", "Latitude")) #changing to sf format so the coordinates will be accurately placed

# K-Means Cluster Analysis, compute k-means with k = 4
ClusterInfo <- kmeans(cbind(fields_sampled$Latitude, fields_sampled$Longitude), 10, nstart = 25) 
# Add clusters obtained using the K-means algorithm
fields_sampled$cluster <- factor(ClusterInfo$cluster)

#plot latitude longitude
gpsmap <-
ggplot() +
  geom_sf(data = usa_ontario) +
  geom_point(data=fields_sampled, aes(x=Longitude, y=Latitude, color=Species), alpha=0.5) +
  theme_void() + 
  coord_sf() +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  theme(axis.text.x = element_text(size = 15, face = "bold", family = "serif"),
        axis.text.y = element_text(size = 15, face = "bold", family = "serif"),
        axis.title.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.y = element_text(size = 20, face = "bold", family = "serif", angle = 90),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        legend.text = element_text(size = 15, face = "bold", family = "serif"),
        legend.key = element_blank(),
        legend.title = element_text(size = 15, face="bold", family = "serif"),
        legend.position = "right",
        strip.text.x = element_text(size = 15, face = "bold", family = "serif"))

ggsave(gpsmap, filename = "MapUSA-Canada gps.png",  bg = "transparent", dpi=1000,units="in", height=9, width=11)

State_GPS <- read_excel("03312022 First Trial Info edited.xlsx",
                             col_types = c("numeric", "numeric",
                                           "text", "numeric",
                                           "numeric", "numeric", 
                                           "numeric", "numeric",
                                           "numeric", "numeric"))


data_lines <- data.frame(x = c(-77, -75.5),                # Create data for multiple segments
                         y = c(39.5, 39),
                         xend = c(-75, -73),
                         yend = c(37, 39))
data_lines                                       # Print data for multiple segments

#plot pie chart on map
piemap <-
  ggplot() +
  geom_sf(data = usa_ontario) +
  geom_scatterpie(data=State_GPS, aes(x=Longitude, y=Latitude, group=State), 
                  alpha=0.8,  
                  color=NA, pie_scale = 2, cols = c("C. cf. flagellaris", "C. kikuchii", "C. zeae-maydis", "Cercospora sp. M", "Cercospora sp. Q", "Cercospora sp. T") , legend_name = "Species") +
  geom_segment(data = data_lines,
               aes(x=x,xend=xend,y=y,yend=yend),
               size = 1) +
  geom_text(data=State_GPS, aes(x=Longitude, y=Latitude, group = Samples, label = Samples), 
            stat = "identity",position = position_dodge(width = 0.75), 
            hjust = 0.5, vjust = 2,
            check_overlap = TRUE, na.rm = FALSE, show.legend = NA, 
            inherit.aes = TRUE) +
  coord_sf() +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  theme(axis.text.x = element_text(size = 15, face = "bold", family = "serif"),
        axis.text.y = element_text(size = 15, face = "bold", family = "serif"),
        axis.title.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.y = element_text(size = 20, face = "bold", family = "serif", angle = 90),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        legend.text = element_text(size = 15, face = "bold", family = "serif"),
        legend.key = element_blank(),
        legend.title = element_text(size = 15, face="bold", family = "serif"),
        legend.position = "right",
        strip.text.x = element_text(size = 15, face = "bold", family = "serif")) +
  xlab("Longitude") + ylab("Latitude")

piemap  
  
ggsave(piemap, filename = "MapUSA-Canada pie.png",  bg = "transparent", dpi=1000,units="in", height=11, width=13)

