#Demonstrations

library(tidyverse)
library(leaflet)
library(scales)

rkurs_data <- read.csv("~/Desktop/Uni/MASTER/WS 2018:2019/Rkundung/Datensatz/rkurs_data02.csv")


rkurs_data <- rkurs_data %>%
  mutate(datetime = paste(date, time, sep=' '))%>%
  select(region, date, time, datetime, everything()) #reorder columns
#erstellt einer neue Spalte mit datetime
rkurs_data$datetime <- as.POSIXct(x=rkurs_data$datetime, format= '%d.%m.%y %H:%M') #veraendert unser datenformat in datetime 

#--------------------------------------------------------------------------------------#

# UEBUNG: Erstelle eine plot der die Siliacatconncentration fuer die einzelnen Stationen ueber die Zeit darstellt

rkurs_data$date <- as.POSIXct(x=rkurs_data$date, format='%d.%m.%y')

plot_Si <- rkurs_data %>%
  group_by(region,date)%>%
  ggplot(., aes(x=as.Date(date), y=silicate, group=region, colour=region))+
  geom_point()+
  geom_line(linetype='dashed')+
  scale_y_continuous(breaks=c(seq(0, 20, 2)))+
  scale_x_date(labels = date_format("%d.%m"))+
  labs(x='Date', y='Silicate (µg/L)', title='Silicate at different stations over time')+
  theme_bw()+
  theme(legend.position='bottom')

plot_Si

#--------------------------------------------------------------------------------------#
#Stelle die Nutrient und T-Daten graphisch dar ueber die Zeit an den einzelnen Stationen
#using facet_wrap

#two options to melt our data using gather or summarise

summarised_data <- rkurs_data %>%
  select(-latitude, -longitude, -HAB, -spec_biomass, -genus, - toxicity)%>%
  group_by(region, date, temperature, nitrate, phosphate, silicate) %>%
  summarise() %>%
  gather(key = nutrient, value = concentration, -region, -date) 

distinct_data <- rkurs_data %>%
  group_by(region,date)%>%
  distinct(temperature, nitrate, phosphate, silicate) %>% #schmeisst duplicate raus
  gather(key = nutrient, value = concentration, -region, -date)  #mit key uebergebe ich eine neue variable die die nutrients enthaelt
                                                                 #region und date bleiben erhalten, der Rest wird der neuen Variable zugefuehrt
                                                                

f <- distinct_data%>%
  ggplot(., aes(x = date, y = concentration, group = region, colour = region)) +
  geom_point(alpha = 0.9) +
  geom_line(alpha = 0.3,size = 1) +
  facet_wrap(~nutrient, scale = 'free_y') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(linetype = 20, colour = grey(0.8)),
        legend.position = 'bottom',
        axis.line = element_line(colour = grey(0.5))) 
f
ggsave(p, filename='silicate.png',width = 9, height = 4.5)


#--------------------------------------------------------------------------------------#
#Wie sieht es mit Gesamtbiomasse an jeder Station ueber Zeit aus?
rkurs_data %>%
  group_by(region, date) %>%
  summarise(tot_biomass = sum(spec_biomass)) %>%
  ggplot(., aes(x = date, y = tot_biomass, group = region, colour = region)) +
  geom_point(alpha = 0.9) +
  geom_line(alpha = 0.3,size = 1) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(linetype = 20, colour = grey(0.8)),
        axis.line = element_line(colour = grey(0.5))) 

    
#Durchschnittlich?
rkurs_data %>%
  group_by(region, date) %>%
  summarise(tot_biomass = sum(spec_biomass)) %>%
  group_by(region) %>%
  summarise(mean_totb = mean(tot_biomass)) %>%
  ggplot(., aes(x = region, y = mean_totb, fill = region)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  theme(legend.position = 'none')


#Wie viel tragen HAB-Arten an jeder Station zur Biomasse bei?
rkurs_data %>%
  group_by(region,date) %>%
  mutate(tot_biomass = sum(spec_biomass)) %>%
  filter(HAB == 1) %>%
  mutate(hab_biomass = sum(spec_biomass),
         rel_hab = hab_biomass / tot_biomass) %>%
  distinct(region, date, rel_hab) %>%
  ggplot(.,aes(x = date, y = rel_hab, group = region, colour = region)) +
  geom_point(alpha = 0.9) +
  geom_line(alpha = 0.3,size = 1) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(linetype = 20, colour = grey(0.8)),
        axis.line = element_line(colour = grey(0.5))) 

#Wie viele Arten finden wir pro Station?
rkurs_data %>%
  group_by(region, date) %>%
  summarise(richness = n())  %>%
  distinct(richness) %>%
  ggplot(., aes(x = region, y = richness, fill = region)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  theme(legend.position = 'none')

#Wie haengen P und Richness zusammen?
rkurs_data %>%
  group_by(region, date) %>%
  summarise(richness = n(),
            mean_p = mean(phosphate))  %>%
  distinct(richness, mean_p) %>%
  ggplot(., aes(x = mean_p, y = richness)) +
    geom_point(aes(colour = region),alpha = 0.9) +
    geom_smooth(method = 'loess', linetype = 20, alpha = 0.5, se = F) +
    theme_classic() +
    theme(panel.grid.major.y = element_line(linetype = 20, colour = grey(0.8)),
          axis.line = element_line(colour = grey(0.5))) 

#--------------------------------------------------------------------------------------#
#schneller plot fuer 1 Umweltparameter
p <- rkurs_data %>%
  group_by(region,date) %>%
  distinct(temperature, nitrate, phosphate, silicate) %>%
  ggplot(., aes(x = date, y = silicate, group = region, colour = region)) +
  geom_point(alpha = 0.9) +
  geom_line(alpha = 0.3,size = 1) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(linetype = 20, colour = grey(0.8)),
        axis.line = element_line(colour = grey(0.5))) 
p
ggsave(p, filename='silicate.png',width = 9, height = 4.5)


#--------------------------------------------------------------------------------------#
#create quick map
rkurs_map <- rkurs_data %>%
  group_by(region) %>%
  distinct(latitude, longitude) 

leaflet() %>%
  setView(lng = mean(rkurs_map$longitude), lat = mean(rkurs_map$latitude), zoom = 4) %>%
  addProviderTiles(providers$OpenStreetMap) %>% #OpenStreetMap, Esri.World.Terrain, Esri.OceanBasemap
  addCircleMarkers(lng = rkurs_map$longitude, lat = rkurs_map$latitude, label = as.character(rkurs_map$region),
                   labelOptions = labelOptions(noHide = T, direction = 'top',
                                               textsize = '12px', opacity = 0.9,
                                               textOnly = F), radius = 6 )

