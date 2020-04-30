# Session 7
# Erstellen von plots und benutzen verschiedener theme operationen 
# Dominik Bahlburg, Charlotte Kunze


library(tidyverse) #laden des tidyverse package 

setwd("~/Desktop/Uni/MASTER/WS 2018:2019/Rkundung") # Working Directory setzen
#------------------------------------------------------------------------------------------------------------------------------------------#

## Plotte die Gesamtbiomasse fuer jede Station ueber die Zeit (facet) 
# zusaetzlich: die mittlere Biomasse pro Art mit SD in einen Graphen
# Tipp: bennene dazu die Variables gleich


rkurs_data <- read_csv("~/Desktop/Uni/MASTER/WS 2018:2019/Rkundung/Datensatz/rkurs_data02.csv") %>%
    mutate(datetime =  as.POSIXct(x = paste(date, time, sep=" "), format = '%d.%m.%y %H:%M:%S' )) #erstellen einer neuen Spalte mit datetime, die im POSIXct Format sein soll
                      # seperator soll ein Leerzeichen sein, mit paste werden die beiden Informationen aus date und time in eine Zeile zusammengefuegt.

#------------------------------------------------------------------------------------------------------------------------------------------#
# Erstellen von zwei Datensaetzen, die einmal die Gesamtbiomasse an jeder Station ueber die Zeit enthaelt 
# und einen df, der die mittlere Biomasse ueber die Zeit enthaelt 

sum_data <- rkurs_data %>%  #Gesamtbiomasse ueber die Zeit 
  group_by(region, datetime) %>% #gruppieren nach region und datetime
  summarise(sum_biomass = sum(spec_biomass)) #zusammenfassen der Daten, erstellen der Spalte Gesamtbiomasse 

species <- rkurs_data %>% #neuer Datensatz species
  group_by(region, datetime) %>% 
  summarise(sum_biomass = mean(spec_biomass), #zusammenfassen der Daten, berechnen der mittleren Biomasse
         sd_biomass = sd(spec_biomass))  # berechnen der sd 


# plotten:
# mit ggplot, als erstes wird die Gesamtbiomasse geplottet 
# 
ggplot(sum_data, aes(x = datetime, y = sum_biomass))+ #hier muessen die Elemente x und y gleich sein, da in beiden df danach gesucht wird
  geom_point(colour = 'darkred')+ #Datenpunkte fuer sum_data erstellen
  geom_point(data = species, colour='black', shape = 21, size = 2)+ #Datenpunkte aus unserem anderem Datensatz
  geom_line(data = species, linetype = 'dashed', colour = 'black')+ #Linie erstellen, die unsere Punkte von dem species df verbindet, mit linetype spezifiziere ich den Typ
  geom_errorbar(data = species, colour = 'black', aes(ymin = sum_biomass-sd_biomass, #Fehlerbalken erstellen, hier muss spezifiziert werden aus welchem Datensatz das ganz kommt
                ymax=sum_biomass+sd_biomass, width=.2)) + #sprich mit data=species in diesem Fall 
  geom_smooth(method = 'loess', se=F, color = 'darkred')+ #erstellt eine Trendlinie
  #Alternative: stat_smooth(method = 'loess', se=F, color = 'darkred')
  scale_x_datetime(breaks = '2 days', date_labels = '%d.%m')+ #veraendert die Achsenticks mit breaks, date_labels veraendert die Darstellung
  labs(x=' ', y = expression(Biomass~'in'~mu*gCL^{-1}))+ # erstellt x und y Titel meines ggplots, expression wird genutzt, um Sonderzeichnen benutzen zu koennen
                                                         # mit ~ werden Leerzeichen definiert. in muss in '' gesetzt werden, da es sich hier auch um eine Funktion handelt 
  facet_wrap(~region, scale = 'free_y')+ #free_y erlaubt andere skalen zu setzen in jedem facet
  theme(panel.background = element_rect(fill = NA), #loescht den Hintergrund meines Plots/ fuellt ihn mit nichts
        panel.grid.major.y = element_line(color='grey', linetype = 20), #setzt horizontale, gestrichelte Linien in grau
        axis.line = element_line(), #setzt x-y-Achsenlinien 
        strip.background = element_blank(), #loescht meinen strip background 
        strip.text = element_text(size = 12, face = 'bold'), #veraendert meinen strip text, so dass er fett geschrieben und groesser ist.
        axis.text.x = element_text(size = 10)) #vergroessert die x-Achsenbeschriftung



##-----------------------------------------------------------------------------------------------------------------------------##
#------------------------------------------------------------------------------------------------------------------------------------------#

# Uebung: Erstelle einen plot, der die mittlere Naehrstoffkonzentration pro Station enthaelt


labels <- c('nitrate' = 'Nitrate', 'phosphate'='Phosphate', 'silicate'='Silicate') #erstellen eines Labelvektors, um die facets anders zu bennenen

#einladen der nutrient_summary.csv data aus der Studiengruppe

nutrient_data <- read_csv("~/Desktop/Uni/MASTER/WS 2018:2019/Rkundung/Datensatz/nutrient_summary.csv")

#-----------------------------------------------------------------------------------------------------------------#
# oder: 

#Zusammenfassen meiner Daten und speichern als nutrient_data
nutrient_data <- rkurs_data %>% 
  group_by(datetime, region)%>% #gruppieren nach region und date
  distinct(datetime, region, phosphate, silicate, nitrate) %>% #schmeisst die duplicate raus 
  gather( key = nutrient, value = concentration, -datetime, -region) %>% # gathern der Daten und erstellen der Saplte key nutrient.
  group_by(region, nutrient)%>% #neues gruppieren, da wir die Gesamtnaehrstoffkonzentration ausrechnen wollen
  summarise(mean_nutrient = mean(concentration), #zusammenfassen des df, erstellen von neuen Spalten
            sd_nutrient = sd(concentration))


#plot erstellen
plot <- ggplot(nutrient_data, aes(x=region, y=mean_nutrient, color = region))+ #color faerbt die regionsdaten entsprechend
  geom_point()+ #erstellt Punkte
  geom_errorbar(aes(ymin = mean_nutrient-sd_nutrient, # error bars erstellen 
                ymax = mean_nutrient+sd_nutrient), width=.2 )+ # mit width wird der Typ bestimmt. 
  facet_wrap(~nutrient, scale = 'free_y',labeller=labeller(nutrient = labels))+ #facets erstellen fuer die jeweiligen nutrients mit freier y-Skala
                                                                                #und den selbstgewaehlten labels definiert in meinem label-vector oben
  labs(x= ' ', y= expression(Concentration~'in'~mu*gL^{-1}))+ #ueber Expression kann ich einen Titel erstellen, der Sonderzeichen enthaelt 
                                                              #mit mu* sage ich R, dass er mu als µ sehen soll.
  labs(color= 'Region')+ # gibt der Legende einen Titel
  scale_color_manual(values = c('#190788', '#0783BD', '#F57607','#F4B004'))+ #eigene Farben definieren
  theme(panel.background = element_rect(fill = NA), # Hintergrund weiß
        panel.grid.major.y = element_line(color='grey', linetype = 20), # horizontale, graue linien in dashed 
        axis.line = element_line(), #zeichnen der x-y-Linien
        strip.background = element_blank(), # loescht den strip background 
        strip.text = element_text(size = 12, face = 'bold'), #ueberschriften fett und grosser
        axis.text.x = element_text(size = 10), #achsenbeschriftung groesser
        legend.key = element_blank(), #symbol hintergrund blank
        legend.background = element_blank(), #legendenhintergrund blank
        legend.box.background = element_rect(size = 0.5), #box erstellen um die Legende
        legend.position = 'bottom') #Legendposition am Boden
plot

ggsave('nutrient.png',plot = plot, width = 15, height = 7) #speichern des nutrient-plots 

#--------------------------------------------------------------------------------------------------------------------------------------------##
#------------------------------------------------------------------------------------------------------------------------------------------#
# Coole Websiten mit Tipps zu Plots: 

# https://www.data-to-viz.com zeigt an, welche Plots fuer welche Daten geeignet sind
# theme website: https://ggplot2.tidyverse.org/reference/theme.html


# Eine Einfuehrung in GitHub:
# https://github.com dient dem Verteilen von R-Skripten und gemeinsamen Arbeiten an einem Skript (aehnlich wie google.docs fuer R Skripte)







 