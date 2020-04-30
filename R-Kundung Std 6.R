#Session 6                                                                                  04.12.2018
# Dominik Bahlburg, Charlotte Kunze

#Inhalt: Loops, if-Abfragen und eigene Funktionen 

setwd("~/Desktop/Uni/MASTER/WS 2018:2019/Rkundung") #setzen eines Working Directory

library(tidyverse)
library(scales)

rkurs_data <- read_csv("~/Desktop/Uni/MASTER/WS 2018:2019/Rkundung/Datensatz/rkurs_data02.csv")

#------------------------------------------------------------------------------------------------------------------------#

#WDH der letzten Session
#Visualisieren von Teildaten

#Uebung:
#Aufgabe: Untersucht wie sich die HAB-Arten an den Stationen ueber die Zeit verhalten:
#Wie sieht der prozentuale Anteil von HAB-Arten an der Gesamtbiomasse zu jedem Zeitpunkt in den vier Regionen aus. 
#Beachte dabei auch wie viel davon von leicht-, mittel-, und hochtoxischen Arten beigetragen wird.


data_ES <- rkurs_data %>% #neuer Datensatz data_ES auf Basis des Ursprungsdatensatzes erstellen
  group_by(date, region)%>% #gruppieren nach region und date, damit nicht eine Summe der Biomassen ueber alle Zeiten und Regionen gebildet wird.
  mutate(sum_biomass = sum(spec_biomass), #berechnen der Gesamtbiomasse
         rel_biomass = spec_biomass/sum_biomass)%>% #berechnen der relativen Biomasse
  filter(toxicity != 'non')%>% #schmeisst alle Arten raus, die nicht toxisch sind (alternativ geht auch HAB == '1')
  group_by(date, region, toxicity)%>% #um die Toxizitaetslevel auszuwaehlen, neues Gruppieren der Daten auch nach toxicity
  summarise(rel_hab = sum(rel_biomass))%>% #summiert die relativen Biomassen der Arten auf nach toxicity
  filter(region == 'Elbe Estuary') %>%#waehlt die Region aus, fuer die der subset erstellt werden soll

ggplot(data_ES,aes(x=date, y=sum_rel_biomass, fill = toxicity)) + #fill macht die Balken farbig nach toxicity
  geom_bar(stat = 'identity')+ #Voreinstellungen aussetzen, damit geom_bar ncith immer neu alles berechnet
  labs(x= '', y='relative abundance of HAB species', title = 'Elbe Estuary') #erstellt Namen fuer die x,y - Achse und einen Titel
       
#nun koennte man diesen Text kopieren und jeweils die Region veraendern um fuer die anderen Regionen auch einen plot zu erstellen
#um die Automatisierung soll es nun gehen

#------------------------------------------------------------------------------------------------------------------------#
##Loops, Abfragen, Funktion

#for-Schleifen

#Ein einfaches Beispiel
for(i in 1:10){ #i ist mein Laufparameter, der die Werte 1 - 10 annehmen kann
  print(i+i) #gibt das Ergebnis von i+i in der Console aus
} #umfasst meine schleife, makiert Anfang und Ende


#if-Abfragen (wenn ... dann ...)
# funktioniert wie ein Filter

if(region == 'Elbe Estuary'){ #wenn unsere Region Elbe Estuary ist
   # dann mach...
} else if(region == 'English Chanell') # sonst wenn region english channel ist, mach...


##

#erstellen unseres Subsets
data_sub <- rkurs_data %>%
  group_by(date, region)%>% #gruppieren nach region und date, damit nicht eine Summe der Biomassen ueber alle Zeiten und Regionen gebildet wird.
  mutate(sum_biomass = sum(spec_biomass), #berechnen der Gesamtbiomasse
         rel_biomass = spec_biomass/sum_biomass)%>% #berechnen der relativen Biomasse
  filter(toxicity != 'non')%>% #schmeisst alle Arten raus, die nicht toxisch sind (alternativ geht auch HAB == '1')
  group_by(date, region, toxicity, exposure)%>% #um die Toxizitaetslevel + Exposure auszuwaehlen, neues Gruppieren der Daten 
  summarise(rel_hab = sum(rel_biomass)) #zusammenfassen der Daten


  
#um verschiedene regions abzudecken, definiere ich einen Vektor mit den Elementen
#exposure wird auch ein Vektor um auf die Daten zuzugreifen, und nur ein Element pro region zu erhalten
#unser Laufparameter i waehlt dann entsprechend das jeweilige Element aus.
region_vec <- c('Elbe Estuary','Irish Sea', 'English Channel','Norwegian Trench') #erstellt einen regions-vektor
exposure_ind <- c('low','high', 'low','high')  #Exposure muss in derselben Reihenfolge sein wie die regions, sprich das erste Element der region ist auch das erste Element aus Exposure

for (i in 1:4){ #for-schleife, die 4 mal laufen soll. Dafuer definieren wir den Laufparameter i, der die Werte 1-4 annimmt. 

  plot_data <- data_sub %>% #bennenen des plots, um etwaige Aenderungen immer wieder speichern zu koennen. 
    filter(region == region_vec[i]) %>%#region soll der regions_vector sein an der Stelle i - Sprich fuer Elbe Estuary i =1, Norwegian Trench i = 4
    ggplot(., aes(x=date, y=rel_hab, fill = toxicity)) + #damit der Datensatz nicht immer ueberschrieben wird, lasse ich mir die Ergebnisse in Form eines plots jeweils ausgeben
    geom_bar(stat='identity')+ #erstellen eines barplots, mit stat = identity wird die dahinterliegende Statistik 'ausgestellt', die selber nochmal Daten zusammenfassen will
    labs(x='', y='relative abundance of HAB species', title = region_vec[i])+ #title wird automatisch generiert ueber den region_vec
    theme(plot.title = element_text(face = ifelse(exposure_ind[i] == 'low', 'italic', 'bold')) )# in theme koennen verfeinerungen unserer plotfunktionen vorgenommen werden, um die Art der Beschriftungen zu aendern nehmen wir face
                                                  # ifelse fasst die Funktionen einer For-Schleife zusammen, ist aber etwas anders von der syntax Format: ifelse(Wenn exposure_ind = 'low', schreibe italic, Sonst: bold) 
    print(plot_data) #ausgeben der plots
    ggsave(paste(region_vec[i],'.png'), plot = plot_data, width = 8, height = 3.5)# ggsave speichert und exportiert die plots am Ort unsere Working Directory, mit .png kann das Format spezifiziert werden
                                            # in Anfuehrungszeichen wird spezifiziert, wie die Datei heissen soll, da immer verschieden Namen gefordert sind, nehme ich meinen Vektor und fuege die Namen mit paste im richtigen Format zusammen                                      
                                            # plot = gibt den plot an, der exportiert werden soll
                                            # mit Hilfe von width und height kann man die Groesse auswaehlen
             }




