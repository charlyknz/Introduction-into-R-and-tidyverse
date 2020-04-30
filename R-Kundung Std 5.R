# 5. Session zur Daten Visualisierung
#WDH

# Grammar of Graphics - ggplots die Theorie
# Aufbau eines Plots:
# # 7 Elemente 
# 1. dates 
# 2. asthetics (oder auch scales): Beschreibung, wie meine Daten dargestellt werden sollen
## Angaben und Struktur der x-, y-Achse  
## aes() in ggplot, gefuellt mit Daten aes(data, x,y)
# 3. geometries: Geometrische Formen, verwendet zur Darstellung unserer Daten (z.B. Balken und Punkte)
## gennant geom_ in ggplot
# 4. facets: Fenster, die wir sehen koennen. Bspw. 4 Einzelplots fuer jede Region
## facet_ in ggplot
# 5. statistics: folgt der Statistik meiner Daten, bspw. Glaettung meiner Daten oder auch lineare Regressionen
## stat_ in ggplot
# 6. coordinates: cathetic, polar, usw. Koordinatensystem, in dem ich arbeite
# 7. theme: Erscheinungsbild unserer Plots, zB Hintergrundfarbe, Linien  
## theme_ in ggplot

#---------------------------------------------------------------------------------------------------

#Plot zur Darstellung der 7 Elemente eines ggplots

#gemittelte Biomasse ueber Zeit, pro Station in verschiedenen facets (Fenstern)

library(tidyverse)
rkurs_data <- read.csv("~/Desktop/Uni/MASTER/WS 2018:2019/Rkundung/Datensatz/rkurs_data02.csv")

data <- rkurs_data%>% #zuweisung der geaenderten Daten der Variable data 
  group_by(date, region)%>% #gruppiert nach Region und Date, um Datenstruktur zu aendern
  summarise(tot_biomass = sum(spec_biomass)) #zusammenfassen der data
  
#Alternative mit distinct, da unsere Tabelle nachdem wir mutate benutzt haben, lauter replicates beinhaltet
data1 <- rkurs_data%>% #zuweisung der geaenderten Daten der Variable data 
  group_by(date, region)%>% #gruppiert nach Region und Date, um Datenstruktur zu aendern
  mutate(tot_biomass = sum(spec_biomass)) %>% #erstellt neue Spalte 
  distinct(region, date, tot_biomass) #schmeisst die replicate raus


##Modifikationen
str(rkurs_data) # wir sehen, dass das Format fuer date ein Factor ist.

# Um das Datum in die richtige Form zu bringen, benutzen wir am liebsten POSIXct
data$date <- as.POSIXct(x=rkurs_data$date, format='%d.%m.%y') #Format wird 

# Erstellen eines ggplots
ggplot(data, aes(x= date, y=tot_biomass, group=region, colour= region, shape=region))+ #aes steht fuer aesthetics und gibt die Daten 
                                                         # group zeigt an, welche Datenpunkte zusammengehoeren und koennen verbunden werden
                                                        #colour generiert Farbe fuer die einzelnen regions, shape weist jeder region ein Symbol zu
  geom_point()+  #+ verknuepft die Elemente
  geom_line()+
  facet_wrap(~region, ncol=4)+ #facet_wrap erstellt Fenster, mit Hilfe der Tilde geben wir an, nach welcher variable die Fenster erstellt werden sollen 
                              #ncol gibt die Anzahl der Spalten in denen die Fenster angeordnet werden sollen
  theme_bw()+               #waehlt andere Einstellungen zu Hintergrund usw, theme_bw ist ein vorgegebenes Format
  theme(strip.background = element_rect(color='red'), #manuelle Einstellungen zu meinen Verschoenerungen passieren in theme()
        panel.grid = element_line(colour='#F5FFFA')) #Farben koennen generell auch anhand der hex colours (#F5FFFA) bestimmt werden (siehe google) 


#Cheat sheet dazu gibts hier: https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf

#---------------------------------------------------------------------------------------------------
#UEBUNG 1
#Erstelle einen Plot, der den zeitlichen Verlauf aller Naehrstoffe an allen Stationen zeigt. 
#Dafuer sollten die Naehrstoffe in separaten Fenstern (facets) dargestellt werden 
#und die Stationen optisch voneinander getrennt sein.

#gather(): mit key uebergebe ich eine neue variable die die nutrients enthaelt
#region und date bleiben erhalten, der Rest wird der neuen Variable zugefuehrt


#Loesung:
distinct_data <- rkurs_data %>%
  distinct(region, date, nitrate, phosphate, silicate) %>% #schmeisst duplicate raus und schrumpft den Datensatz auf region, date, nitrate, phosphate and silicate
  gather(key = nutrient, value = concentration, -region, -date)%>%# um tidy data zu erhalten,  brauchen wir alle Nutrients in einer Saplte
  mutate(date = as.POSIXct(x=date, format='%d.%m.%y')) #erstellen einer neuen Spalte, die ein neues Date-Format enthaelt 


ggplot(distinct_data, aes(x = date, y = concentration, group = region, colour = region)) +
  geom_point() +
  geom_line() +
  facet_wrap(~nutrient)  #Fenster sollen pro Naehrstoff erstellt werden

#---------------------------------------------------------------------------------------------------

#UEBUNG 2
#Erstelle einen stacked barplot, der zeigt welche Arten im Mittel >10% zur Gesamtbiomasse 
#an jeder Station beigetragen haben - in anderen Worten: Welche Arten sind an jeder Station 
#besonders häufig/wichtig (Häufigkeit definiert durch >10% Beitrag zur Gesamtbiomasse)

data_relbio <- rkurs_data %>%
  group_by(region, date) %>% #gruppieren der Daten 
  mutate(sum_biomass = sum(spec_biomass), #neue Spalte mit total_biomass und rel. biomasseanteil
         rel_biomass = spec_biomass/sum_biomass )%>%
  group_by(species, region)%>% #gruppieren nach species und region
  summarise(mean_contribution = mean(rel_biomass))%>% #zusammenfassen der Daten und erstellen einer Spalte, die den Anteil angibt
  filter(mean_contribution > 0.1)%>% #schmeisst alle species raus, die weniger als 10 % Anteil darstellen
  arrange(region, desc(mean_contribution)) #sortiert die Daten 

#stelle die Arten als Balkendiagramm dar, die mehr als 10% zur Gesamtbiomasse der Station beitragen
#kennzeichne die einzelnen Arten farbig
#plot
data_relbio %>%
  ggplot(.,aes(x = region, y = mean_contribution, fill = species))+ #erstellen eines barplots mit fill werden balken gefuellt
  geom_bar(stat = 'identity') 

