#RKundung3
#Dominik Bahlburg, Charlotte Kunze
#20.11.2018
#Session 4: Data wrangling und data visulasation 
#--------------------------------------------------------------------------------------#
#Laden der tidyverse-packages, um Funktion benutzen zu koennen

library(tidyverse)

#Arbeitsverzeichnis auslesen und als Kommandozeile einfuegen
#Grund: Schnellerer Zugriff auf Dateien im Verzeichnis, Output wird im richtigen 
#Verzeichnis gespeichert

setwd('~/Documents/Studium/R_Introkurs/03DataHandling')

#--------------------------------------------------------------------------------------#
#Daten einlesen:
#moeglich ueber Import Dataset in Environment oder per Zeilenbefehl
#Wenn Import ueber base oder read.csv: Data-Frame
#Wenn Import ueber readr oder read_csv: Tibble
#Was ist tibble? Tibble ist sehr aehnlich zu data-frame, jedoch mit weniger "Voreinstellungen", fuer tidyverse

plankton <- read_csv("~/Desktop/Uni/MASTER/WS 2018:2019/Rkundung/Datensatz/rkurs_data02.csv")


#--------------------------------------------------------------------------------------#
#Wrangling des Datensatzes mit 6 grundlegenden Funktionen aus dplyr:
#filter()
#arrange()
#select()
#mutate()
#summarise()
#group_by()

#filter: Dient dazu observations nach ihren values auszuwaehlen!
#Input: Logische Abfrage! Moeglichkeiten: <, >, <=, >=, !=, ==, %in%, Verknüpfung durch &, |
elbe_data <- filter(plankton, region %in% c('English Channel', 'Elbe Estuary'))

#arrange: Sortierung der Reihen (alphabetisch, numerisch auf-/ oder absteigend). automatisch Default: Aufsteigend, mit descendent:absteigend
plankton_sorted <- arrange(plankton, desc(spec_biomass))

#select: Variablen aussuchen, um Datensatz auf das noetigste zu reduzieren (z.B. Koordinaten nicht von Interesse erstmal)
elbe_reduced <- select(elbe_data, -latitude, -longitude)

#mutate: Erstellt neue Variable mit Funktionen bereits existierender Variablen
elbe_data <- mutate(elbe_data, mean_p = mean(phosphate)) #fasst alle Eintraege von phosphate zusammen und mittelt sie

#summarise: Fasst Daten/Observations zu einer Zusammenfassung zusammen, so dass der Datensatz eingestampft wird
elbe_sum <- summarise(elbe_data, mean_temp = mean(temperature))

#group_by: Bestimmt ob oben genannten Funktionen auf gesamten Datensatz oder gruppenweise ausgefuehrt werden sollen
#Gruppen koennen nach mehreren Variablen erstellt werden (z.B. Region und Tag)
#Format: group_by(data, variable1, variable2)
elbe_grouped <- group_by(elbe_data, region, date)  # hier ensteht praktisch ein Unterdatensatz
#elbe_grouped <- nest(elbe_grouped)
elbe_grouped <- summarise(elbe_grouped, mean_temp = mean(temperature)) 

#--------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------#
#Uebung1: Erstelle ein Subset, das nur noch Irish Sea und Norwegian Trench enthaelt
#Berechne anschliessend die mittlere Wassertemperatur sowie Phosphat-Konzentration fuer 
#jeden Tag in beiden Regionen

#Loesung:
data_sub <- filter(data, region %in% c('Irish Sea','Norwegian Trench'))
data_grouped <- group_by(data_sub, region, date)
data_summarised <- summarise(data_grouped, mean_temp = mean(temperature),
                             mean_p = mean(phosphate))


#Einfuehren des Pipe-Operators: nur moeglich bei Funktion bei deinen Dataargument gegeben wird
# der Form function(data, ....)
#dadurch spare ich jedes mal den Datensatz einzugeben und etliche subsets zu erstellen

data_summarised <- data %>%
  filter(region %in% c('Irish Sea','Norwegian Trench')) %>%
  group_by(region, date) %>%
  summarise(mean_temp = mean(temperature),
            mean_p = mean(phosphate))

#--------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------#

#Uebung1a: Berechne den durschnittlichen Anteil der HAB-Arten zur Gesamtbiomasse pro Station

# in der Session
hab_sub <- plankton%>%
  group_by(region, date)%>% #gruppiert pro Station und Date
  mutate(sum_biomass = sum(spec_biomass)) %>% #berechnet in neuer Spalte den relativen Anteil der Arten
  filter(HAB == 1)%>% #filtert nach HAB Arten - wenn ich vorher filtere, kann ich aber keine gesamtbiomasse und anteil berechnen
  mutate(hab_biomass = sum(spec_biomass),
       rel_hab = hab_biomass/sum_biomass)%>%
  group_by(region)%>% #hier muss nochmal gruppiert werden, damit ich auf die Anteile der einzelnen Spezies komme
  summarise(mean_rel_hab = mean(rel_hab)) 
# jetzt wird geplottet!

#generelles Format ggplot(data, aes(x=variable1, y=variable2))

ggplot(hab_sub, aes(x=region, y=mean_rel_hab, fill = region))+ #mit fill fuelle ich die Balken, anwendbar fuer geom_bar
  geom_bar(stat = 'identity') # um geom_bar zu benutzen, ohne die dahinterliegende Statistik zu benutzen 
                              # mit stat = 'identity' sagen wir, dass unsere oben berechneten Werte benutzt werden

ggplot(hab_sub, aes(x=region, y=mean_rel_hab, colour = region ))+ # colir fuellt die Punkte aus, anwendbar fuer geom_plot
  geom_point()  #erstellen eines Punkte-plots


#Schichten der Plots:
ggplot(hab_sub, aes(x=region, y=mean_rel_hab, fill = region))+
  geom_bar(stat = 'identity')+
  geom_point() 

#--------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------#

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

