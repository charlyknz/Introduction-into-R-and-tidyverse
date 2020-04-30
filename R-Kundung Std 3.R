#RKundung3
#Dominik Bahlburg, Charlotte Kunze
#9.11.2018
#Session 3: Data handling und wrangling mit den Tidyverse-Packages
#--------------------------------------------------------------------------------------#
#Wiederholung vom letzten Mal:
#Tidy Data: 
#1. Jede Variable ist eine Spalte
#2. Jede Observation ist eine Zeile
#3. Jede Observational Unit ist eine Tabelle

#Funktionen in R: Ermoeglichen es denselben Code auf verschiedene Input-Argumente anzuwenden
#Syntax: function_name(comma-separated arguments)
#Bsp.:

#Input kann direkt (hard-coden) oder indirekt erfolgen:

print('hello world') # print() gibt Ergebnis in der Console aus
                      # hier wird 'hard gecoded', das Input Argument direkt in die Funktion geschrieben

#alltagstauglicher:
x <- 'hello world' #indirekt coden, das Input Argument wird eine Variable zugewiesen
print(x) 

#Funktionen kann man schachteln! Funktion1(Funktion2(Input-Argument))
print(seq(0,10,2)) #wir erinnern uns an die seq()-Funktionsaufbau seq(from, to, by)

#Logische Abfragen:
#Du stellst eine Frage an den Computer und bekommst als Antwort einen Boolean/Logical Output (0,1 oder TRUE, FALSE)
#Operatoren: <, >, <=, >=, !=, ==, %in%, Verknüpfung durch &, |
#z.B.
5 > 3 #hier wird mir TRUE als Ergebnis ausgegeben, denn 5 ist groesser als 3
5 < 3 #FALSE, 5 ist nicht kleiner als 3 
5 = 3 # Zuweisung und keine Abfrage
5 == 5 #Abfrage mit doppeltem ==
5 != 5 #neg-Abfrage, "ist nicht..."


#Vektoren
#ich kann auch Abfragen mit gesamten Vektoren durchfeuhren und erhalte dann einen Vektor aus TRUE und FALSE zu den entsprechenden Elementen
1:15 < 8 # 1:15 ist kleiner 8, das ist True bis zur Zahl 8 

x <- 1:15 #erstellen des Vektor x
sub <-x !=4 #erstellen eines subsets "sub", das die Ergebnisse meine Abfrage enthaelt (alle Zaheln, die nicht 4 sind)
            # output: TRUE  TRUE FALSE  TRUE  TRUE  TRUE
x[sub] #gibt die entsprechende Zahl aus, fuer die die Abfrage zutrifft, die Elemente meines Vektors die nicht 4 sind

#das geht auch mit Charakter-vektoren
y <-c('a', 'b', 'c', 'n') #Elemente meines Vektor x sind Buchstaben
sub_y <- y=='b' #Erstellen eines subsets sub_y, das die Ergebnisse meiner Abfrage enthaelt, sprich wann y == b ist
sub_y #output: FALSE TRUE FALSE FALSE da weder a, noch c, noch n = b ist. 

#%in% ueberprueft, inwieweit zwei Vektoren sich ueberschneiden
x <- 1:10
y <- 5:20
x %in% y #ueberprueft Ueberschneidungen von x und y, hier Zahlen 5:10 

#das geht auch fuer den Buchstaben-vektor y
y <-c('a', 'b', 'c', 'n')
y %in% c('a', 'b')  #y ist a, b an erster und zweiter Stelle daher der Output T,T,F,F
sub_t <- y %in% c('a', 'b') #speichert den Output in sub_t

#hier nochmal der Unterschied zwischen direkter und indirekter Eingabe des Inputarguments 
y[sub_t] #indirekt coden
y[y %in% c('a', 'b')] #direkt coden

#--------------------------------------------------------------------------------------#
#Laden der tidyverse-packages
install.packages('tidyverse')
library(tidyverse)

# HINWEIS: FALLS BEI EUCH DIE INSTALLATION VON TIDYVERSE NICHT FUNKTIONIERT,
# LADET DIE AKTUELLSTE VERSION VON R STUDIO UND R HERUNTER


#Beachte dass mehrere packages eingeladen werden! Conflicts: filter() und lag() aus base 
#werden maskiert/ueberdeckt. Moechte man diese Funktionen ansprechen:
stats::filter()
#funktioniert fuer alle Funktionen. Schema: package::function()

#--------------------------------------------------------------------------------------#
#Daten einlesen:
#moeglich ueber Import Dataset in Environment oder per Zeilenbefehl
#Einlesen ueber Import Dataset: Erste Entscheidung: base oder readr?
#Wenn Import ueber base: Data-Frame
#Wenn Import ueber readr: Tibble
#Was ist tibble? Tibble ist sehr aehnlich zu data-frame, jedoch mit weniger "Voreinstellungen"
#Format der Wahl: Geschmackssache aber die komplette Tidyverse-Umgebung wird fuer tibbles geschrieben
#Unterschiede nicht gravierend, data-frames funktionieren genauso mit tidyverse, benoetigen allerdings gelegentlich Umformatierungen
#daher:
env_data <- read_csv('~/Desktop/Uni/MASTER/WS 2018:2019/Rkundung/Datensatz/env_data.csv') #tibble
env_data_df <- read.csv('~/Desktop/Uni/MASTER/WS 2018:2019/Rkundung/Datensatz/env_data.csv') #dataframe
env_data_df
env_data

str(env_data) #gibt mir die Struktur meines Datensatzes in der Console aus, zeigt die einzelnen Spalten an, welche mit
              #Bezeichnungen versehen sind: <chr> fuer character, <dbl> fuer double = numerical variable

str(env_data_df) #gibt mir die Struktur des df zusammengefasst in der Console aus 
                # charakterisiert meine variables als factor fuer region:time und als numerical fuer temperature:silicate


station_data <- read_csv('~/Desktop/Uni/MASTER/WS 2018:2019/Rkundung/Datensatz/station_info.csv')
plankton_data <- read_csv('~/Desktop/Uni/MASTER/WS 2018:2019/Rkundung/Datensatz/plankton.csv')

#Unser Datensatz aus der letzten Session wurde zerstueckelt in 3 einzelnen Tabelle, die wir nun wieder zusammenfuegen wollen
# da unsere variable date eine Spaltenbezeichnung ist, benutzen wir die gather() funktion

# umformen der plankton_data zu tidy
# gather wird verwendet, wenn die Spaltennamen values sind sprich die Daten messy sind
# - wird behalten, wie es steht (invers gedacht)

# key ist die variable, die zusammengefasst wird, in unserem Fall wollen wir, dass das die values (die Tage) der variable date keine Spaltenueberschriften sind,
# sondern als eine eigene Spalte, mit der Uberschrift "date" vorliegen
#Struktur: gather(datensatz, key= variable, values= messwerte, - nicht mit einbeziehen )
plankton_data1 <- gather(plankton_data,key = date, value = spec_biomass, #gather braucht key and value um festzulegen, nach welcher variable und values zusammengefasst wird
                         -region, -toxicity, -HAB, -genus, -species)

#full_join funktionen: cheat sheet https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
#wichtig ist, dass mind eine Spalte in beiden Datensaetzen enthalten ist (hier region, denn ich habe Umweltdaten fuer jede region
#sowie planktondaten)
#left_join der linke Datensatz gilt als Referenzdatensatz und enstcheidet in welcher Reihenfolge die Inputargumente abgefragt werden 
#right_join der rechte Datensatz gilt als Referenzdatensatz und enstcheidet in welcher Reihenfolge die Inputargumente abgefragt werden 
#full_join haengt alle Daten aneinander 
#inner_join fasst nur die Daten zusammen, die in beiden Datensaetzen vorhanden sind 

#wir benutzen full_join mit der Struktur full_join(df1, df2 by= IDvariable)
data <- full_join(station_data, env_data, by = 'region') #zusammenfassen der Daten nach region
data1 <- full_join(data, plankton_data1, by = c('region','date')) #hier brauche ich zwei ID Variablen date und region, um die Daten fuer den entsprechenenden Tag 
                                                                  #zu erhalten und meine Daten nicht nur nach region gemittelt werden

write.csv(data1, file = 'tidy_plankton.csv') #speichern meiner neuen Tabelle im csv Format 
# um festzulegen, wo gespeichert wird, solltet ihr ein Arbeitsverzeichnis (working directory ) setzen

# Arbeitsverzeichnis manuell auslesen und als Kommandozeile einfuegen
# ueber Session --> Set Working Directory --> Choose Directory...
#Grund: Schnellerer Zugriff auf Dateien im Verzeichnis, Output wird im richtigen Verzeichnis gespeichert

setwd('~/Documents/Studium/R_Introkurs/03DataHandling')

#--------------------------------------------------------------------------------------#
#Wrangling des Datensatzes mit 6 grundlegenden Funktionen aus dplyr:

#select()	select columns
#filter()	filter rows
#arrange()	re-order or arrange rows
#mutate()	create new columns
#summarise()	summarise values
#group_by()	allows for group operations in the “split-apply-combine” concept

#filter: Dient dazu observations nach ihren values auszuwaehlen!
#Input: Logische Abfrage! Moeglichkeiten: <, >, <=, >=, !=, ==, %in%, Verknüpfung durch &, |
elbe_data <- filter(data1, region == 'Elbe Estuary') #herausfiltern der Daten fuer region Elbe Estuary und speichern des subsets in elbe_data

#arrange: Sortierung der Reihen (alphabetisch, numerisch auf-/ oder absteigend). Default: Aufsteigend
elbe_sorted <- arrange(elbe_data, desc(date)) #desc() sortiert die Daten nach absteigendem Datum 

#select: Variablen aussuchen, um Datensatz auf das noetigste zu reduzieren (z.B. Koordinaten nicht von Interesse erstmal)
elbe_reduced <- select(elbe_data, -latitude, -longitude)

#mutate: Erstellt neue Variable + neue Spalte mit Funktionen bereits existierender Variablen
elbe_mut <- mutate(elbe_data, mean_biomass = mean(spec_biomass)) #erstellt die variable mean_biomass aus dem mean der existierenden variable spec_biomass
                                                                  #fuer die ganze Station Elbe, gemittelt ueber alle Tage

#summarise: Fasst Daten/Observations zusammen
elbe_sum <- summarise(elbe_mut, mean_temp = mean(temperature))

#group_by: Bestimmt ob oben genannten Funktionen auf gesamten Datensatz oder gruppenweise ausgefuehrt werden sollen
#Gruppen koennen nach mehreren Variablen erstellt werden 
# bspw. wenn nicht nur noch region gemittelt werden soll, sondern auch nach date
elbe_grouped <- group_by(elbe_data,date)  # hier ensteht praktisch ein Unterdatensatz
elbe_grouped <- summarise(elbe_grouped, mean_temp = mean(temperature)) #fasst meine Daten zusammen und gibt mir die Durchschnittstemperatur pro Tag an. 



#naechste Woche geht es weiter mit Uebungen zu den einzelnen Funktion und wir wollen mit dem Daten visualisieren beginnen.

# umbedingte Leseempfehlung: https://r4ds.had.co.nz/transform.html R for Data Science - Hadley Wickham 


