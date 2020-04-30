
#R-Kurs 2. Treffen 6.11.2018
#Intro

#Wiederholung einiger wesentlicher Inhalte:
#Basic Data types: integer, real, character, logical
#generell definieren von Variablen:
a <- 7 #ganze Zahl, integer
a <- 7/3 #reelle Zahl - real
a <- 'hello world' # character
a <- TRUE #logical

##vectors (c()), matrices (matrix()), dataframes(data.frame())

#Ansprechen eines Elements im Vektor:
a <- 1:5 #erstellen eines vectors
a[2] #ansprechen des zweiten Elements im Vektor a 

#Ansprechen eines Elements in Matrix/Data-Frame
m <- matrix(1:9, ncol = 3) #erstellen einer 3x3 Matrix
m[2,3] #ansprechen eines einzelnen Elements
m[2,] #ansprechen der 2. Zeile (sprich alle Werte aus der 2.Zeile)
m[,3] #ansprechen 3. Spalte (alle Werte aus der 2. Spalte)

#im data-frame zusaetzlich ansprechen einer Spalte
df<- data.frame(x1=c('a', 'b', 'c'),
                x2=1:3,
                x3= 4:6) #erstellen eines df mit 3 Spalten x1,x2,x3

df$x1 #ansprechen der Spalte x1 im df
df[,1] #Alternative, spricht auch die erste Spalte an
df[[1]] #Alternative

#Struktur erkunden von Objekt:
str(df)

#-------------------------------------------------------------------------------------------------#
#Nachtrag Funktionen
#ermoeglichen es denselben Programmcode auf unterschiedliche Eingabedaten anzuwenden
#Syntax: function(comma-separated arguments) Wichtig: Runde Klammern! Eckige Klammern dienen zum Indizieren von Objekten!
#Beispiel Funktion mit einem Argument
s <- 1:6 #erstellen des vectors s
mean(x = s) #berechnen des Mittelwertes, generelle Struktur siehe Hilfe 
sd(x = s) #berechnen der Standardabweichung 

# Ein anderes Beispiel 
a <- c(1,2,3) #erstellt vector a 
mean_a <- (1+2+3)/3 #haendisches Berechnen des Mittelwertes
mean(a) #berechnet den Mittelwert von a anhand der Funktion mean()


#Beispielfunktion mit mehreren Argumenten
#fuer die Plotfunktion muessen generell zwei Argumente x,y gegeben sein, hier x_values und y_values
x_values <- 1:5
y_values <- x_values^2
plot(x = x_values, y = y_values) 

#Informationen zu Funktionen:
?mean
help(mean)

#Installation of packages
#Zur Installation von packages wird die Funktion install.packages() verwendet. 
#Das package der Wahl wird dabei in '' gesetzt. 
#Hier wird das package reshape2 geladen, das die melt() funktion beinhaltet, die wir spaeter zum 
#aufraeumen unseres dataframe benoetigen
install.packages('reshape2')
library(reshape2) #laden des package in unsere library, um auf die enthaltenen Funktionen zugreifen zu koennen.
library(tidyverse)

#-------------------------------------------------------------------------------------------------#
#Uebung 1: Erstelle einen Vektor mit folgenden Elementen: 0,2,4,6,8,10 und benutze dafuer die seq() - Funktion
#Berechne anschliessend Mittelwert und Varianz dieses Vektors
#Hierbei ist es sinnvoll, die Hilfe fuer seq() anzuschauen
?seq #oder auch
help(seq)

#Loesung:
x <- seq(from=0,to=10,by = 2) 
x <- seq(0,20,2) #verkuerzt 
x_mean <- mean(x) #berechnet den Mittelwert von x und speichert es in x_mean
x_var <- var(x)#berechnet die Varianz von x und speichert sie in x_var


#Uebung 2: 
#Wir nehmen den df der letzten Stunde. 
df <- data.frame(land=c("Deutschland", "Schweiz", "Niederlande"),    #eine Spalte, die Land heisst und drei Elemente hat
                 pop=c(82.52e6, 8.48e6, 17.13e6), #meine zweite Spalte heisst pop (oder auch populatio) und gibt die Einwohnerzahl an
                 bip_per_capita=c(44550,80591,48346)) # meine dritte Spalte gibt mir den BIP per capita an

#1.Berechnet den Mittelwert des BIP-per-capita
#2.Plotten der BIP_per_capita gegen das Land

#Loesung:
mean_bip_per_capita <-mean(df$bip_per_capita) # berechnet den Mittelwert der Spalte und speichert sie unter mean_bip_per_capita
plot(df$land, df$bip_per_capita) #plotten der Spalte Land gegen BIP-per-capita

#________________________________________________________________________________________________________##
#Tidy data 

# Literaturempfehlung: Hadley Wickham - Tidy Data
# Hadley Wickhams Paper beschaeftigt sich mit der optimalen Datenorganisation.
# Dazu zur Veranschaulichung erst einmal ein sogenannter Messy-Datensatz:

da<-data.frame(treatment=c('a', 'b'),
           john_smith=c(NA, 2),
           jane_done=c(16,11),
           marry_jonson=c(3,1))
da #unser Datensatz ist rechteckig angeordnet 

# Beschreibung eines Datensatzes nach Wickham:
# ein Datensatz ist generell eine Sammlung aus values (qualitativ, quantitativ). 
# Values koennen Zahlenwerte sein muessen aber nicht (zB bei Treatment waere es a/b).
# Jeder vlaue gehoert dabei zu einer variable und einer observation.
# Eine observation beinhaltet alle values, die in einer Einheit gemessen wurden ueber alle Variablen 
# (alle variables ueber alle variables von einer Einheit)

# In unserem Beispiel haben wir also eine Tabelle aus:
# 3 variables: person, treatment, result
# values sind vorhanden fuer alle variablen 
# fuer die variable 'person' waeren die values also john smith, jane done and marry jonson
# variable treatement hat die values 'a' und 'b'
# variable result hat 6 moegliche values
# daraus ergeben sich insgesamt 18 values,6 observations

#Faustregeln fuer das Erstellen eines tidy Datensatzes:
# es ist einfacher funktionelle Zusammenhaenge zwischen den variables zu analysieren
# es ist einfacher zwischen Gruppen von observations zu vergleichen
# es gibt auch im Nachhinein die Moeglichkeit seinen Datensatz neu zu strukturieren, aber waehle trotzdem ein kluges Format

# Eigenschaften eines Tidy Datensatzes:
# Jede variable ist eine Spalte
# Jede observation ist eine Reihe
# Jede Art observational unit ist eine table

#so erhaelt jede Zeile alle Informationen zu meinen Messwerten
#nach tidy data ergibt sich daraus fuer unsere da Dataframe:
tidy_da<- data.frame(person=c('john smith', 'john smith', 'jane doe','jane doe','mary johnson','mary johnson'), 
                     treatment=c('a', 'b','a', 'b','a', 'b'), 
                     result=c(NA,2,16,11,3,1) )
tidy_da

##Uebung: Mach den folgenden Datensatz mess_df tidy

mess_df <- data.frame( religion = c('agnostic', 'catholic', 'muslim'),
                       below_10_k = c(200, 126, 138),
                       above_10_k = c(400, 178, 154))
mess_df

#meine variables: religion, class, person
#Die values sind entsprechend fuer religion = agnostic, catholic, muslim. 


#Haeufige Fehler, die gemacht wurden im Ursprungsdatensatz
# 1. values sind Spaltennamen (<10k, >10k)
# 2. mehrere variables in einer Spalte
# 3. variables sind in Spalten und Reihen  
# 4. mehrere Arten von observational units in einer table
# 5. eine observational unit in mehreren tables

# mit R und dem reshape2 package, das wir oben geladen haben, koennen wir nun unseren Datensatz 'tidy' machen.
df_tidy <- melt(mess_df, id.vars = c('religion')) #id.vars gibt an nach welcher variable ich strukturiere 
                                                  # die variable 'religion' bleibt bestehen, der Rest des Datensatzes wird umgeformt


# sogenannte R cheat sheets zum base R gibt es hier https://www.rstudio.com/wp-content/uploads/2016/05/base-r.pdf

#______________________________________________________________________________________________________________________________________##
# Einladen von eigenen Datensaetzen in Rstudio 
# 1. Ueber die Import Dataset Option in meinem Environment kann ich generell einen selbst erstellten Datensatz einladen.
# Dieser sollte am Besten im csv Format gespeichert sein 

# Das Einlesen eines Datensatzes kann auch mit der read.csv() Funktion erfolgen. 
# Euren eigenen Pfad koennt ihr euch dazu beispielweise aus der Console rauskopieren nach dem manuellen Einlesen (ueber Import Dataset)
# bei mir sieht das so aus: 

rkurs_data1 <- read.csv("~/Desktop/Uni/MASTER/WS 2018:2019/Rkundung/Datensatz/rkurs_data01.csv")
View(rkurs_data1)

# datensatz01 ist aufgrund der vielen Leerzeichen und Sonderzeichen in den Spaltenueberschriften total unuebersichtlich
# da R beim Einladen diese automatisch durch Punkte ersetzt. Gerade wenn man Spalten ansprechen will, ist das sehr unpraktisch. 
# Es gilt also: Lasst lieber die Einheiten und Leerzeichen in euren Spaltenueberschriften weg
# und erstellt ein Mastersheet in Excel, das Einheiten und genauere Erklaerungen gibt. 


rkurs_data <- read.csv("~/Desktop/Uni/MASTER/WS 2018:2019/Rkundung/Datensatz/rkurs_data02.csv")
View(rkurs_data)

# das ist unser fertiger Datensatz mit dem wir nun arbeiten werden, der wie ihr seht, keine ueberfluessigen Punkte in den Spaltenbezeichnungen enthaelt




