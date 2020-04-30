# R-Skript zur 1. Rkundungs-Session
# 30.10.2018 


# 1. Das ausfuehren von Befehlen erfolgt anhand der Tastenkombination Steuerung und Enter (bei Mac Command und Enter

# 2. Zuweisungen
#Zuweisungen erfolgen mithilfe von Zuweisungsoperatoren wie dem Pfeil <- oder dem Gleichzeichen = 
#z.B. 
variable <- 1
a=1 
b=2

# nun kann man die zugewiesenen Variablen in dem Environment wiederfinden (rechtes Fenster)
# eine erneute Abfrage kann jederzeit erfolgen, indem die Variable nochmals ausgefuehrt wird. 
b #b ist 2, siehe untere Console


# 3. Formate die eine Variable annehmen kann
# generell kennt R sechs verschiedene Basic data types (Formate):
a=1      #ganze Zahl (integer)
b=0.3    #reelle Zahl (real)
a='hello world' #Textobjekt (character), (bzw. string/text)
                # die Elemente werden durch Anfuehrungszeichen "" oder '' gekennzeichnet bzw eingelsen
a=TRUE  #alternative boelean variable (logical) --> wie ein Schalter, entweder TRUE (auch T) oder FALSE (F)

# einfache Rechenoperationen mit Variablen, die Ergebnisse werden in der Console ausgegeben
a+b
a-b
a*b
a/b

#die Ergebnisse der Rechnungen koennen nun auch neuen Variablen zugewiesen werden anhand unserer Zuweisungsoperatoren 
# um das Ergebnis einsehen zu koennen, muessen nun die Variablen erneut ausgefuehrt werden
d <- a+b
d <- a-b
d <- a*b
M= a/b 


# 4. Vektoren
#Vektoren koennen wir variablen verschiedene Formate annehmen, allerdings nur, sodass sie homogen sind
#sprich eine Mischung der Formate ist nicht moeglich:
# numerisch (Zahlen) -integer, real 
# character (deskriptiv) 
# logical, Faktor ( zB blue, green oder auch True und False)

#die Zuweisung erfolgt mit einem Zuweisungsoperator
Vector1=c(1,2,3,4,5) # mit der Funktion c() wird ein Vektor erstellt, mit Elementen, die durch Kommatas getrennt werden  
Vector1=c(1:5)       # die Zahlenreihe kann man auch abkuerzen durch den Befehl 1:5 (alle Zahlen von einschließlich 1 bis 5)

vec2=c("Wort1", "Wort2", "Wort3") # character #Textobjekte werden wieder durch Anfuehrungszeichen "" oder '' gekennzeichnet 
vec3=c(TRUE, FALSE, TRUE, T, F) # logical 
vec4=c(4:8) #numerical, Zahlen 4,5,6,7,8

##--# Uebung:erstelle einen Vektor mit folgenden Elementen 1,2,3,4,5,10,11,12,13,14,15
# die Loesung ist einfach, meine Elemente werden mit einem Komma getrennt. 
# da es sich um Zahlenreihen handelt, kann ich meinen Schreibaufwand erheblich einschraenken
vec<- c(1:5, 10:15)


# Rechenoperationen mit Vektoren
# Rechnen mit Vektoren erfolgt nach dem gleichen Prinzip wie das Rechnen mit anderen Variablen
# hier einige einfache Beispiele 

vec1 <- 1:6
vec2 <- 1:3
vec3 <- vec1 + vec2
vec3 <- vec1 - vec2 #hier benennen ich mein Ergebnis immer wieder mit vec 3 und ueberschreibe es so 
vec4 <- vec1 * vec2 
vec3 <- vec1 / vec2
# Die Ergebnisse lassen sich wieder folgendermassen ausgeben: 
vec3 # mein erzeugter Vektor ist 1.0 1.0 1.0 4.0 2.5 2.0 
vec4 #mein erzeugter Vektor lautet 1  4  9  4 10 18


# 5. Das Erstellen einer Matrix 
## Eine Matrix ist eine zweidimensionale Anordnung von Vektoren
# auch hier gilt wieder: alle Elemente muessen einheitliches Format haben (numerisch, character oder logical)

mat1=matrix(c(1,2,3,1,2,3),  # mit der Operation c() uebergeb ich meine Elemente, die ich in der Matrix haben moechte
            ncol = 2,        # ncol gibt die Anzahl der Spalten - R fuellt spaltenweise den Vektor, also von oben nach unten und dann die naechste Spalte
            dimnames = list(c("Reihe1", "Reihe2", "Reihe3"), # dimnames benennt meine Zeilen und Spalten    
                            c("Spalte1", 'Spalte2')))  # es gilt: ZZ SS - Zeile zuerst, Spalte spaeter
mat1 # schauen wir uns das ganze nochmal zur Kontrolle in der Console an

##--# Uebung: Erstelle eine 2x2 Matrix mit folgenden Spalten: 
## Spalte 1: 4,3,4 #Spalte 2: 4,2,4
## betitel die Zeilen wie folgt: Zeile 1, Zeile 2, Zeile 3 
## die Spalten sollen wie folgt betitelt werden: s1, s2 

mat2=matrix(c(4,3,4,4,2,4),
            ncol=2,  #R fuellt spaltenweise den Vektor, also von oben nach unten und dann die naechste Spalte
            dimnames = list(c("Zeile1", "Zeile2", "Zeile3"), # Zeilenueberschrift
                            c("s1","s2"))) # Spaltenueberschrfit
# Haeufige Fehler: 
matrix(c(4,3,4), c(4,2,4), ncol=2,
       dimnames = list(c("Zeile1", "Zeile2", "Zeile3"), # Zeilenueberschrift
                       c("s1","s2"))) #hier entsteht ein roter Fehler in meiner Console, da ich zwei Vektoren uebergebe 
 

# 6. Hilfe finden bei R Studio 
?dimnames #mit dem Fragezeichen vor dem Befehl, oeffnet sich ein Hilfefenster in Rstudio



# 7. Erstellen eines Dataframes (df abegekuerzt) 
## Ein Dataframe ist wie eine Matrix, kann aber aus Elementen mit verschiedenen Formaten bestehen, sprich heterogen sein
## Ein einfaches Beispiel: 
dat1 <- data.frame(x1 = 1:3, # die erste Spalte soll x1 heissen und folgende Elemente beinhalten: Zahlen 1,2,3 (numerical)
                   x2 = 2:4, # die zweite Spalte soll x2 heissen und Elemente 2,3,4 enthalten
                   x3 = c('wort1','wort2','wort3')) # die dritte Spalte x3 enthaelt die Elemente wort1, wort2, wort3 (character)

#nun ein anwendungsbezogenes Beispiel

df=data.frame(land=c("Deutschland", "Schweiz", "Niederlande"),    #eine Spalte, die Land heisst und drei Elemente hat
              pop=c(82.52e6, 8.48e6, 17.13e6), #meine zweite Spalte heisst pop (oder auch populatio) und gibt die Einwohnerzahl an
              bip_per_capital=c(44550,80591,48346)) # meine dritte Spalte gibt mir den BIP per capital an

#nun einige Spielerein: 

str(df) #structure gibt an, in welchem Format die Daten eingelesen wurden fuer jede Spalte   

#str(df) gibt uns folgenden Output 
#'data.frame':	3 obs. of  3 variables:
#$ land           : Factor w/ 3 levels "Deutschland",..: 1 3 2
#$ pop            : num  82520000 8480000 17130000
#$ bip_per_capital: num  44550 80591 48346

# einige Anmerkungen:
#Factor w/3 levels "Deutschland",..: 1 3 2: dies bedeutet, dass R den Laendern Zahlen zuordnet, und zwar in der alphabetischen Reihenfolge. 
#Es gibt drei Level, da drei verschiedene Laender genannt wurden: Deutschland (1), Niederlande (3), Schweiz (2) 


# Ansprechnen einer Spalte innerhalb eines dataframe:
# zum Ansprechen einer Spalte verwendet man das folgende Format: Dataframe$Spaltenname
# wenn wir also die Spalte Land aus unserem Dataframe ziehen wollen und in einer neuen Variable speichern, geht das wie folgt:
a = df$land   #Spalte land aus dem dataframe ziehen und als Variable a speichern


#wir wollen unsere vorher festgelegte Reihenfolge Deutschland (1), Schweiz (2) und Niederlande (3) behalten
#das machen wir anhand der folgenden Funktion
df$land=factor(df$land, levels = c("Deutschland", "Schweiz", "Niederlande")) #Aenderung der Reihenfolge der Faktorenvergabe

#ob das funktiniert hat, koennen wir durch erneutes Anwenden von str(df) ueberpruefen


#Einen Wert aus df ziehen
#Ansprechen des Wertes der in der 1. Zeile 3. Spalte steht 
df[1,3] # die eckigen Klammern enthalten den Ort, an dem der Wert in meiner Matrix steht: 1 (Zeile zuerst), 3 (Spalte spaeter)


##--## Uebung: erstelle eine neue Spalte in deinem df mit dem gesamt BIP (Produkt aus bip_per_capital und pop)

df$bip= df$pop*df$bip_per_capital #df$bip kreiert neue Spalte mit gewuenschten Elementen 


#Hauefige Fehler:
#erneutes erstellen des dataframe df und das einbinden der bip-Spalte
#dies ist nicht moeglich, da fuer das Berechnen des bip bereits die Spalten pop und bip_per_capital bekannt sein muessen
df=data.frame(land=c("Deutschland", "Schweiz", "Niederlande"),   
              pop=c(82.52e6, 8.48e6, 17.13e6),
              bip_per_capital=c(44550,80591,48346),
              bip=c(df$pop*df$bip_per_capital))


#Ein paar allgemeine Tipps:
#sinnvolle Objektbezeichnungen: Benutzt Namen, die nicht mit Funktionen oder Argumenten zu verwechseln sind! (z.B. mean)
#Idealerweise beinhaltet der Name Informationen ueber das Objekt (z.B. "bodysize" anstatt von "value")
#macht eure eigene standardisierte Syntax (z.B. Matrixname - Großbuchstabe, Vektorname - Kleinbuchstabe o.A.)
#Kommentiert und strukturiert eure Skripte. Jeder sollte nachvollziehen koennen was ihr gemacht habt, vor allem ihr selber nach einiger Zeit...
#Kennt das "Vokabular" - ihr werdet Fehler besser verstehen und wisst wonach ihr googlen muesst falls ihr Hilfe braucht
#meidet Umlaute, Eszett, griechisch/roemische Buchstaben usw. im Skript. Oft funktioniert es zwar bei euch aber zerschiesst auf anderen Computern
#achtet auf das Format von Anfuehrungszeichen, wenn ihr ein fremdes Skript oeffnet... je nachdem wie die Schrift bei euch codiert ist kann das Skript an einem anderen Computer dort Probleme bereiten
#Gewoehnt euch an "." als Dezimalzeichen zu verwenden - auch in Excel
#Speichern am besten im UTF 8 Format, dies ist das gaengigste.

