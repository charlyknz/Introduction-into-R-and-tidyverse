#UEBUNG 5
#Erstellen von Plots

library(tidyverse)
library(scales)

rkurs_data <- read_csv("~/Desktop/Uni/MASTER/WS 2018:2019/Rkundung/Datensatz/rkurs_data.csv")
str(rkurs_data)
# Aufgabentyp 1: Data handling and transformation

# Uebung Z1: Berechne den durchschnittlichen Shannon-Index fuer jede Station
# Formel: H = - ∑ (p * log(p))
# p = n (Anzahl species)/ N (Gesamtanzahl)

# Loesung:
data_shannon <- rkurs_data %>%
  group_by(region, date) %>%
  mutate(sum_biomass = sum(spec_biomass), rel_biomass = spec_biomass/sum_biomass)%>%
  summarise(H = -sum(rel_biomass*log(rel_biomass)))

#oder 
data_dome <- rkurs_data %>%
  group_by(region, date) %>%
  mutate(biomass = sum(spec_biomass),
         rel_ab = spec_biomass/biomass,
         H = -sum(rel_ab*log(rel_ab))) %>%
  summarise(mean_H = mean(H))



#Uebung Z5:
#Erstelle ein Subset, das nur noch Irish Sea und Norwegian Trench enthaelt
#Berechne anschliessend die mittlere Wassertemperatur sowie Phosphat-Konzentration fuer 
#jeden Tag in beiden Regionen

data_summarised <- plankton_tbl %>%
  filter(region %in% c('Elbe Estuary','Norwegian Trench')) %>%
  group_by(region, date) %>%
  summarise(mean_si = mean(silicate),
            mean_p = mean(phosphate),
            mean_n = mean(nitrate))



#Uebung Z3:
#Erstelle eine date-time Spalte, um das POSIXct Format zu nutzen

rkurs_data$datetime <- paste(rkurs_data$date, rkurs_data$time, sep = ' ') #erstellt eine neue Spalte datetime mit date und time, getrennt durch nen Leerzeichen
rkurs_data$datetime <- as.POSIXct(rkurs_data$datetime, format = '%d.%m.%y %H:%M') #acht hier auf das richtige Format - y/Y

#----------------------------------------------------------------------------------------------------------------------------------##
#----------------------------------------------------------------------------------------------------------------------------------##
# Aufgabentyp 2: Visualisieren von Daten

# Uebung 1
#a) Erstelle einen Plot, der die Gesamtbiomasse an jeder Station über die Zeit darstellt. Wähle eine geeignete Darstellungsform.
rkurs_data %>%
  group_by(region, date) %>%
  summarise(tot_biomass = sum(spec_biomass)) %>%
  ggplot(., aes(x = date, y = tot_biomass, group = region, colour = region)) +
  geom_point(alpha = 0.9) +
  geom_line(alpha = 0.3,size = 1) 


#b) Stelle grafisch die Gesamtbiomasse in den unterschiedlichen Regionen dar. Nutze farbige Boxplots, um Unterschiede kenntlich zu machen
rkurs_data %>%
  group_by(date,region) %>%
  summarise(sum_biomass = sum(spec_biomass))%>%
  ggplot(., aes(x= region, y = sum_biomass, fill = region) )+ #mit fill wird nach region gefaerbt. 
  geom_boxplot( color = '#2E2E2E')+ # stellt als geom boxplot ein, mit color wird die Umrandung gefaerbt
  scale_fill_manual(values = c('#190788', '#0783BD', '#F57607','#F4B004'))+ #eigene Farben
  labs(x=' ', y= expression(Biomass~'in'~mu*gCL^{-1}))+ #x, y Achsentitel
  labs(fill = 'Region')+ #aendert die Legendenbeschriftung
  theme_bw()



#------------------------------------------------------------------------------------------------------------------------#
# Uebung 2
#Erstelle einen Plot, der den zeitlichen Verlauf aller Nährstoffe an allen Stationen zeigt. Dafür sollten die Nährstoffe in separaten Fenstern (facets) dargestellt werden und die Stationen optisch voneinander getrennt sein.
#two options to melt our data using gather or summarise

summarised_data <- rkurs_data %>%
  select(-latitude, -longitude, -HAB, -spec_biomass, -genus, - toxicity)%>%
  group_by(region, date, temperature, nitrate, phosphate, silicate) %>%
  summarise() %>%
  gather(key = nutrient, value = concentration, -region, -date) 

distinct_data <- rkurs_data %>%
  group_by(region,date)%>%
  distinct(temperature, nitrate, phosphate, silicate) %>% #schmeisst duplicate raus
  gather(key = nutrient, value = concentration, -region, -date)  

#mit key uebergebe ich eine neue variable die die nutrients enthaelt
#region und date bleiben erhalten, der Rest wird der neuen Variable zugefuehrt
distinct_data%>%
  ggplot(., aes(x = date, y = concentration, group = region, colour = region)) +
  geom_point(alpha = 0.9) +
  geom_line(alpha = 0.3,size = 1) +
  facet_wrap(~nutrient, scale = 'free_y') 

#------------------------------------------------------------------------------------------------------------------------#
# Uebung 3
#Erstelle einen stacked barplot, der zeigt welche Arten im Mittel >10% zur Gesamtbiomasse 
#an jeder Station beigetragen haben - in anderen Worten: Welche Arten sind an jeder Station 
#besonders häufig/wichtig (Häufigkeit definiert durch >10% Beitrag zur Gesamtbiomasse)

data_relbio <- rkurs_data %>%
  group_by(region, date) %>%
  mutate(sum_biomass = sum(spec_biomass), 
         rel_biomass = spec_biomass/sum_biomass )%>%
  group_by(species, region)%>%
  summarise(mean_contribution = mean(rel_biomass))%>%
  filter(mean_contribution > 0.1)%>%
  arrange(region, desc(mean_contribution))

library(ggdark)
#plot
data_relbio %>%
  ggplot(.,aes(x = region, y = mean_contribution, fill = species))+
  geom_bar(stat = 'identity', col = 'black') +
  scale_y_continuous(limits = c(0.0,0.6), breaks = seq(0.0,0.6,0.2))+
  labs(y = 'relative species contribution (over 10 %)', x = '')+
  facet_wrap(~region, scales = 'free_x')+
  dark_theme_gray()+
  theme(panel.grid.major.x = element_blank(),
        panel.border = element_rect(colour = 'black', fill = NA),
        legend.text = element_text(face = 'italic'))
ggsave(plot = last_plot(), file = 'grammar_of_graphics_plot_dark.png')
#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
# PLOTS ZUM NACHBAUEN

#1
#Gesamtbiomasse pro Station
rkurs_data %>%
  group_by(region, date) %>%
  summarise(tot_biomass = sum(spec_biomass)) %>%
  group_by(region) %>%
  summarise(mean_totb = mean(tot_biomass)) %>%
  ggplot(., aes(x = region, y = mean_totb, fill = region)) +
  geom_bar(stat = 'identity') +
  labs(y = expression(mean~biomass~mu*g~C*l^-1),x = '')+
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = grey(0.8), linetype = '22'),
        panel.border = element_rect(colour = grey(0.8), fill = NA),
        axis.text.x = element_text(face = 'bold'),
        legend.position = 'none')


#2
#Wie haengen P und Richness zusammen?
rkurs_data %>%
  group_by(region, date) %>%
  summarise(richness = n(),
            mean_p = mean(phosphate))  %>%
  distinct(richness, mean_p) %>%
  ggplot(., aes(x = mean_p, y = richness)) +
  geom_point(aes(colour = region),alpha = 0.9) +
  geom_smooth(method = 'loess', linetype = 20, colour = grey(0.6),alpha = 0.5, se = F) +
  labs(x = expression(Phosphate~mu*mol~L^-1), y = 'number of species')+
  theme_classic() +
  theme(panel.grid.major.y = element_line(linetype = 20, colour = grey(0.8)),
        axis.line = element_line(colour = grey(0.5)),
        legend.position = 'top',
        legend.title = element_blank()) 

#3
#Naerstoffplot
rkurs_data$date <- as.POSIXct(x=rkurs_data$date, format='%d.%m.%y')
rkurs_data %>%
  group_by(region,date)%>%
  ggplot(., aes(x=as.Date(date), y=silicate, group=region, colour=region))+
  geom_point()+
  geom_line(linetype='dashed')+
  scale_y_continuous(breaks=c(seq(0, 20, 2)))+
  scale_x_date(labels = date_format("%d.%m"), breaks = '2 days')+
  labs(x='', y='Silicate (µg/L)', title='Silicate at different stations over time')+
  theme_bw()+
  theme(legend.position='bottom')
ggsave('Rplot3_Uebung.png',plot = last_plot(), width = 5, height = 4.6)

#------------------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------------------##
#Zusatzaufgaben


# Uebung Z4:
# plotte den Temperaturverlauf ueber die Zeit fuer die einzelnen Stationen. Dabei sollte jede Station ein einzelnes facet haben.

ggplot(data = rkurs_data, aes(x = datetime, y = temperature, group = region)) +
  scale_x_datetime(date_breaks = '2 days', date_label = '%d. %b. %y') + #date_label aendert das #Format
  stat_smooth(geom = 'line',alpha = 0.5, size = 1, method = 'loess', linetype = 1, se = F, colour = '#dd3e54') +
  geom_point() +
  facet_wrap(~region) +
  theme(panel.grid.major.y = element_line(linetype = 'dashed',colour = '#dbdbdb'),
        panel.border = element_rect(fill = NA),
        panel.background = element_blank(),
        axis.title = element_text(size = 10, colour = grey(0.28)),
        legend.title = element_text(size = 10,colour = grey(0.28)),
        legend.text = element_text(size = 9, colour = grey(0.28)),
        #legend.position = 'top',
        strip.text = element_text(face="bold",size = 10,colour = grey(0.28)),
        strip.background = element_blank(),
        axis.title.x = element_blank())


#Uebung Z5:
# Erstelle einen plot, der anzeigt, welchen relativen Anteil die HAB Arten an jeder Station zur Gesamtbiomasse beitragen.

rkurs_data %>%
  group_by(region, date) %>%
  mutate(biomass = sum(spec_biomass),
         rel_ab = spec_biomass/biomass) %>%
  filter(HAB == 1) %>%
  group_by(region, species) %>%
  summarise(mean_contr = mean(rel_ab)) %>%
  ggplot(.,aes(x = region, y = mean_contr, fill = species)) +
  #geom_bar(stat = 'identity') +
  geom_col()+
  labs(x = '',y = 'relative contribution to total biomass') +
  theme(panel.grid.major.y = element_line(linetype = 'dashed', colour = grey(0.8)),
        panel.border = element_rect(fill = NA),
        panel.background = element_blank())





