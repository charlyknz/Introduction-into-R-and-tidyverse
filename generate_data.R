#Berechnung von Phytoplankton-Biomasse
library(tidyverse)

setwd("~/Documents/Studium/R_Introkurs/02Data")

#Einlesen der Umweltdaten
umweltdaten <- read.csv("~/Documents/Studium/R_Introkurs/02Data/umweltdaten.csv", dec=",")
algen <- read.csv("~/Documents/Studium/R_Introkurs/02Data/algen.csv", dec=",")

#Idee: Gesamtbiomasse ist co-limitiert durch Phosphat und Nitrat. Wir schauen auf die relative Konzentration von P und N
#an jeder Station im Verhältnis zu dem maximal gemessenen Wert beider Nährstoffe. Die kleinere dieser beiden 
#relativen Konzentrationen bestimmt letztendlich die C-Konzentration.
#Biomasse-Einheit: ug C pro L
umweltdaten <- umweltdaten %>%
  mutate(rel_nut = pmin(phosphate/max(phosphate),nitrate/max(nitrate)),
         tot_biomass = abs(30*rel_nut+rnorm(1, sd = 1)))

#weitere Probleme: Diatomeen dominieren in Gebieten mit viel Silikat
#Evenness?
#HAB-species?

#Erstelle Data-Frame mit den beobachteten Arten fuer jede Station+Zeitpunkt
#die Arten werden zufaellig aus der Artenliste gezogen
sampled_species <- data.frame()

for (i in 1:length(unique(umweltdaten$richness))){
  sampled_species_new <- sample_n(algen,unique(umweltdaten$richness)[i])
  sampled_species_new <- sampled_species_new %>%
    mutate(richness = unique(umweltdaten$richness)[i]) %>%
    left_join(.,umweltdaten, by = 'richness') %>%
    arrange(date) %>%
    select(region, date, time, latitude, longitude, exposure, temperature, phosphate, nitrate, silicate, rel_nut, richness, toxicity, HAB, genus, species, tot_biomass)
  sampled_species <- rbind(sampled_species,sampled_species_new)
}

#Weise allen Arten relative Abundanzen zu
#folgende Regeln sollen gelten:
#Regel 1: bei der hoechsten Silikatkonzentration soll ca. 70% des C in Diatomeen gebunden sein. Je geringer Silikat desto proportional weniger Diatomeenbiomasse
#Regel 2: bei hoechsten Naehrstoffkonzentration (rel_nut) sind ca. 50% des C in HAB-Arten gebunden. Je geringer rel_nut desto propotional weniger HAB-Biomasse
#Regel 3: bei maximaler Naehrstoff + Silikatkonzentration sind 50% des Diatomeen-C in HAB-Arten gebunden.
#Regel 4: die Regionen unterscheiden sich natuerlicherweise in N,P,S und Temperatur auf Basis von Messdaten
#Regel 5: Gesamtbiomasse ist bestimmt durch die Co-Limitation von N und P. Der relativ gesehen geringer konzentrierte Naehrstoff bestimmt total C
#Regel 6: Biodiversitaet ist hump-shaped mit Naehrstoffverfuegbarkeit
S <- sampled_species %>%
  mutate(rel_diatom = silicate/max(silicate) * (0.7 + rnorm(1,sd = 0.02)),
         rel_hab = rel_nut/max(rel_nut) * (0.5 + rnorm(1, sd = 0.02)),
         diatom_hab = (rel_diatom/max(rel_diatom) * 0.6) * rel_diatom,
         rest_hab = rel_hab - diatom_hab,
         diatom_nhab = rel_diatom - diatom_hab,
         rest_nhab = 1 - diatom_hab - rest_hab - diatom_nhab,
         control = diatom_hab + rest_hab + diatom_nhab + rest_nhab) %>%
  arrange(date)
  

S_sub <- S %>%
  filter(date == levels(S$date)[1]) %>%
  mutate(init_distr = runif(length(region))) 

S_final <- S %>%
  mutate(distr = rep(S_sub$init_distr, times = length(levels(date)))) %>%
  arrange(region) %>%
  group_by(region, date) %>%
  mutate(distr = abs(distr + rnorm(length(distr),mean = 0,sd = 0.025))) %>%
  group_by(HAB, genus, add = T) %>%
  mutate(rel_contr = distr/sum(distr),
         spec_biomass = abs(ifelse(HAB == 1 & genus == 'diatom', rel_contr*diatom_hab*tot_biomass, 
                               ifelse(HAB == 0 & genus == 'diatom', rel_contr*diatom_nhab*tot_biomass,
                                      ifelse(HAB == 1 & genus != 'diatom', rel_contr*rest_hab*tot_biomass,rel_contr*rest_nhab*tot_biomass))))) %>%
  select(-c(rel_nut, rel_diatom, rel_hab, tot_biomass, diatom_hab, diatom_nhab, rest_hab, rest_nhab, control, rel_contr, distr, richness))
write.csv(S_final, file = 'rkurs_data.csv')

S_final %>%
  filter(date == levels(S$date)[1] & genus == 'diatom') %>%
  group_by(region) %>%
  summarise(sum_hab = sum(spec_biomass)) %>%
  ggplot(., aes(x = region, y = sum_hab))+
    geom_point()
