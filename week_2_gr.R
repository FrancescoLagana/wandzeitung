library(bbplot)
library(ggplot2)
library(reshape2)
library(RColorBrewer)


library(magrittr)
library(dplyr)

setwd("/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_2/")

df <- read.table(text="N Bfeld_de Bfeld_fr Femmes Hommes Frauen M�nner
1501 'Geisteswissenschaften und K�nste' 'Lettres et arts' 59.36043 40.63957 59.36043 40.63957
#'Sozialwissenschaften, Journalismus und Informationswesen' 'Sciences sociales, journalisme et information' 84.09091 15.90909 84.09091 15.90909
18281 'Wirtschaft, Verwaltung und Recht' 'Commerce, administration et droit' 58.30644 41.69356 58.30644 41.69356
1724 'Informations- und Kommunikationstechnologie' 'Technologie de l�information et de la communication' 5.51044 94.48956 5.51044 94.48956
#18876 'Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe' 'Ing�nierie, industries de transformation et construction' 12.18478 87.81522 12.18478 87.81522
10751 'Ingenieurwesen und Technische Berufe' 'Ing�nierie et techniques apparent�es' 6.05525 93.94475 6.05525 93.94475
2678 'Verarbeitendes Gewerbe und Bergbau' 'Industries de transformation et de traitement' 33.08439 66.91561 33.08439 66.91561
5447 'Architektur und Baugewerbe' 'Architecture et b�timent' 14.00771 85.99229 14.00771 85.99229
1971 'Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin' 'Agriculture, sylviculture, halieutique et sciences v�t�rinaires' 26.12887 73.87113 26.12887 73.87113
7226 'Gesundheit und Sozialwesen' 'Sant� et protection sociale' 91.04622 8.95378 91.04622 8.95378
#4575 'Dienstleistungen' 'Services' 59.34426 40.65574 59.34426 40.65574", sep=" ", header=T)


df$Bfeld_fr <- factor(df$Bfeld_fr, levels = df$Bfeld_fr[rev(order(df$Femmes))])


df <- melt(df, id.vars = c("N", "Bfeld_de", "Bfeld_fr"))

df$value <- df$value/100
df$pp <- df$N/ 54154
df <- data.frame(df)
df <- df %>% filter(df$variable %in% c("Hommes", "Femmes")) 

df$variable <- factor(df$variable, levels = c("Hommes",
                                    "Femmes"))


p <- ggplot(df,aes(x = Bfeld_fr, y = value,fill = variable)) + 
  geom_bar(position = "fill",stat = "identity",width = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  bbc_style() + scale_fill_manual(values = c("#a398e2", "#6752c0")) +
  geom_text(data = df, aes(x = Bfeld_fr, y = value,
                           label = round(value*100, 0)), size = 8, 
            position = position_stack(vjust = 0.5), 
            color = "white") +bbc_style() + 
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank(), 
        legend.spacing.x = unit(0.15, 'cm')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  scale_x_discrete(limits = levels(df$Bfeld_fr))+
  labs(title = "Domaines de la formation professionnelle initiale",  subtitle = "El�ves ayant d�but� le degr� secondaire II en 2015") 


p

finalise_plot(p, source = "Source: OFS, Analyses longitudinales dans le domaine de la formation", 
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_2/graph1_fr.pdf", 
              width_pixels = 1100, height_pixels = 850)


# DE:

library(bbplot)
library(ggplot2)

df <- read.table(text="N Bfeld_de Bfeld_fr Femmes Hommes Frauen M�nner
1501 'Geisteswissenschaften und K�nste' 'Lettres et arts' 59.36043 40.63957 59.36043 40.63957
#'Sozialwissenschaften, Journalismus und Informationswesen' 'Sciences sociales, journalisme et information' 84.09091 15.90909 84.09091 15.90909
18281 'Wirtschaft, Verwaltung und Recht' 'Commerce, administration et droit' 58.30644 41.69356 58.30644 41.69356
1724 'Informations- und Kommunikationstechnologie' 'Technologie de l�information et de la communication' 5.51044 94.48956 5.51044 94.48956
#18876 'Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe' 'Ing�nierie, industries de transformation et construction' 12.18478 87.81522 12.18478 87.81522
10751 'Ingenieurwesen und Technische Berufe' 'Ing�nierie et techniques apparent�es' 6.05525 93.94475 6.05525 93.94475
2678 'Verarbeitendes Gewerbe und Bergbau' 'Industries de transformation et de traitement' 33.08439 66.91561 33.08439 66.91561
5447 'Architektur und Baugewerbe' 'Architecture et b�timent' 14.00771 85.99229 14.00771 85.99229
1971 'Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin' 'Agriculture, sylviculture, halieutique et sciences v�t�rinaires' 26.12887 73.87113 26.12887 73.87113
7226 'Gesundheit und Sozialwesen' 'Sant� et protection sociale' 91.04622 8.95378 91.04622 8.95378
#4575 'Dienstleistungen' 'Services' 59.34426 40.65574 59.34426 40.65574", sep=" ", header=T)


df$Bfeld_de <- factor(df$Bfeld_de, levels = df$Bfeld_de[rev(order(df$Femmes))])


df <- melt(df, id.vars = c("N", "Bfeld_de", "Bfeld_fr"))

df$value <- df$value/100
df$pp <- df$N/ 54154
df <- data.frame(df)
df <- df %>% filter(df$variable %in% c("M�nner", "Frauen")) 

df$variable <- factor(df$variable, levels = c(
                                              "M�nner", "Frauen"))


p <- ggplot(df,aes(x = Bfeld_de, y = value,fill = variable)) + 
  geom_bar(position = "fill",stat = "identity",width = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  bbc_style() + scale_fill_manual(values = c("#a398e2", "#6752c0")) +
  geom_text(data = df, aes(x = Bfeld_de, y = value,
                           label = round(value*100, 0)), size = 8, 
            position = position_stack(vjust = 0.5), 
            color = "white") +bbc_style() + 
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank(), 
        legend.spacing.x = unit(0.15, 'cm')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  scale_x_discrete(limits = levels(df$Bfeld_de))+
  labs(title = "Ausbildungsfelder der beruflichen Grundbildung",  subtitle = "Lernenden, die im 2015 in der Sekund�rstufe II eingetreten sind") 


p

finalise_plot(p, source = "Quelle: BFS, L�ngsschnittanalysen im Bildungsbereich", 
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_2/graph1_de.pdf", 
              width_pixels = 1100, height_pixels = 850)




#############


####### Graphique 3:

s2 <- read.table(text = "degre sex percentage
'Ecole obligatoire' Femmes 75.0
'Ecole obligatoire' Hommes 25.0
'Degr� secondaire II' Femmes 56.0
'Degr� secondaire II' Hommes 44.0
'Degr� tertiaire' Femmes 34
'Degr� tertiaire' Hommes 66
", sep=" ", header=T)


s2$percentage <- s2$percentage/100

s2$sex <- factor(s2$sex, levels = c("Hommes",
                                    "Femmes"))

s2$degre <- factor(s2$degre, levels = c("Ecole obligatoire",
                                        "Degr� secondaire II",
                                        "Degr� tertiaire"), 
                   labels = c("Ecole\nobligatoire",
                              "Degr�\nsecondaire II",
                              "Degr�\ntertiaire"))


#df3$colr <- ifelse(df3$variable == "Hommes", "black", "white")
p1 <- ggplot(s2,aes(x = degre, y = percentage,fill = sex)) + 
  geom_bar(position = "fill",stat = "identity",width = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  bbc_style() + scale_fill_manual(values = c("#a398e2", "#6752c0")) +
  geom_text(data = s2, aes(x = degre, y = percentage,
                           label = round(percentage*100, 0)), size = 8, 
            position = position_stack(vjust = 0.5), 
            color = "white") +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank(), 
        legend.spacing.x = unit(0.15, 'cm')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Corps enseignant en 2016/2017") +coord_flip() +
  scale_x_discrete(limits = rev(levels(s2$degre)))

p1

finalise_plot(p1, source = "Source: OFS, Statistiques du personnel des �coles et des hautes �coles",
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_2/graph3_fr.pdf", 
              width_pixels = 790, height_pixels = 500)





####### Graphique 3: DEUTSCH


s2$sex_de <- c("Frauen", "M�nner", "Frauen", "M�nner","Frauen", "M�nner")

s2$sex_de <- factor(s2$sex_de, levels = c("M�nner",
                                          "Frauen"))

s2$sex <- s2$sex_de

s2$degre_de <- c("Obligatorische Schule", "Obligatorische Schule", "Sekundarstufe II", "Sekundarstufe II", "Terti�rstufe", "Terti�rstufe")
s2$degre_de <- factor(s2$degre_de, levels = c("Obligatorische Schule",
                                              "Sekundarstufe II",
                                              "Terti�rstufe"), 
                      labels = c("Obliagtorische\nSchule",
                                 "Sekundarstufe II",
                                 "Terti�rstufe"))


#s2$degre_de <- factor(s2$degre_de)
s2$degre <- s2$degre_de




#df3$colr <- ifelse(df3$variable == "Hommes", "black", "white")
p1 <- ggplot(s2,aes(x = degre, y = percentage,fill = sex)) + 
  geom_bar(position = "fill",stat = "identity",width = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  bbc_style() + scale_fill_manual(values = c("#a398e2", "#6752c0")) +
  geom_text(data = s2, aes(x = degre, y = percentage,
                             label = round(percentage*100, 0)), size = 8, 
            position = position_stack(vjust = 0.5), 
            color = "white") +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank(), 
        legend.spacing.x = unit(0.15, 'cm')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Lehrkr�fte im 2016/2017") +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(s2$degre)))

p1

finalise_plot(p1, source = "Quelle: BFS, Statistiken zum Schul- und Hochschulpersonal",
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_2/graph3_de.pdf", 
              width_pixels = 790, height_pixels = 500)


############## Gr 3:
library(ggpubr)
library(cowplot)
dfr <- read.table(text = "Mesure_fr Niv_fr Frauen M�nner Mesure_de Niv_de
'Taux de premi�re certification du degr� secondaire II' 'Formation professionnelle initiale' 61.2 69.0 'Erstabschl�sse auf Sekundarstufe II1' 'Berufliche Grundbildung'
'Taux de premi�re certification du degr� secondaire II' 'Formation g�n�rale' 32.4 20.1 'Erstabschl�sse auf Sekundarstufe II1' 'Allgemeinbildung'
'Taux de maturit�s, 20161' 'Maturit�s gymnasiales' 25.1 17.5 'Maturit�tsquote1' 'Gymnasiale Maturit�ten'
'Taux de maturit�s, 20161' 'Maturit�s professionnelles' 14.8 16.0 'Maturit�tsquote1' 'Berufsmaturit�ten'
'Taux de maturit�s, 20161' 'Maturit�s sp�cialis�es' 5.0 1.1 'Maturit�tsquote1' 'Fachmaturit�ten'
'Taux de dipl�mes au degr� tertiaire' 'Hautes �coles sp�cialis�es et p�dagogiques' 18.5 13.8 'Abschlussquoten auf Terti�rstufe2' 'Fach- und p�dagogische Hochschulen'
'Taux de dipl�mes au degr� tertiaire' 'Hautes �coles universitaires' 15.1 12.9 'Abschlussquoten auf Terti�rstufe2' 'Universit�re Hochschulen'
", sep= " " ,header = T)


df2 <- melt(dfr)

# CHanger pour DE
df2$variable <- ifelse(df2$variable == "M�nner", "Hommes", "Femmes")
df2$variable <- factor(df2$variable, levels = c("Hommes", "Femmes"), ordered = TRUE)

library(dplyr)
df <- df2 %>% filter(Mesure_fr == "Taux de maturit�s, 20161")


df$Niv_fr <- factor(df$Niv_fr, levels = c("Maturit�s gymnasiales", "Maturit�s professionnelles", "Maturit�s sp�cialis�es"), ordered = TRUE)
df$value <- df$value/100



a <- ggplot(data = df, mapping = aes(x = variable, y = value, fill = Niv_fr,
                                     label = round(value*100, 0)))+ 
  geom_bar( stat="identity") +
  scale_fill_manual(values = c("#a398e2", "#6752c0", "#2b225d")) +
  bbc_style() +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) + xlab("") + 
  coord_flip() + ylab("") + labs(subtitle = "")+
  theme(legend.position = "right", legend.direction = "vertical") + 
  guides(col = guide_legend(nrow  = 2)) +
  geom_text(position = position_stack(vjust = 0.5), size = 8, col="white") 

a

a <- ggdraw(a) + draw_text("Taux de maturit�s (2)", 
                           x = 0.00, y = 0.98, hjust = 0, vjust = 1,
                           size = 22, family="Helvetica") 

df <- df2 %>% filter(Mesure_fr == "Taux de premi�re certification du degr� secondaire II")
df$value <- df$value/100
df$Niv_fr <- factor(df$Niv_fr, labels = c("Formation\ng�n�rale", 
                                          "Formation professionnelle\ninitiale"))
b <- ggplot(data = df, mapping = aes(x = variable, y = value, fill = Niv_fr, label = round(value*100, 0))) + 
  geom_bar( stat="identity") +
  scale_fill_manual(values = c("#a398e2", "#6752c0"))+
  bbc_style() +
  theme(legend.position = "right", legend.direction = "vertical") + 
  guides(col = guide_legend(nrow  = 2))+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) + xlab("") + 
  coord_flip() + ylab("") + labs(subtitle = "")+
  geom_text(position = position_stack(vjust = 0.5), size = 8, col="white") 

b

b <- ggdraw(b) + draw_text("Taux de premi�re certification du degr� secondaire II (1)", 
                           x = 0.00, y = 0.98, hjust = 0, vjust = 1,
                           size = 22, family="Helvetica") 

df <-  df2 %>% filter(Mesure_fr == "Taux de dipl�mes au degr� tertiaire")
df$Niv_fr <- factor(df$Niv_fr, labels = c("Hautes �coles\nsp�cialis�es et p�dagogiques", 
                                          "Hautes �coles\nuniversitaires"))

df$value <- df$value/100
c <- ggplot(data = df, mapping = aes(x = variable, y = value, fill = Niv_fr, label = round(value*100, 0))) + 
  geom_bar( stat="identity")+
  scale_fill_manual(values = c( "#a398e2", "#6752c0"))+
  bbc_style() +
  theme(legend.position = "right", legend.direction = "vertical") + 
  guides(col = guide_legend(nrow  = 2))+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) + xlab("") + 
  coord_flip() + ylab("") + labs(subtitle = "")+
  geom_text(position = position_stack(vjust = 0.5), size = 8, col="white") 

c

c <- ggdraw(c) + draw_text("Taux de dipl�mes du degr� tertiaire (3)", 
                           x = 0.00, y = 0.98, hjust = 0, vjust = 1,
                           size = 22, family="Helvetica") 


library(bbplot)
library(ggpubr)
fina <- ggarrange( b, a, c, nrow  = 3) + bbc_style() +
  labs(title = "Taux de dipl�mes en 2016", subtitle = "") +
  theme(axis.text = element_text(color = "white"))    +
  theme(plot.title = element_text(hjust = 0, vjust=0)) +
  theme(plot.subtitle = element_text(size = 3))




finalise_plot(fina, source = "Sources: OFS, (1, 2) Analyses longitudinales dans le domaine de la formation; (3) Etudiants et examens finals des hautes �coles", 
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_2/graph1ee_fr.pdf", 
              width_pixels = 950, height_pixels = 650)

########3 DEUTSCH:


############## Gr 3:

dfr <- read.table(text = "Mesure_fr Niv_fr Frauen M�nner Mesure_de Niv_de
'Taux de premi�re certification du degr� secondaire II' 'Formation professionnelle initiale' 61.2 69.0 'Erstabschl�sse auf Sekundarstufe II1' 'Berufliche Grundbildung'
'Taux de premi�re certification du degr� secondaire II' 'Formation g�n�rale' 32.4 20.1 'Erstabschl�sse auf Sekundarstufe II1' 'Allgemeinbildung'
'Taux de maturit�s, 20161' 'Maturit�s gymnasiales' 25.1 17.5 'Maturit�tsquote1' 'Gymnasiale Maturit�ten'
'Taux de maturit�s, 20161' 'Maturit�s professionnelles' 14.8 16.0 'Maturit�tsquote1' 'Berufsmaturit�ten'
'Taux de maturit�s, 20161' 'Maturit�s sp�cialis�es' 5.0 1.1 'Maturit�tsquote1' 'Fachmaturit�ten'
'Taux de dipl�mes au degr� tertiaire' 'Hautes �coles sp�cialis�es et p�dagogiques' 18.5 13.8 'Abschlussquoten auf Terti�rstufe2' 'Fach- und p�dagogischen Hochschulen'
'Taux de dipl�mes au degr� tertiaire' 'Hautes �coles universitaires' 15.1 12.9 'Abschlussquoten auf Terti�rstufe2' 'Universit�re Hochschulen'
", sep= " " ,header = T)


df2 <- melt(dfr)



library(dplyr)
df <- df2 %>% filter(Mesure_fr == "Taux de maturit�s, 20161")

df$variable <- factor(df$variable, levels=c( "M�nner", "Frauen"), ordered = TRUE)

df$Niv_de <- factor(df$Niv_de, levels = c("Gymnasiale Maturit�ten", "Berufsmaturit�ten", "Fachmaturit�ten"), ordered = TRUE)
df$value <- df$value/100



  a <- ggplot(data = df, mapping = aes(x = variable, y = value, fill = Niv_de,
                                     label = round(value*100, 0)))+ 
  geom_bar( stat="identity") +
  scale_fill_manual(values = c("#a398e2", "#6752c0", "#2b225d")) +
  bbc_style() +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) + xlab("") + 
  coord_flip() + ylab("") + 
  labs(subtitle = "")+
  theme(legend.position = "right", legend.direction = "vertical") + 
  guides(col = guide_legend(nrow  = 2)) +
  geom_text(position = position_stack(vjust = 0.5), size = 8, col="white") 

a <- ggdraw(a) + draw_text("Maturit�tsquote (2)", 
                           x = 0.00, y = 0.98, hjust = 0, vjust = 1,
                           size = 22, family="Helvetica") 



df <- df2 %>% filter(Mesure_fr == "Taux de premi�re certification du degr� secondaire II")
df$value <- df$value/100
df$Niv_de <- factor(df$Niv_de, labels = c("Allgemeinbildung", 
                                          " Berufliche Grundbildung"))
df$variable <- factor(df$variable, levels=c( "M�nner", "Frauen"), ordered = TRUE)

b <- ggplot(data = df, mapping = aes(x = variable, y = value, fill = Niv_de, label = round(value*100, 0))) + 
  geom_bar( stat="identity") +
  scale_fill_manual(values = c("#a398e2", "#6752c0"))+
  bbc_style() +
  theme(legend.position = "right", legend.direction = "vertical") + 
  guides(col = guide_legend(nrow  = 2))+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) + xlab("") + 
  coord_flip() + ylab("") + 
  labs(subtitle = "")+
  geom_text(position = position_stack(vjust = 0.5), size = 8, col="white") 
   
b <- ggdraw(b) + draw_text("Erstabschl�sse auf Sekundarstufe II (1)", 
                      x = 0.00, y = 0.98, hjust = 0, vjust = 1,
                      size = 22, family="Helvetica") 

df <-  df2 %>% filter(Mesure_fr == "Taux de dipl�mes au degr� tertiaire")
df$Niv_de <- factor(df$Niv_de, labels = c("Fach- und p�dagogische\nHochschulen", 
                                           "Universit�re\nHochschulen"))

df$variable <- factor(df$variable, levels=c( "M�nner", "Frauen"), ordered = TRUE)

df$value <- df$value/100
c <- ggplot(data = df, mapping = aes(x = variable, y = value, fill = Niv_de, 
                                      label = round(value*100, 0))) + 
  geom_bar( stat="identity")+
  scale_fill_manual(values = c( "#a398e2", "#6752c0"))+
  bbc_style() +
  guides(col = guide_legend(nrow  = 2))+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) + xlab("") + 
  coord_flip() + ylab("") +
  labs(subtitle = "")+
  geom_text(position = position_stack(vjust = 0.5), size = 8, col="white") +
  theme(legend.position = "right", legend.direction = "vertical") 
  

c

c <- ggdraw(c) + draw_text("Abschlussquoten auf Terti�rstufe (3)", 
                      x = 0.00, y = 0.98, hjust = 0, vjust = 1,
                      size = 22, family="Helvetica") 



fina <- ggarrange( b, a, c, nrow  = 3) + bbc_style() +
  labs(title = "Abschlussquoten 2016", subtitle = "") +
  theme(axis.text = element_text(color = "white"))    +
  theme(plot.title = element_text(hjust = 0, vjust=0)) +
  theme(plot.subtitle = element_text(size = 3))


finalise_plot(fina, source = "Quellen: BFS, (1, 2) L�ngsschnittanalysen im Bildungsbereich; (3) Studierende und Abschl�sse der Hochschulen", 
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_2/graph1ee_de.pdf", 
              width_pixels = 950, height_pixels = 650)
