library(reshape2)
library(ggplot2)
library(bbplot)
library(magrittr)
library(dplyr)

bb_streik <- function(){
  font <- "Helvetica"
  ggplot2::theme(plot.title = ggplot2::element_text(family = font, 
                                                    size = 50, face = "bold", color = "#222222"), 
                 plot.subtitle = ggplot2::element_text(family = font, 
                                                       size = 34, margin = ggplot2::margin(9, 0, 9, 0)), plot.caption = ggplot2::element_blank(), 
                 legend.position = "top", legend.text.align = 0, legend.background = ggplot2::element_blank(), 
                 legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), 
                 legend.text = ggplot2::element_text(family = font, size = 30, 
                                                     color = "#222222"), axis.title = ggplot2::element_blank(), 
                 axis.text = ggplot2::element_text(family = font, size = 30, 
                                                   color = "#222222"), axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, 
                                                                                                                                    b = 10)), axis.ticks = ggplot2::element_blank(), 
                 axis.line = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                 panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"), 
                 panel.grid.major.x = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), 
                 strip.background = ggplot2::element_rect(fill = "white"), 
                 strip.text = ggplot2::element_text(size = 30, hjust = 0))
}

df1 <- read.table(text = "occup besch Femmes Hommes Frauen Männer
'Partenaires dans un ménage de deux personnes' 'Partner/in in 2-Personenhaushalten' 21.7 15.7 21.7 15.7
'Mères, pères avec partenaire et enfant(s) (le plus jeune 0-14 ans)' 'Mütter, Väter mit Partner/in und jüngstem Kind 0-14 jährig' 52.8 29.2 52.8 29.2
", header = TRUE, sep = " ")

df1 <- melt(df1)


df <- df1 %>% filter(df1$variable %in% c("Hommes", "Femmes")) 

df$variable <- factor(df$variable, levels = c("Hommes",
                                              "Femmes"))

df$occup <- factor(df$occup, levels = c('Partenaires dans un ménage de deux personnes',
                                        'Mères, pères avec partenaire et enfant(s) (le plus jeune 0-14 ans)'), ordered = TRUE,
                   labels = c('Partenaires dans un ménage\nde deux personnes',
                              'Mères, pères avec partenaire\net enfant(s) (le plus jeune 0-14 ans)'))

df$occup  <- factor(df$occup, levels = rev(levels(df$occup)))

p <- ggplot(df, aes(x = occup, y = value,fill = variable )) + 
  geom_bar(position="dodge",stat = "identity",width = 0.6) +
  scale_y_continuous(breaks=c(0, 10,20,30,40, 50, 60)) +
  bb_streik() + scale_fill_manual(values = c("#a398e2", "#6752c0")) +
  geom_text(aes(y=df$value+ 1.7, label=round(value, 0)),stat='identity',position=position_dodge(0.6), 
            size = 12)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank(), 
        legend.spacing.x = unit(0.30, 'cm')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  scale_x_discrete(limits = levels(df$occup))+
  labs(title = "Nombre d'heures consacrées en moyenne\npar semaine au travail domestique et familial,\nen 2016") 
p

finalise_plot(p, source = "Source: OFS, Enquête suisse sur la population active (ESPA)", 
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_4/graph1_fr.pdf", 
              width_pixels = 1200, height_pixels = 800)


######## Version DE:

df <- df1 %>% filter(df1$variable %in% c("Frauen", "Männer")) 

df$variable <- factor(df$variable, levels = c("Männer",
                                              "Frauen"))

df$besch <- factor(df$besch, levels = c('Partner/in in 2-Personenhaushalten',
                                        'Mütter, Väter mit Partner/in und jüngstem Kind 0-14 jährig'), ordered = TRUE,
                   labels = c('Partner/in\nin 2-Personenhaushalten',
                              'Mütter, Väter mit Partner/in\nund jüngstem Kind 0-14 jährig'))

df$besch  <- factor(df$besch, levels=rev(levels(df$besch)))

p <- ggplot(df, aes(x = besch, y = value,fill = variable )) + 
  geom_bar(position="dodge",stat = "identity",width = 0.6) +
  scale_y_continuous(breaks=c(0, 10,20,30,40, 50, 60)) +
  bb_streik() + scale_fill_manual(values = c("#a398e2", "#6752c0")) +
  geom_text(aes(y=df$value + 1.5, label=round(value, 0)),stat='identity',position=position_dodge(0.6), 
            size = 12)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank(), 
        legend.spacing.x = unit(0.30, 'cm')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  scale_x_discrete(limits = levels(df$besch))+
  labs(title = "Durchschnittlicher wöchentlicher Zeitaufwand für\nHaus- und Familienarbeit, 2016") 
p

finalise_plot(p, source = "Quelle: BFS, Schweizerische Arbeitskräfteerhebung (SAKE)", 
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_4/graph1_de.pdf", 
              width_pixels = 1200, height_pixels = 800)


######## Grafico 2

library(readxl)

df1 <- read.table(text = '"occup";"besch";"Couples sans enfant";"Couples avec enfant(s) de moins de 25 ans";"Tous les ménages de couples";"Paare ohne Kind";"Paare mit Kind(ern) unter 25 Jahren";"Paarhaushalte insgesamt"
"Accomplies principalement par la femme";"hauptsächlich von der Frau erledigt";42.846;73.847;63.0983;42.846;73.847;63.0983
"Accomplies par les deux partenaires";"von beiden Partnern erledigt";49.0785;20.9543;30.6013;49.0785;20.9543;30.6013
"Accomplies principalement par lhomme";"hauptsächlich vom Mann erledigt";6.6543;3.2947;4.5038;6.6543;3.2947;4.5038
"Autres";"anderes";1.4212;1.9039;1.7965;1.4212;1.9039;1.7965',  sep=";", header=T)

df1
df1 <- melt(df1)

df1$value <- df1$value/100

df <- df1 %>% filter(df1$variable %in% c( "Couples sans enfant",
                                          "Couples avec enfant(s) de moins de 25 ans",
                                          "Tous les ménages de couples")) 

df$variable <- factor(df$variable, 
                      levels = c("Couples sans enfant",
                                 "Couples avec enfant(s) de moins de 25 ans",
                                 "Tous les ménages de couples"), 
                      
                      labels = c("Couples sans enfant",
                                 "Couples avec enfant(s)\nde moins de 25 ans",
                                 "Tous les ménages\nde couples"), 
                      ordered = TRUE)

df$occup <- factor(df$occup, levels = c( "Accomplies par les deux partenaires",
                                         "Accomplies principalement par la femme",
                                         "Accomplies principalement par l'homme",
                                         "Autres"),
                   ordered = TRUE,
                  labels = c("Accomplies\npar les\ndeux partenaires",
                             "Accomplies\nprincipalement\npar la femme",
                            "Accomplies\nprincipalement\npar l'homme",
                            "Autres"))


df <- df %>% arrange(occup)

p <- ggplot(df,aes(x = variable, y = value, fill = forcats::fct_rev(occup))) + 
  geom_bar(position = "fill",stat = "identity",width = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  bbc_style() + scale_fill_manual(values = c("#d6ccff",  "#a398e2", "#6752c0",  "#2b225d")) +
  geom_text(data = df, aes(x = variable, y = value,
                           label = round(value*100, 0)), size = 12, 
            position = position_stack(vjust = 0.5), 
            color = "white") + bb_streik() + 
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank(), 
        legend.spacing.x = unit(0.30, 'cm')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  scale_x_discrete(limits = levels(df$variable)) +
  labs(title = "Répartition des tâches domestiques et familiales\ndans les ménages de couples, âge des deux\npartenaires entre 25 et 54 ans, en 2013") 

p

finalise_plot(p, source = " Source: OFS, Enquête sur les familles et les générations (EFG)", 
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_4/graph2_fr.pdf", 
              width_pixels = 1200, height_pixels = 800)

######### DE:

df <- df1 %>% filter(df1$variable %in% c( "Paare ohne Kind",
                                          "Paare mit Kind(ern) unter 25 Jahren",
                                          "Paarhaushalte insgesamt")) 

df$variable <- factor(df$variable,
                      levels = c("Paare ohne Kind",
                                              "Paare mit Kind(ern) unter 25 Jahren",
                                              "Paarhaushalte insgesamt"),
                      labels = c("Paare ohne Kind",
                                          "Paare mit Kind(ern)\nunter 25 Jahren",
                                          "Paarhaushalte insgesamt"),
                      ordered = TRUE)

df$besch <- factor(df$besch, levels = c("von beiden Partnern erledigt", 
                                        "hauptsächlich von der Frau erledigt",
                                         "hauptsächlich vom Mann erledigt",
                                         "anderes"),
                   ordered = TRUE,
                   labels = c("von beiden\nPartnern\nerledigt", 
                              "hauptsächlich\nvon der Frau\nerledigt",
                              "hauptsächlich\nvom Mann\nerledigt",
                              "anderes"))


df <- df %>% arrange(occup)

p <- ggplot(df,aes(x = variable, y = value, fill = forcats::fct_rev(besch))) + 
  geom_bar(position = "fill",stat = "identity",width = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  bbc_style() + scale_fill_manual(values = c("#d6ccff",  "#a398e2", "#6752c0",  "#2b225d")) +
  geom_text(data = df, aes(x = variable, y = value,
                           label = round(value*100, 0)), size = 12, 
            position = position_stack(vjust = 0.5), 
            color = "white") + bb_streik() + 
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank(), 
        legend.spacing.x = unit(0.30, 'cm')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  scale_x_discrete(limits = levels(df$variable)) +
  labs(title = "Aufteilung der Haus- und Familienarbeit in\nPaarhaushalten, beide Partner im Alter von 25 bis\n64 Jahren, 2013") 

p

finalise_plot(p, source = "Quelle: BFS, Erhebung zu Familien und Generationen (EFG)", 
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_4/graph2_de.pdf", 
              width_pixels = 1200, height_pixels = 800)



########### Grafico 3:

library(readxl)

df1 <- read.table(text = '"occup";"besch";"Femmes";"Hommes";"Autres personnes";"Frauen";"Männer";"andere Person(en)"
"Couples sans enfant dans le ménage";"Paare ohne Kind im Haushalt";39.515;60.485;0;39.515;60.485;0
"Couples avec enfant(s) dans le ménage";"Paare mit Kind(ern)";24.504;68.525;6.971;24.504;68.525;6.971', header=T, sep=";")

df1 <- melt(df1)

df1$value <- df1$value/100

df <- df1 %>% filter(df1$variable %in% c( "Femmes", "Autres personnes", "Hommes")) 

df$variable <- factor(df$variable, levels = c("Autres personnes", "Hommes", "Femmes"
                                              ), ordered = TRUE)

df$occup <- factor(df$occup, levels = c('Couples sans enfant dans le ménage',
                                        'Couples avec enfant(s) dans le ménage'), ordered = TRUE,
                   labels = c('Couples sans enfant\ndans le ménage',
                              'Couples avec enfant(s)\ndans le ménage')
                   )


p <- ggplot(df,aes(x = occup, y = value, fill = variable)) + 
  geom_bar(position = "fill",stat = "identity",width = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  bbc_style() + scale_fill_manual(values = c( "#d6ccff", "#a398e2", "#6752c0")) +
  geom_text(data = df, aes(x = occup, y = value,
                           label = ifelse(value <= 0.01, paste(""), round(value*100, 0))), 
            size = 12, 
            position = position_stack(vjust = 0.5), 
            color = "white") + bb_streik() + 
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank(), 
        legend.spacing.x = unit(0.30, 'cm')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  scale_x_discrete(limits = levels(df$occup))+
  labs(title = "Contribution individuelle au revenu\ndu travail du ménage, en 2016") 


finalise_plot(p, source = "Source: OFS, Enquête suisse sur la population active (ESPA)", 
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_4/graph3_fr.pdf", 
              width_pixels = 1100, height_pixels = 700)


#############3  DE:


df <- df1 %>% filter(df1$variable %in% c("Frauen" , "Männer" , "andere Person(en)")) 

df$variable <- factor(df$variable, levels = c("andere Person(en)", "Männer", "Frauen"), ordered = TRUE)

df$besch <- factor(df$besch, levels = c("Paare ohne Kind im Haushalt",
                                        "Paare mit Kind(ern)"), ordered = TRUE,
                   labels = c("Paare ohne Kind\nim Haushalt",
                              "Paare mit\nKind(ern)")
)


p <- ggplot(df,aes(x = besch, y = value, fill = variable)) + 
  geom_bar(position = "fill",stat = "identity",width = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  bbc_style() + scale_fill_manual(values = c( "#d6ccff", "#a398e2", "#6752c0")) +
  geom_text(data = df, aes(x = besch, y = value,
                           label = ifelse(value <= 0.01, paste(""), round(value*100, 0))),
            size = 12, 
            position = position_stack(vjust = 0.5), 
            color = "white") + bb_streik() + 
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank(), 
        legend.spacing.x = unit(0.30, 'cm')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  scale_x_discrete(limits = levels(df$besch)) +
  labs(title = "Individueller Beitrag am Arbeits-\neinkommen des Haushaltes, 2016") 

p

finalise_plot(p, source = "Quelle: BFS, Erhebung über die Einkommen und die Lebensbedingungen, SILC-2016 Version 24.01.2018", 
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_4/graph3_de.pdf", 
              width_pixels = 1100, height_pixels = 700)
