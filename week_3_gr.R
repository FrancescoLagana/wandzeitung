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

df1 <- read.table(text = "occup besch Hommes Femmes Männer Frauen
'Personnes actives occupées à plein temps (dès 90%)' 'Vollzeiterwerbstätige (Beschäftigungsgrad ab 90%)' 82 41 82 41
'Temps partiel I (50-89%)' 'Teilzeit I (Beschäftigungsgrad 50-89%)' 11 35 11 35
'Temps partiel II (< 50%)' 'Teilzeit II (Beschäftigungsgrad < 50%)' 7 24 7 24
", header = TRUE, sep=" ")

df1 <- melt(df1)

df1$value <- df1$value/100

df <- df1 %>% filter(df1$variable %in% c("Hommes", "Femmes")) 

df$variable <- factor(df$variable, levels = c("Hommes",
                                              "Femmes"))

df$occup <- factor(df$occup, levels = c("Personnes actives occupées à plein temps (dès 90%)", 
                                        "Temps partiel I (50-89%)",
                                        "Temps partiel II (< 50%)"), ordered = TRUE,
                   labels = c("Personnes actives occupées\nà plein temps (dès 90%)", 
                              "Temps partiel I\n(50-89%)",
                              "Temps partiel II\n(< 50%)"))

df$occup  <- factor(df$occup, levels=rev(levels(df$occup)))

p <- ggplot(df, aes(x = occup, y = value,fill = variable )) + 
  geom_bar(position="dodge",stat = "identity",width = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  bb_streik() + scale_fill_manual(values = c("#a398e2", "#6752c0")) +
  geom_text(aes(y=df$value+ 0.03, label=round(value*100, 0)),stat='identity',position=position_dodge(0.6), 
            size = 9)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank(), 
        legend.spacing.x = unit(0.15, 'cm')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  scale_x_discrete(limits = levels(df$occup))+
  labs(title = "Taux d'occupation, en 2018") 
p

finalise_plot(p, source = "Source: OFS, Enquête suisse sur la population active (ESPA)", 
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_3/graph1_fr_big.pdf", 
              width_pixels = 900, height_pixels = 800)

###### DE:

df <- df1 %>% filter(df1$variable %in% c("Männer", "Frauen")) 

df$variable <- factor(df$variable, levels = c("Männer",
                                              "Frauen"))

df$besch <- factor(df$besch, levels = c('Vollzeiterwerbstätige (Beschäftigungsgrad ab 90%)',
                                        'Teilzeit I (Beschäftigungsgrad 50-89%)',
                                        'Teilzeit II (Beschäftigungsgrad < 50%)'), 
                   labels = c('Vollzeiterwerbstätige\n(Beschäftigungsgrad ab 90%)',
                              'Teilzeit I\n(Beschäftigungsgrad 50-89%)',
                              'Teilzeit II\n(Beschäftigungsgrad < 50%)'
                   )
)

df$besch  <- factor(df$besch, levels=rev(levels(df$besch)))

p <- ggplot(df, aes(x = besch, y = value,fill = variable )) + 
  geom_bar(position="dodge",stat = "identity",width = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  bb_streik() + scale_fill_manual(values = c("#a398e2", "#6752c0")) +
  geom_text(aes(y=df$value+ 0.03, label=round(value*100, 0)),stat='identity',position=position_dodge(0.6), 
            size = 9)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank(), 
        legend.spacing.x = unit(0.15, 'cm')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  scale_x_discrete(limits = levels(df$besch))+
  labs(title = "Beschäftigungsgrad, 2018") 
p

finalise_plot(p, source = "Quelle: BFS, Schweizerische Arbeitskräfteerhebung (SAKE)", 
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_3/graph1_de_big.pdf", 
              width_pixels = 900, height_pixels = 800)

#--------------------------
# GRAFICO 2
#--------------------------

df2 <- read.table(text = "occup besch Hommes Femmes Männer Frauen
'Indépendant(e)s' 'Selbständige' 15.1223757007897 10.1029417728502 15.1223757007897 10.1029417728502
'Collab. familiales/familiaux' 'Mitarb. Familienmitglieder' 1.47023489908187 2.51618285066425 1.47023489908187 2.51618285066425
'Salarié(e)s membres de la direction ' 'Arbeitnehmende in Unternehmensleitung ' 7.51194570364248 4.00311195834667 7.51194570364248 4.00311195834667
'Salarié(e)s exerçant une fonction de chef' 'Arbeitnehmende mit Vorgesetztenfunktion' 24.3494468771448 16.708623926699 24.3494468771448 16.708623926699
'Salarié(e)s sans fonction de chef' 'Arbeitnehmende ohne Vorgesetztenfunktion' 46.6309763800817 62.5537252258079 46.6309763800817 62.5537252258079
'Apprenti(e)s' 'Lehrlinge' 4.91502044325605 4.115414265632 4.91502044325605 4.115414265632", header=T, sep=" ")

df2 <- melt(df2)

df2$occup <- factor(df2$occup, 
                    levels = c('Indépendant(e)s',
                               'Collab. familiales/familiaux',
                               'Salarié(e)s membres de la direction ',
                               'Salarié(e)s exerçant une fonction de chef',
                               'Salarié(e)s sans fonction de chef',
                               'Apprenti(e)s'
                    ),
                    ordered = TRUE, 
                    labels = c('Indépendant(e)s',
                               'Collab.\nfamiliales/familiaux',
                               'Salarié(e)s\nmembres de la direction ',
                               'Salarié(e)s\nexerçant une fonction de chef',
                               'Salarié(e)s\nsans fonction de chef',
                               'Apprenti(e)s'))

df2$value <- df2$value/100

df <- df2 %>% filter(df2$variable %in% c("Hommes", "Femmes")) 

df$variable <- factor(df$variable, levels = c("Hommes",
                                              "Femmes"))


p <- ggplot(df, aes(x = occup, y = value,fill = variable )) + 
  geom_bar(position="dodge",stat = "identity",width = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  bb_streik() + scale_fill_manual(values = c("#a398e2", "#6752c0")) +
  geom_text(aes(y=df$value+ 0.02, label=round(value*100, 0)),stat='identity',position=position_dodge(0.6), 
            size = 9)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank(), 
        legend.spacing.x = unit(0.15, 'cm')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  scale_x_discrete(limits = levels(df$occup))+
  labs(title = "Situation dans la profession, en 2018") 
p

finalise_plot(p, source = "Source: OFS, Enquête suisse sur la population active (ESPA)", 
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_3/graph2_fr_big.pdf", 
              width_pixels = 900, height_pixels = 800)

####### DE

df <- df2 %>% filter(df2$variable %in% c("Männer", "Frauen")) 

df$variable <- factor(df$variable, levels = c("Männer",
                                              "Frauen"))

df$besch <- factor(df$besch, levels = c('Selbständige',
                                        'Mitarb. Familienmitglieder',
                                        'Arbeitnehmende in Unternehmensleitung ',
                                        'Arbeitnehmende mit Vorgesetztenfunktion',
                                        'Arbeitnehmende ohne Vorgesetztenfunktion',
                                        'Lehrlinge'), 
                   labels = c('Selbständige',
                              'Mitarb.\nFamilienmitglieder',
                              'Arbeitnehmende\nin Unternehmensleitung ',
                              'Arbeitnehmende\nmit Vorgesetztenfunktion',
                              'Arbeitnehmende\nohne Vorgesetztenfunktion',
                              'Lehrlinge'
                   )
)

#df$besch  <- factor(df$besch, levels=rev(levels(df$besch)))

p <- ggplot(df, aes(x = besch, y = value,fill = variable )) + 
  geom_bar(position="dodge",stat = "identity",width = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  bb_streik() + scale_fill_manual(values = c("#a398e2", "#6752c0")) +
  geom_text(aes(y=df$value+ 0.02, label=round(value*100, 0)),stat='identity',position=position_dodge(0.6), 
            size = 9)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank(), 
        legend.spacing.x = unit(0.15, 'cm')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  scale_x_discrete(limits = levels(df$besch))+
  labs(title = "Berufliche Stellung, 2018") 
p

finalise_plot(p, source = "Quelle: BFS, Schweizerische Arbeitskräfteerhebung (SAKE)", 
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_3/graph2_de_big.pdf", 
              width_pixels = 900, height_pixels = 800)

#--------------------------
# GRAFICO 3
#--------------------------

df3 <- read.table(text = "occup besch Femmes Hommes Frauen Männer
'Personnes en formation' ' Personen in Ausbildung' 26.9695070561176 43.8180179143954 26.9695070561176 43.8180179143954
'Femmes/Hommes au foyer' ' Hausfrauen/Hausmänner' 32.5321390347427 2.92976925092555 32.5321390347427 2.92976925092555
'Rentiers/Rentières' ' Rentner/Rentnerinnen' 21.3214220229754 33.060700855677 21.3214220229754 33.060700855677
'Autres personnes sans activité professionnelle' ' Andere Nichterwerbspersonen' 19.1769318861644 20.191511979002 19.1769318861644 20.191511979002", 
header=TRUE, sep = " ")

df3 <- melt(df3)

df3$value <- df3$value/100

df <- df3 %>% filter(df3$variable %in% c("Hommes", "Femmes")) 

df$variable <- factor(df$variable, levels = c("Hommes",
                                              "Femmes"))

df$occup <- factor(df$occup, 
                   levels = c('Personnes en formation',
                              'Femmes/Hommes au foyer',
                              'Rentiers/Rentières',
                              'Autres personnes sans activité professionnelle'
                   ),
                   ordered = TRUE, 
                   labels = c('Personnes en formation',
                              'Femmes/Hommes au foyer',
                              'Rentiers/Rentières',
                              'Autres personnes\nsans activité professionnelle'))

df$occup <- factor(df$occup,  levels=rev(levels(df$occup)))

p <- ggplot(df, aes(x = occup, y = value,fill = variable )) + 
  geom_bar(position="dodge",stat = "identity",width = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  bb_streik() + scale_fill_manual(values = c("#a398e2", "#6752c0")) +
  geom_text(aes(y=df$value+ 0.02, label=round(value*100, 0)),stat='identity',position=position_dodge(0.6), 
            size = 9)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank(), 
        legend.spacing.x = unit(0.15, 'cm')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  scale_x_discrete(limits = levels(df$occup)) +
  labs(title = "Personnes âgées de 15 à 64 ans sans\nactivité professionnelle selon\nle statut, en 2018") 
p

finalise_plot(p, source = "Source: OFS, Enquête suisse sur la population active (ESPA)", 
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_3/graph3_fr_big.pdf", 
              width_pixels = 900, height_pixels = 800)


#### DE:

df <- df3 %>% filter(df3$variable %in% c("Männer", "Frauen")) 

df$variable <- factor(df$variable, levels = c("Männer",
                                              "Frauen"))

df$besch <- factor(df$besch, levels = c(' Personen in Ausbildung',
                                        ' Hausfrauen/Hausmänner',
                                        ' Rentner/Rentnerinnen',
                                        ' Andere Nichterwerbspersonen'), 
                   labels = c(' Personen in Ausbildung',
                              ' Hausfrauen/Hausmänner',
                              ' Rentner/Rentnerinnen',
                              ' Andere Nichterwerbspersonen')
)

df$besch  <- factor(df$besch, levels=rev(levels(df$besch)))

p <- ggplot(df, aes(x = besch, y = value,fill = variable )) + 
  geom_bar(position="dodge",stat = "identity",width = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  bb_streik() + scale_fill_manual(values = c("#a398e2", "#6752c0")) +
  geom_text(aes(y=df$value+ 0.02, label=round(value*100, 0)),stat='identity',position=position_dodge(0.6), 
            size = 9)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank(), 
        legend.spacing.x = unit(0.15, 'cm')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  scale_x_discrete(limits = levels(df$besch))+
  labs(title = "15- bis 64-jährige Nichterwerbs-\npersonen nach Status, 2018") 
p

finalise_plot(p, source = "Quelle: BFS, Schweizerische Arbeitskräfteerhebung (SAKE)", 
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_3/graph3_de_big.pdf", 
              width_pixels = 900, height_pixels = 800)

