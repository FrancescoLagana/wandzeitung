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
                 legend.text = ggplot2::element_text(family = font, size = 34, 
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


# BB_plot ne gère pas bien la pie chart. Je change la fonction:
bb_pie <- function() {
  font <- "Helvetica"
  ggplot2::theme(plot.title = ggplot2::element_text(family = font, 
                                                    size = 2, face = "bold", color = "#222222"),
                 plot.subtitle = ggplot2::element_text(family = font,size = 1, margin = ggplot2::margin(0, 0, 0, 0)), plot.caption = ggplot2::element_blank(), 
                 legend.position = "bottom", legend.text.align = 0, legend.background = ggplot2::element_blank(), 
                 legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), 
                 legend.text = ggplot2::element_text(family = font, size = 22, 
                                                     color = "#222222"), axis.title = ggplot2::element_blank(), 
                 axis.text = ggplot2::element_text(family = font, size = 0, 
                                                   color = "white"), 
                 axis.text.x = ggplot2::element_text(margin = ggplot2::margin(1, b = 1)), axis.ticks = ggplot2::element_blank(), 
                 axis.line = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                 panel.grid.major.y = ggplot2::element_blank(), 
                 panel.grid.major.x = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), 
                 strip.background = ggplot2::element_rect(fill = "white"), 
                 strip.text = ggplot2::element_text(size = 12, hjust = 0))
}




# Graphique 1a fr:

df <- read.table(text = "occup besch base plus
'Hommes' 'Männer' 6266 1532
'Femmes' 'Frauen' 6266 0", header=T , sep=" ")

df <- melt(df)

p <- ggplot(df,aes(x = occup, y = value, fill = forcats::fct_rev(variable))) + 
  geom_bar(stat = "identity",width = 0.6) +
  scale_y_continuous() +
  bbc_style() +
  bb_streik() + scale_fill_manual(values = c("#a398e2", "#6752c0"))  +
  theme(legend.position = "none", legend.direction = "horizontal", legend.title = element_blank(), 
        legend.spacing.x = unit(0.30, 'cm')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(limits = levels(df$occup)) + # 7798
  geom_label(aes(x = 1, y = 3000, label = "6266 fr."), 
             hjust = 0.5, 
             vjust = 0.5, 
             colour = "white", 
             fill = "#6752c0", 
             label.size = NA, 
             family = "Helvetica", 
             size = 12) +
  geom_label(aes(x = 2, y = 7000, label = "+1532 fr."),
             hjust = 0.5, 
             vjust = 0.5, 
             colour = "black", 
             fill = "#a398e2", 
             label.size = NA, 
             family = "Helvetica", 
             size = 12) +
  labs(title = "Part expliquée et inexpliquée de la\ndifférence salariale entre femmes\net hommes, en 2016", 
       subtitle = "Secteur privé, moyenne\nRésultats sur la base de la nomenclature NOGA 2008\n") 

p


finalise_plot(p, source = "Sources: Enquête suisse sur la structure des salaires (ESS); Calculs: B,S,S.Volkswirtschaftliche Beratung AG", 
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_7/graph1a_fr.pdf", 
              width_pixels = 900, height_pixels = 800)


# Graphique 1b: piechart:
df = read.table(text = "category value n
                'Part expliquée' 0.571 875
                'Part inexpliquée' 0.429 657", header = T, sep = "")

df$category <- factor(df$category, levels = c("Part inexpliquée", "Part expliquée"), ordered = TRUE)

# Create a basic bar
pie = ggplot(df, aes(x = "", y = value, fill = category)) +
  geom_bar(stat = "identity", width = 5000) + 
  coord_polar("y", start = 0) + 
  scale_fill_manual(values =  c("#ee8704", "#fbd024")) +
  bb_pie() +
  theme( plot.caption = element_text(hjust = 1)) +
  geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]), 
                label = paste0(value*100, "%\n", "(", n, " fr.)")), size=9,
            family = "Helvetica", col="black") +
  guides(fill = guide_legend(reverse = FALSE)) + 
  theme(text = element_text(size = 12),  legend.spacing.x = unit(0.3, 'cm')) 

pie

pdf( "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_7/graph2a_fr.pdf", 
     width = 6, height = 6)
pie

dev.off()

####### Graphique 1 DE:
# Partie 1:

df <- read.table(text = "occup besch base plus
'Hommes' 'Männer' 6266 1532
'Femmes' 'Frauen' 6266 0", header = T , sep = " ")

df <- melt(df)

p <- ggplot(df,aes(x = besch, y = value, fill = forcats::fct_rev(variable))) + 
  geom_bar(stat = "identity",width = 0.6) +
  scale_y_continuous() +
  bbc_style() +
  bb_streik() + scale_fill_manual(values = c("#a398e2", "#6752c0"))  +
  theme(legend.position = "none", legend.direction = "horizontal", legend.title = element_blank(), 
        legend.spacing.x = unit(0.30, 'cm')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(limits = levels(df$besch)) + # 7798
  geom_label(aes(x = 1, y = 3000, label = "6266 Fr."), 
             hjust = 0.5, 
             vjust = 0.5, 
             colour = "white", 
             fill = "#6752c0", 
             label.size = NA, 
             family = "Helvetica", 
             size = 12) +
  geom_label(aes(x = 2, y = 7000, label = "+1532 Fr."), 
             hjust = 0.5, 
             vjust = 0.5, 
             colour = "black", 
             fill = "#a398e2", 
             label.size = NA, 
             family = "Helvetica", 
             size = 12) +
  labs(title = "Erklärter und unerklärter Anteil\ndes Lohnunterschieds zwischen\nFrauen und Männern, 2016", 
       subtitle = "Privater Sektor, Mittelwert\nErgebnisse auf Basis der Nomenklatur NOGA 2008\n") 

p

finalise_plot(p, source = "Quelle: Schweizerische Lohnstrukturerhebung (LSE); Berechnung: B,S,S.Volkswirtschaftliche Beratung AG", 
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_7/graph1a_de.pdf", 
              width_pixels = 900, height_pixels = 800)

# Graphique 1b: piechart:
df = read.table(text = "category value n
                'Erklärter Anteil' 0.571 875
                'Unerklärter Anteil' 0.429 657", header = T, sep = "")

df$category <- factor(df$category, levels = c("Unerklärter Anteil", "Erklärter Anteil"), ordered = TRUE)

# Create a basic bar
pie = ggplot(df, aes(x = "", y = value, fill = category)) +
  geom_bar(stat = "identity", width = 5000) + 
  coord_polar("y", start = 0) + 
  scale_fill_manual(values = c("#ee8704", "#fbd024")) +
  bb_pie() +
  theme( plot.caption = element_text(hjust = 1)) +
  geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]), 
                label = paste0(value*100, "%\n", "(", n, " Fr.)")), size=9,
            family = "Helvetica", col="black") +
  guides(fill = guide_legend(reverse = FALSE)) + 
  theme(text = element_text(size = 9),  legend.spacing.x = unit(0.3, 'cm')) 


pie

pdf( "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_7/graph2a_de.pdf", 
              width = 6, height = 6)
pie

dev.off()
# Graphique 2
df1 <- read.table(text = "besch occup Femmes Hommes
'Universitäre Hochschule (UNI, ETH)' 'Haute école universitaire (UNI, EPF)' 8488 10810
'Fachhochschule (FH), PH ' 'Haute école spécialisée (HES), HEP' 7533 9530
'Höhere Berufsausbildung, Fachschule ' 'Formation prof. supérieure, écoles sup. ' 7276 8661
'Matura ' 'Maturité ' 5810 6709
'Abgeschlossene Berufsausbildung ' 'Apprentissage complet (CFC)' 5476 6201
'Ohne abgeschlossene Berufsausbildung' 'Sans formation professionnelle complète ' 4337 5181", sep = " ", header=T)	

df1$diff <- df1$Hommes -df1$Femmes

df1 <- melt(df1)


df <- df1 %>% filter(df1$variable %in% c("Hommes", "Femmes", "diff")) 

df$variable <- factor(df$variable, levels = c("diff",
                                              "Hommes",
                                              "Femmes"), labels=c("Ecart salarial hommes/femmes", "Hommes", "Femmes"))

df$occup <- factor(df$occup, levels = c('Haute école universitaire (UNI, EPF)',
                                           'Haute école spécialisée (HES), HEP',
                                           'Formation prof. supérieure, écoles sup. ',
                                           'Maturité ',
                                           'Apprentissage complet (CFC)',
                                           'Sans formation professionnelle complète '), 
                   labels = c('Haute école universitaire\n(UNI, EPF)',
                              'Haute école spécialisée\n(HES), HEP',
                              'Formation prof. supérieure,\nécoles sup.',
                              'Maturité',
                              'Apprentissage\ncomplet (CFC)',
                              'Sans formation\nprofessionnelle complète')
                                          )


p <- ggplot(df, aes(x = occup, y = value,fill = variable )) +
  geom_bar( position = "dodge",stat = "identity",width = 0.9) +
  scale_y_continuous(limits = c(0, 13000), breaks = c(0, 2000, 4000, 6000, 8000, 10000, 12000),
                     labels = c("0", "2000", "4000", "6000", "8000", "10000", "12000 fr.")) +
  bb_streik() + scale_fill_manual(values = c("#ee8704", "#a398e2", "#6752c0")) +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank(),
        legend.spacing.x = unit(0.30, 'cm'), legend.key.height  = unit(0.1,"cm")) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 2)) +
  coord_flip() +
  scale_x_discrete(limits = levels(df$occup)) +
  labs(title = "Salaire mensuel brut selon la formation, en 2016",
       subtitle = "Valeur centrale (médiane), en francs - Secteur privé") +
  theme(panel.grid.major.x = element_line(color = "#cbcbcb"),
        panel.grid.major.y = element_blank())  +
  geom_text(aes(y=value, label=value, color=variable),hjust=1,
            stat='identity',position=position_dodge(0.9), size = 12)+
  scale_color_manual(values=c("black", "black", "white"),
                     breaks = c("", "", ""),
                     labels = c("", "", ""))

p

finalise_plot(p, source = "Source: Enquête suisse sur la structure des salaires (ESS)", 
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_7/graph2_fr.pdf", 
              width_pixels = 1400, height_pixels = 1200)


######## Version DE:

table(df$variable)

df$variable <- factor(df$variable,
                      levels = c("Ecart salarial hommes/femmes",
                                 "Hommes","Femmes"), 
                      labels = c("Lohngefälle Männer/Frauen","Männer",
                                          "Frauen"))

df$besch <- factor(df$besch, levels = c('Universitäre Hochschule (UNI, ETH)',
                                        'Fachhochschule (FH), PH ',
                                        'Höhere Berufsausbildung, Fachschule ',
                                        'Matura ',
                                        'Abgeschlossene Berufsausbildung ',
                                        'Ohne abgeschlossene Berufsausbildung'), 
                   labels = c('Universitäre Hochschule\n(UNI, ETH)',
                              'Fachhochschule\n(FH), PH',
                              'Höhere Berufsausbildung,\nFachschule',
                              'Matura',
                              'Abgeschlossene\nBerufsausbildung',
                              'Ohne abgeschlossene\nBerufsausbildung')
)



p <- ggplot(df, aes(x = besch, y = value,fill = variable )) +
  geom_bar( position = "dodge",stat = "identity",width = 0.9) +
  scale_y_continuous(limits = c(0, 13000), breaks = c(0, 2000, 4000, 6000, 8000, 10000, 12000),
                     labels = c("0", "2000", "4000", "6000", "8000", "10000", "12000 Fr.")) +
  bb_streik() + scale_fill_manual(values = c("#ee8704", "#a398e2", "#6752c0")) +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank(),
        legend.spacing.x = unit(0.30, 'cm'), legend.key.height  = unit(0.1,"cm")) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 2)) +
  coord_flip() +
  scale_x_discrete(limits = levels(df$besch)) +
  labs(title = "Monatlicher Bruttolohn nach Ausbildung, 2016",
       subtitle = "Zentralwert (Median), in Franken - Privater Sektor") +
  theme(panel.grid.major.x = element_line(color = "#cbcbcb"),
        panel.grid.major.y = element_blank())  +
  geom_text(aes(y=value, label=value, color=variable),hjust=1,
            stat='identity',position=position_dodge(0.9), size = 12)+
  scale_color_manual(values=c("black", "black", "white"),
                     breaks = c("", "", ""),
                     labels = c("", "", ""))

finalise_plot(p, source = "Quelle: Schweizerische Lohnstrukturerhebung (LSE)", 
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_7/graph2_de.pdf", 
              width_pixels = 1400, height_pixels = 1200)


######## Grafico 3 FR


library(readxl)


df1 <- read.table(text = "variable_de variable_fr value fac_de fac_fr
'Frauen, in %' 'Femmes, en %' 17.0001 'Tieflohn, in Franken 4335' 'Bas salaire, en francs 4335'
'Männer, in %' 'Hommes, en %' 7.6 'Tieflohn, in Franken 4335' 'Bas salaire, en francs 4335'
",  sep=" ", header=T)

df1$value <- df1$value/100

df1$variable <- factor(df1$variable_de,
                      levels = c('Frauen, in %', 'Männer, in %' ), 
                      labels = c('Frauen, in %', 'Männer, in %'))

options(digits=1)
p <- ggplot(df1, aes(x = variable, y = value, fill = variable)) + 
  geom_bar(stat = "identity",width = 0.6) +
  scale_y_continuous(limits = c(0,0.25),labels = scales::percent_format(accuracy = 1)) +
  bb_streik() + scale_fill_manual(values = c("#6752c0", "#6752c0")) +
  theme(legend.position = "none", legend.direction = "horizontal", legend.title = element_blank(), 
        legend.spacing.x = unit(0.30, 'cm')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_label(aes(x = variable, 
                 y = value, label = round(value * 100, 2)),
             hjust = 0.5, position = position_stack(vjust = .5), 
             colour = "white", fill = NA, label.size = NA, family = "Helvetica", 
             size = 12) +
  labs(title = "Arbeitnehmende mit einem Lohn unter\n4335 Fr., 2016", 
       subtitle = "Wirtschaft insgesamt")  
    #+  geom_label(aes(x = 1, y = 0.225, label = "Tieflohn, in Franken 4335"), 
    #         hjust = -0.1, 
    #        vjust = 0.5, 
    #         colour = "#222222", 
    #         fill = "white", 
    #         label.size = NA, 
    #         family = "Helvetica", 
    #         size = 12) 
p


finalise_plot(p, source = " Quelle: Schweizerische Lohnstrukturerhebung (LSE)", 
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_7/graph3_de.pdf", 
              width_pixels = 1200, height_pixels = 800)

######### FR:

df1$variable <- factor(df1$variable_fr,
                       levels = c('Femmes, en %', 'Hommes, en %'), 
                       labels = c('Femmes, en %', 'Hommes, en %'))


p <- ggplot(df1, aes(x = variable, y = value, fill=variable)) + 
  geom_bar(stat = "identity",width = 0.6) +
  scale_y_continuous(limits = c(0,0.25),labels = scales::percent_format(accuracy = 1))+
  bb_streik() + scale_fill_manual(values = c("#6752c0", "#6752c0")) +
  theme(legend.position = "none", legend.direction = "horizontal", legend.title = element_blank(), 
        legend.spacing.x = unit(0.30, 'cm')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_label(aes(x = variable, 
                 y = value, label = round(value * 100, 3)),
             hjust = 0.5, position = position_stack(vjust = .5), 
             colour = "white", fill = NA, label.size = NA, family = "Helvetica", 
             size = 12) +
  labs(title = "Personnes salariées avec un salaire inférieur\nà 4335 fr., en 2016", 
       subtitle = "Economie totale") 
  #geom_label(aes(x = 1, y = 0.225, label = "Bas salaire, en francs 4335"), 
   #          hjust = -0.1, 
    #         vjust = 0.5, 
     #        colour = "#222222", 
      #       fill = "white", 
      #   label.size = NA, 
       #      family = "Helvetica", 
        #     size = 12) 
p


finalise_plot(p, source = "Source: Enquête suisse sur la structure des salaires (ESS)",  
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_7/graph3_fr.pdf", 
              width_pixels = 1200, height_pixels = 800)

