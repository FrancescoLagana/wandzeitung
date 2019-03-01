# Load libraires:
library(readxl)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(reshape2)
library(magrittr)
library(bbplot)
library(ggalt)
library(tidyr)

# WDir pour les données:
lin <- setwd("~/Dropbox/Francesco/F. Lavori/Infografica/Dati/")
lin

# Stocker les graphiques
graf <- "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/Grafici/week_1/"

##### Graphique 1:
df2 <- read_excel("week_1.xlsx", 
    sheet = "graph_1b")
df2 <- df2 %>% filter(Fonction != "Total")
df3 <- melt(df2, id.vars = c("Haute Ecole", "Fonction"))
df3$value <- df3$value/100

df <- df3 %>% filter(`Haute Ecole` %in% c("HEU", "EO"), 
                     Fonction %in% c("Enseignantes avec responsabilité de direction", 
                                     "Degré primaire 1-2"))
df$Fonction <- factor(df$Fonction, 
                      labels =
                        c("Enseignantes du degré primaire (école enfantine et cycle élémentaire)", 
                          "Enseignantes des HEU avec responsabilité de direction"
                          )
                      )

p1 <- ggplot(df, aes(x = variable, y = value, group = factor(Fonction), 
    color = factor(Fonction))) + geom_line(stat = "identity", 
    size = 1.5) + scale_color_manual(values = c("darksalmon", "darkslateblue")) +
    theme(legend.position = "bottom", legend.direction = "vertical") + 
    guides(col = guide_legend(ncol = 1)) + scale_x_discrete(breaks = c(seq(from = 2003, to = 2016, by = 2))) + xlab("") + ylab("") + 
    bbc_style() + theme(legend.title = element_blank()) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
    limits = c(0, 1)) + geom_hline(yintercept = 0, size = 1, 
    colour = "#333333") + labs(title = "Proportion d'enseignantes au degré primaire\net dans les hautes écoles universitaires (HEU)", 
    subtitle = "Ecoles publiques") + theme(legend.position = "bottom", legend.justification = "left") + 
    guides(fill = guide_legend(reverse = TRUE)) 


finalise_plot(p1, source = "OFS, Statistiques du personnel des écoles et des hautes écoles",
              paste0(graf, "/graph_1_fr.pdf"), 
              width_pixels = 660, height_pixels = 500)


##### Graphique 2:

te <- read_excel("week_1.xlsx", sheet = "graph_3")

te$Dirig <- te$Dirig/100
# Prepare data
dumbbell_df <- te %>% spread(Sexe, Dirig) %>% mutate(gap = Homme - 
    Femme) %>% arrange(desc(gap))

# Make plot
te
pku <- ggplot(dumbbell_df, aes(y = `Etat parental`)) + geom_point(data = te, 
    aes(x = Dirig, color = Sexe)) + geom_dumbbell(aes(x = Femme, 
    xend = Homme), size = 5, color = "#e3e2e1", colour_x = "#6752c0", 
    colour_xend = "#a398e2", dot_guide = TRUE, dot_guide_size = 0.25) + 
    geom_hline(yintercept = 0, size = 1, colour = "#333333") + 
    scale_color_manual(name = "", values = c("#6752c0", "#a398e2"), 
        labels = c("Femmes", "Hommes")) + 
  scale_shape_manual(values=c(3, 16)) + bbc_style() +
    scale_x_continuous(limits = c(0.2, 0.65), labels = scales::percent_format(accuracy = 1)) + 
    labs(title = "Écarts de taux de cadres entre femmes\net hommes selon le statut parental", 
        subtitle = "Situation cinq ans après l'obtention du diplôme,\nannée de diplôme 2012") +
    guides(colour = guide_legend(override.aes = list(size = 5, alpha = 1))) +
  theme(legend.position = 'bottom', legend.spacing.x = unit(0.15, 'cm')) 
  #+
  #geom_text(data=filter(dumbbell_df, `Etat parental`=="Avec enfant(s)"), aes(x=Femme, y=`Etat parental`, label="Ages 35+"), color="#9fb059", size=3, vjust=-2, fontface="bold", family="Calibri")


pku
finalise_plot(pku, source = "OFS, Enquête auprès des diplômés des hautes écoles", 
              paste0(graf, "/pku_fr.pdf"), 
              width_pixels = 550, height_pixels = 350)

##### Graphique 3:
mef <- read_excel("~/Dropbox/Francesco/F. Lavori/Infografica/Dati/week_1.xlsx", 
    sheet = "graph_4f")
mef$perc <- mef$perc/100
mef$se <- mef$se/100

mef$type <- factor(mef$type, levels = c("HEU Master", "HES Bachelor", 
    "HEP Diplôme d'enseignement"), labels = c("Master d'une Haute Ecole Universitaire (HEU)", 
    "Bachelor d'une Haute Ecole Specialisée (HES)", "HEP Diplôme"))

mef <- mef %>% filter(type == "Master d'une Haute Ecole Universitaire (HEU)")
mef$situation <- factor(mef$situation, levels = c("Un an après", 
    "Cinq ans après"), ordered = TRUE)



gmef <- ggplot(mef, aes(y = perc, x = situation, color = sexe, 
    group = sexe)) + geom_line(stat = "identity", position = position_dodge(0.1), 
    size = 1.5) + geom_point(position = position_dodge(0.1), 
    size = 2.5) + geom_errorbar(mef, mapping = aes(ymin = perc - 
    se, ymax = perc + se), width = 0.1, position = position_dodge(0.1), 
    size = 1.5) + guides(col = guide_legend(ncol = 2), legend.position = "bottom", 
    legend.direction = "vertical") + scale_color_manual(name = "", 
    values = c("#6752c0", "#a398e2"), labels = c("Femmes", "Hommes")) + 
    bbc_style() + scale_y_continuous(limits = c(0, 0.45), labels = scales::percent_format(accuracy = 1)) + 
    xlab("") + ylab("") + labs(title = "Temps partiel dû à l'encadrement\ndes enfants et/ou tenue du ménage", 
    subtitle = "Situation un et cinq ans après l'obtention du diplôme\ndes titulaires d'un Master d'une HEU,\nannée de diplôme 2012") + 
    theme(legend.position = "bottom", legend.justification = "center", legend.spacing.x = unit(0.15, 'cm')) 

gmef


finalise_plot(gmef, source = "OFS, Enquête auprès des diplômés des hautes écoles", 
              paste0(graf, "/pmef_fr.pdf"), 
              width_pixels = 550, height_pixels = 450)

####### Graphique 1b:

s2 <- read_excel("~/Dropbox/Francesco/F. Lavori/Infografica/Dati/week_1.xlsx", 
                 sheet = "graph_1c")

s2$percentage <- s2$percentage/100

s2$sex <- factor(s2$sex, levels = c("Hommes",
                                     "Femmes"))

s2$degre <- factor(s2$degre, levels = c("Ecole obligatoire",
                                       "Degré secondaire II",
                                       "Degré tertiaire"), 
                   labels = c("Ecole\nobligatoire",
                              "Degré\nsecondaire II",
                              "Degré\ntertiaire"))


#df3$colr <- ifelse(df3$variable == "Hommes", "black", "white")
p1 <- ggplot(s2,aes(x = degre, y = percentage,fill = sex)) + 
  geom_bar(position = "fill",stat = "identity",width = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  bbc_style() + scale_fill_manual(values = c("#a398e2", "#6752c0")) +
  geom_text(data = s2, aes(x = degre, y = percentage,
                            label = paste0(round(percentage*100, 0),"%")), size = 8, 
            position = position_stack(vjust = 0.5), 
            color = "white") +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank(), 
        legend.spacing.x = unit(0.15, 'cm')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Corps enseignant selon le sexe, 2016/2017") 

p1

finalise_plot(p1, source = "OFS, Statistiques du personnel des écoles et des hautes écoles",
              paste0(graf, "/graph_1b_fr.pdf"), 
              width_pixels = 590, height_pixels = 500)


