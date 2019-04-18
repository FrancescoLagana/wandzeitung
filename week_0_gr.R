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
lin <- "~/Dropbox/Francesco/F. Lavori/Infografica/Dati/"
setwd(lin)

# Stocker les graphiques
graf <- "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/Grafici/week_0/"

# Define Labels pour versions FR et DE: 
sex_fr <- c("Hommes", "Femmes")

t_occu_fr <- c("Inférieur à 50%",
               "50-69%",
               "70-89%",
               "90-100%",
               "Total")

class_sa_fr <- c("Classe \nsalaire \n01-11",
                 "Classe \nsalaire \n12-17",
                 "Classe \nsalaire \n18-23",
                 "Classe \nsalaire\n 24-29",
                 "Classe \nsalaire \n30-38")

##### Graphique 1:
df2 <- read_excel("week_0.xlsx", sheet = "Sheet1")

df3 <- melt(df2, id.vars = c("T_occu"))

df3$variable <- factor(df3$variable, levels = sex_fr,  ordered = TRUE)

df3$T_occu <- factor(df3$T_occu, levels = t_occu_fr, ordered = TRUE)

df3 <- df3 %>% filter(df3$T_occu != "Total")

df3$cols <- ifelse(df3$variable == "Hommes", "black", "white")

#df3$colr <- ifelse(df3$variable == "Hommes", "black", "white")
p1 <- ggplot(df3,aes(x = T_occu, y = value,fill = variable)) + 
  geom_bar(position = "fill",stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  bbc_style() + scale_fill_manual(values = c("#a398e2", "#6752c0")) +
  geom_text(data = df3, aes(x = T_occu, y = value,
                                  label = paste0(round(value*100, 0),"%")), size = 8, 
            position = position_stack(vjust = 0.5), 
            color = "white") +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Taux d'occupation, en 2017") 

p1


finalise_plot(p1, source = "Personallkennzahlen EDI, 2017",
              paste0(graf, "/graph_1_fr.pdf"), 
              width_pixels = 660, height_pixels = 500)


##### Graphique 2:
df2 <- read_excel("week_0.xlsx", sheet = "Sheet2")

df3 <- melt(df2, id.vars = c("classe_salaire"))

df3$variable <- factor(df3$variable, levels = sex_fr, ordered = TRUE)

df3$classe_salaire <- factor(df3$classe_salaire, 
                     levels = class_sa_fr, ordered = TRUE
)


df3$cols <- ifelse(df3$variable == "Hommes", "black", "white")

#df3$colr <- ifelse(df3$variable == "Hommes", "black", "white")
p1 <- ggplot(df3,aes(x = classe_salaire, y = value, fill = variable)) + 
  geom_bar(position = "fill",stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  bbc_style() + scale_fill_manual(values = c("#a398e2", "#6752c0")) +
  geom_text(data = df3, aes(x = classe_salaire, y = value,
                            label = paste0(round(value*100, 0),"%")), size = 8, 
            position = position_stack(vjust = .5), 
            color = "white") +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Classes de salaire, en 2017") 


p1
finalise_plot(p1, source = "Personallkennzahlen EDI, 2017",
              paste0(graf, "/graph_2_fr.pdf"), 
              width_pixels = 660, height_pixels = 500)

# Graphique 3: je ne lis pas les données de l'excel:
df = read.table(text = "category value
                          Femmes 0.XX
                 Hommes 0.XX", header = T, sep = "")

df$category <- factor(df$category, levels = sex_fr, ordered = TRUE)

# Create a basic bar
pie = ggplot(df, aes(x = "", y = value, fill = category)) + geom_bar(stat = "identity", width = 1)

# Convert to pie (polar coordinates) and add labels
pie = pie + coord_polar("y", start = 0) 
# + geom_text( aes(label = paste0(round(value*100),"%")), position = position_stack(vjust = 0.5), size =10)

# Add color scale (hex colors)
pie = pie + scale_fill_manual(values = c("#a398e2", "#6752c0")) 

# BB_plot ne gère pas bien la pie chart. Je change la fonction:
bb_pie <- function() {
  font <- "Helvetica"
  ggplot2::theme(plot.title = ggplot2::element_text(family = font, 
                                                    size = 28, face = "bold", color = "#222222"),
                 plot.subtitle = ggplot2::element_text(family = font,size = 22, margin = ggplot2::margin(9, 0, 9, 0)), plot.caption = ggplot2::element_blank(), 
                 # on ne veut pas de légende (celui qui fait le graphique écrit avec Draw)
                 legend.position = "none", legend.text.align = 0, legend.background = ggplot2::element_blank(), 
                 legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), 
                 legend.text = ggplot2::element_text(family = font, size = 18, 
                                                     color = "#222222"), axis.title = ggplot2::element_blank(), 
                 axis.text = ggplot2::element_text(family = font, size = 18, 
                                                   color = "white"), axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, 
                                                                                                                                    b = 10)), axis.ticks = ggplot2::element_blank(), 
                 axis.line = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                 panel.grid.major.y = ggplot2::element_line(color = "white"), 
                 panel.grid.major.x = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), 
                 strip.background = ggplot2::element_rect(fill = "white"), 
                 strip.text = ggplot2::element_text(size = 22, hjust = 0))
}

# Tidy up the theme
pie <- pie + bb_pie() + labs(title = "Primes versées en francs, en 2017", subtitle = "Valeur totale des primes en 2017 : CHF XXX'XXX")

pie
finalise_plot(pie, source = "Personallkennzahlen EDI, 2017",
              paste0(graf, "/graph_3_fr.pdf"), 
              width_pixels = 600, height_pixels = 600)
