
bb_streik <- function(){
  font <- "Helvetica"
  ggplot2::theme(plot.title = ggplot2::element_text(family = font, 
                                                    size = 50, face = "bold", color = "#222222"), 
                 plot.subtitle = ggplot2::element_text(family = font, 
                                                       size = 34, margin = ggplot2::margin(9, 0, 9, 0)),
                 plot.caption = ggplot2::element_blank(), 
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

df <- read.table(text = "besch occup fr
'Bundesrat' 'Conseil fédéral' 42.9
'Nationalrat' 'Conseil national' 32.0
'Ständerat' 'Conseil des Etats' 15.2
'Kantonale Regierungen' 'Exécutifs cantonaux' 24.0
'Kantonale Parlamente' 'Législatifs cantonaux' 27.9
'Exekutiven der Städte' 'Exécutifs des villes' 26.0
'Legislativen der Städte' 'Législatifs des villes' 31.3", header=T, sep = " ")

df$fr <- df$fr / 100

df$occup <- factor(df$occup, levels = c('Conseil fédéral',
                                        'Conseil national',
                                        'Conseil des Etats',
                                        'Exécutifs cantonaux',
                                        'Législatifs cantonaux',
                                        'Exécutifs des villes',
                                        'Législatifs des villes'), ordered = TRUE)

df$besch <- factor(df$besch, levels = c('Bundesrat',
                                        'Nationalrat',
                                        'Ständerat',
                                        'Kantonale Regierungen',
                                        'Kantonale Parlamente',
                                        'Exekutiven der Städte',
                                        'Legislativen der Städte'), ordered = TRUE)


df$occup <- factor(df$occup, levels=rev(levels(df$occup)))

df$besch <- factor(df$besch, levels=rev(levels(df$besch)))

p <- ggplot(df, aes(x = occup, y = fr)) + 
  geom_bar(stat = "identity", fill = "#6752c0") + coord_flip() + 
  scale_y_continuous(limits = c(0,0.5),labels = scales::percent_format(accuracy = 1)) + xlab("") + 
  ylab("") +
  geom_label(aes(x = occup, 
                 y = fr, label = round(fr * 100, 0)),
                 hjust = ifelse(df$fr < 0.1, 0.8, 1), vjust = 0.5, 
                 colour = "white", fill = NA, label.size = NA, family = "Helvetica", 
                 size = 12) +
  bb_streik() + 
  labs(title = "La représentation des femmes dans les\ninstitutions politiques", 
  subtitle = "Part de femmes au niveau fédéral, cantonal et communal ") 

p
finalise_plot(p, source = "OFS, Statistique des élections 2015 (CN), 2017 (villes), 2018 (CE, éxéc. cant.), 2019 (CF, législ. cant.) ", 
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_5/p1_fr.pdf", 
              width_pixels = 1000, height_pixels = 750)

# DE

p <- ggplot(df, aes(x = besch, y = fr)) + 
  geom_bar(stat = "identity", fill = "#6752c0") + coord_flip() + 
  scale_y_continuous(limits = c(0,0.5),labels = scales::percent_format(accuracy = 1)) + xlab("") + 
  ylab("") +
  geom_label(aes(x = besch, 
                 y = fr, label = round(fr * 100, 0)),
             hjust = ifelse(df$fr < 0.1, 0.8, 1), vjust = 0.5, 
             colour = "white", fill = NA, label.size = NA, family = "Helvetica", 
             size = 12) +
  bb_streik() + 
  labs(title = "Vertretung der Frauen in politischen\nInstitutionen", 
       subtitle = "Frauenanteil auf nationaler, kantonaler und städtischer Ebene") 

p
finalise_plot(p, source = "Quelle: BFS, Wahlstatistik 2015 (NR), 2017 (Städte), 2018 (SR, kant. Parl.) 2019 (BR, kant. Reg.)", 
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_5/p1_de.pdf", 
              width_pixels = 1000, height_pixels = 750)

####### Graphique 2:

df <- read.table(text = "'sex' '2000' '2001' '2002' '2003' '2004' '2005' '2006' '2007' '2008' '2009' '2010' '2011' '2012' '2013' '2014' '2015' '2016' '2017' '2018'
Frauen 7 7 7 8 8 9 9 9 10 10 10 11 11 11 11 13 12 14 14
Männer 31 30 32 32 32 31 32 30 28 28 28 27 27 27 27 25 26 24 24", header = T, sep = " ")


library(readxl)
df <- read_excel("Dropbox/Francesco/F. Lavori/Infografica/week_5/Poster-public-life-and-decision-making.xlsx",
                 sheet = "G2", skip = 2, n_max = 3)

df <- melt(df)

df$value <- df$value / 100
df$variable <- as.integer(df$variable) + 1999



df <- data.frame(df)
p1 <- ggplot(df, aes(x = variable, y = value, fill = forcats::fct_rev(sex))) + 
  geom_area(position="fill")  +  scale_fill_manual(values = c( "#a398e2", "#6752c0")) +
  guides(col = guide_legend(ncol = 1)) + xlab("") + ylab("") +
  bb_streik() + scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                                   limits = c(0, 1)) + 
  scale_x_continuous(limits = c(2000, 2018),
                     breaks = c(2000, 2005, 2010, 2015, 2018)) +
  geom_hline(yintercept = 0, size = 1, 
  colour = "#333333") + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme(text = element_text(size = 25),  legend.spacing.x = unit(0.30, 'cm')) +
  bb_streik() + theme(legend.position = "bottom")  +
  scale_colour_manual(labels = function(x) paste0(" ", x, "  ")) +
  labs(title = "Anteil Bundesrichterinnen", subtitle = "Stand am 17.12.2018")  +
  geom_label(aes(x = 2000, y = 0.18, label = "18% "), 
             hjust = -0.1, 
             vjust = 0.5, 
             colour = "#222222", 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             size = 12) +
  geom_label(aes(x = 2018, y = 0.37, label = " 37%"), 
             hjust = 1.1, 
             vjust = 0.5, 
             colour = "#222222", 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             size = 12)
p1

finalise_plot(p1, source = "Quelle: www.bger.ch > Richter und Personal", 
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_5/p2_de.pdf", 
              width_pixels = 1000, height_pixels = 750)


p1 <- ggplot(df, aes(x = variable, y = value, fill = forcats::fct_rev(sexf))) + 
  geom_area(position="fill")  +  scale_fill_manual(values = c( "#a398e2", "#6752c0")) +
  guides(col = guide_legend(ncol = 1)) + xlab("") + ylab("") +
  bb_streik() + scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                                 limits = c(0, 1)) +
  scale_x_continuous(limits = c(2000, 2018), breaks = c(2000, 2005, 2010, 2015, 2018)) +
  geom_hline(yintercept = 0, size = 1, 
             colour = "#333333") + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme(text = element_text(size = 25),  legend.spacing.x = unit(0.30, 'cm')) +
  bb_streik() + theme(legend.position = "bottom")  +
  labs(title = "Part de femmes parmi les juges fédéraux", subtitle = "Etat au 17.12.2018") +
  geom_label(aes(x = 2000, y = 0.18, label = "18% "), 
             hjust = -0.1, 
             vjust = 0.5, 
             colour = "#222222", 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             size = 12) +
geom_label(aes(x = 2018, y = 0.37, label = " 37%"), 
           hjust = 1.1, 
           vjust = 0.5, 
           colour = "#222222", 
           fill = "white", 
           label.size = NA, 
           family="Helvetica", 
           size = 12)
p1

finalise_plot(p1, source = "Source: www.bger.ch > Juges et personnel", 
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_5/p2_fr.pdf", 
              width_pixels = 1000, height_pixels = 750)

### Graphique 3:


# Graphique 3: je ne lis pas les données de l'excel:
df = read.table(text = "category value n
                Femmes 0.33 14
                Hommes 0.67 28", header = T, sep = "")

df$category <- factor(df$category, levels = c("Hommes", "Femmes"), ordered = TRUE)

# Create a basic bar
pie = ggplot(df, aes(x = "", y = value, fill = category)) +
  geom_bar(stat = "identity", width = 2000)

# Convert to pie (polar coordinates) and add labels
pie = pie + coord_polar("y", start = 0) 
# + geom_text( aes(label = paste0(round(value*100),"%")), position = position_stack(vjust = 0.5), size =10)

# Add color scale (hex colors)
pie = pie + scale_fill_manual(values = c("#a398e2", "#6752c0")) 

# BB_plot ne gère pas bien la pie chart. Je change la fonction:
bb_pie <- function() {
  font <- "Helvetica"
  ggplot2::theme(plot.title = ggplot2::element_text(family = font, 
                                                    size = 50, face = "bold", color = "#222222"),
                 plot.subtitle = ggplot2::element_text(family = font,size = 34, margin = ggplot2::margin(9, 0, 9, 0)), plot.caption = ggplot2::element_blank(), 
                 legend.position = "bottom", legend.text.align = 0, legend.background = ggplot2::element_blank(), 
                 legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), 
                 legend.text = ggplot2::element_text(family = font, size = 30, 
                                                     color = "#222222"), axis.title = ggplot2::element_blank(), 
                 axis.text = ggplot2::element_text(family = font, size = 0, 
                                                   color = "white"), 
                 axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)), axis.ticks = ggplot2::element_blank(), 
                 axis.line = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                 panel.grid.major.y = ggplot2::element_blank(), 
                 panel.grid.major.x = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), 
                 strip.background = ggplot2::element_rect(fill = "white"), 
                 strip.text = ggplot2::element_text(size = 30, hjust = 0))
}

# Tidy up the theme
pie2 <- pie + bb_pie() + labs(title = "Part de femmes dans les\nrectorats des hautes écoles*", 
                             subtitle = "Etat au 17.12.2018", 
                             caption = "\n*hautes écoles universitaires, spécialisées et pédagogiques\n") +
  theme( plot.caption = element_text(hjust = 1)) +
  geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]), 
                label = paste0(value*100, "%\n", "(N=", n, ")")), size=12, 
            family = "Helvetica", col="white") +
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme(text = element_text(size = 25),  legend.spacing.x = unit(0.30, 'cm')) 
  

finalise_plot(pie2, source = "Source: Homepages des hautes écoles",
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_5/gr_3_fr.pdf", 
              width_pixels = 800, height_pixels = 900)

# DE:

df$category <- factor(df$category, levels = c("Hommes", "Femmes"),
                      labels = c("Männer", "Frauen"), 
                      ordered = TRUE)


# Create a basic bar
pie = ggplot(df, aes(x = "", y = value, fill = category)) + 
  geom_bar(stat = "identity", width = 200)

# Convert to pie (polar coordinates) and add labels
pie = pie + coord_polar("y", start = 0) 
# + geom_text( aes(label = paste0(round(value*100),"%")), position = position_stack(vjust = 0.5), size =10)

# Add color scale (hex colors)
pie = pie + scale_fill_manual(values = c("#a398e2", "#6752c0")) 


# Tidy up the theme
pie2 <- pie + bb_pie() + labs(title = "Anteil Rektorinnen\nan den Hochschulen*", 
                              subtitle = "Stand am 17.12.2018", 
                              caption = "\n*universitäre, pädagogische und Fachhochschulen\n") +
  theme( plot.caption = element_text(hjust = -2)) +
  geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]), 
                label = paste0(value*100, "%\n", "(N=", n, ")")), size=12, 
            family = "Helvetica", col="white") +
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme(text = element_text(size = 25),  legend.spacing.x = unit(0.30, 'cm')) 


pie2
finalise_plot(pie2, source = "Quelle: Homepages der Hochschulen",
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_5/gr_3_de.pdf", 
              width_pixels = 800, height_pixels = 900)


############ GR 3 alternatif;


# Graphique 3: je ne lis pas les données de l'excel:
df = read.table(text = "category value n
                Femmes 0.33 14
                Hommes 0.67 28", header = T, sep = "")

df$category <- factor(df$category, levels = c("Hommes", "Femmes"), ordered = TRUE, 
                      labels = c("28 Hommes", "14 Femmes"))

# Create a basic bar
pie = ggplot(df, aes(x = "", y = value, fill = category)) +
  geom_bar(stat = "identity", width = 2000)

# Convert to pie (polar coordinates) and add labels
pie = pie + coord_polar("y", start = 0) 
# + geom_text( aes(label = paste0(round(value*100),"%")), position = position_stack(vjust = 0.5), size =10)

# Add color scale (hex colors)
pie = pie + scale_fill_manual(values = c("#a398e2", "#6752c0")) 

# BB_plot ne gère pas bien la pie chart. Je change la fonction:
bb_pie <- function() {
  font <- "Helvetica"
  ggplot2::theme(plot.title = ggplot2::element_text(family = font, 
                                                    size = 50, face = "bold", color = "#222222"),
                 plot.subtitle = ggplot2::element_text(family = font,size = 34, margin = ggplot2::margin(9, 0, 9, 0)), plot.caption = ggplot2::element_blank(), 
                 legend.position = "bottom", legend.text.align = 0, legend.background = ggplot2::element_blank(), 
                 legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), 
                 legend.text = ggplot2::element_text(family = font, size = 30, 
                                                     color = "#222222"), axis.title = ggplot2::element_blank(), 
                 axis.text = ggplot2::element_text(family = font, size = 0, 
                                                   color = "white"), 
                 axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)), axis.ticks = ggplot2::element_blank(), 
                 axis.line = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                 panel.grid.major.y = ggplot2::element_blank(), 
                 panel.grid.major.x = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), 
                 strip.background = ggplot2::element_rect(fill = "white"), 
                 strip.text = ggplot2::element_text(size = 30, hjust = 0))
}

# Tidy up the theme
pie2 <- pie + bb_pie() + labs(title = "Part de femmes dans les\nrectorats des hautes écoles*", 
                              subtitle = "Etat au 17.12.2018", 
                              caption = "\n*hautes écoles universitaires, spécialisées et pédagogiques\n") +
  theme( plot.caption = element_text(hjust = 1)) +
  geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]), 
                label = paste0(value*100, "%")), size=12, 
            family = "Helvetica", col="white") +
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme(text = element_text(size = 25),  legend.spacing.x = unit(0.30, 'cm')) 


finalise_plot(pie2, source = "Source: Homepages des hautes écoles",
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_5/gr_3al_fr.pdf", 
              width_pixels = 800, height_pixels = 900)

# DE:
df = read.table(text = "category value n
                Femmes 0.33 14
                Hommes 0.67 28", header = T, sep = "")

df$category <- factor(df$category, levels = c("Hommes", "Femmes"),
                      labels = c("28 Männer", "14 Frauen"), 
                      ordered = TRUE)


# Create a basic bar
pie = ggplot(df, aes(x = "", y = value, fill = category)) + 
  geom_bar(stat = "identity", width = 200)

# Convert to pie (polar coordinates) and add labels
pie = pie + coord_polar("y", start = 0) 
# + geom_text( aes(label = paste0(round(value*100),"%")), position = position_stack(vjust = 0.5), size =10)

# Add color scale (hex colors)
pie = pie + scale_fill_manual(values = c("#a398e2", "#6752c0")) 


# Tidy up the theme
pie2 <- pie + bb_pie() + labs(title = "Anteil Rektorinnen\nan den Hochschulen*", 
                              subtitle = "Stand am 17.12.2018", 
                              caption = "\n*universitäre, pädagogische und Fachhochschulen\n") +
  theme( plot.caption = element_text(hjust = -2)) +
  geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]), 
                label = paste0(value*100, "%")), size=12, 
            family = "Helvetica", col="white") +
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme(text = element_text(size = 25),  legend.spacing.x = unit(0.30, 'cm')) 


pie2
finalise_plot(pie2, source = "Quelle: Homepages der Hochschulen",
              "/home/flagana/Dropbox/Francesco/F. Lavori/Infografica/week_5/gr_3al_de.pdf", 
              width_pixels = 800, height_pixels = 900)
