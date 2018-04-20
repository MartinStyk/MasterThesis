# Nacitanie dat
data_install<- read.csv("installs_sk.styk.martin.apkanalyzer_overview.csv",
              sep = ",",
              header = TRUE,
              stringsAsFactors = FALSE)

# Uprava datumoveho stlpca
data_install$Date <- as.Date(data_install$Date, "%d.%m.%Y")
# Nacitanie potrebnych kniznic
library(ggplot2)


### Graf vyvoj instalacii v case
ggplot(data = data_install,
       mapping = aes(
           x = Date,
           y = Daily.Device.Installs
       )) + geom_line(color = "skyblue3") +
    labs(y = "História dennıch inštalácií") +
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "gray90"),
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank())

ggsave(file = "ins_vyvoj.pdf",
       width = 8,
       height = 4,
       dpi=900)

### Graf vyvoj poctu instalacii na aktivnych zariadeniach
ggplot(data = data_install,
       mapping = aes(
           x = Date,
           y = Active.Device.Installs
       )) + geom_area(color = "skyblue3", fill = "skyblue") +
    labs(y = "Aktívne inštalácie") +
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "gray90"),
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank())

ggsave(file = "inst_akt.pdf",
       dpi=900)

### Graf poctu instalacii po urcitom case
data_left_install <- data.frame(time = c("1 deò", "7 dní",
                                         "15 dní", "30 dní"),
                                value = c(73.7, 62.1,
                                          56.1, 52),
                                median = c(55.1, 41.4,
                                           35.9, 30.7))
data_left_install$time <- factor(data_left_install$time,
                                 levels = c("1 deò", "7 dní",
                                            "15 dní", "30 dní"))

group.colors <- c("skyblue1", "skyblue2", "skyblue3", "skyblue4")

ggplot(data = data_left_install,
       mapping = aes(
           x = time,
           y = value,
           fill = time
       )) + geom_bar(stat = "identity",
                     position = "dodge",
                     width = 0.6) +
    geom_text(mapping = aes(label = value),
              position = position_dodge(0.9),
              vjust = -0.5,
              size = 3) +
    ylim(0,100) +
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "gray90"),
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank()) +
    labs(y = "Udranie uívate¾ov (v %)") +
    guides(fill = FALSE) +
    scale_fill_manual(values = group.colors) +
    geom_point(data = data_left_install,
               mapping = aes(x = time,
                             y = median),
               size = 0.5)

ggsave(file = "inst_po.pdf",
       width = 8,
       height = 4,
       dpi=900)

### Graf hodnoteni

hodnotenia <- data.frame(stars = factor(c("5", "4", "3", "2", "1"),
                                        levels = c("1", "2", "3", "4", "5")),
                         values = c(146, 20, 8, 3, 9))
hodnotenia.color <- c("#D25A5A", "#ED7F27", "#FACB2D", "#B5DD53", "#00A75C")
ggplot(data = hodnotenia,
       mapping = aes(
           x = stars,
           y = values,
           fill = stars
           )) +
    geom_bar(stat = "identity",
             position = "dodge",
             width = 0.6) +
    guides(fill = FALSE) +
    scale_fill_manual(values = hodnotenia.color) +
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "gray90"),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank()) +
    geom_text(mapping = aes(label = values),
              vjust = 0.3,
              size = 3,
              hjust = -0.4) +
    labs(x = "Poèet hviezdièiek",
         y = "Poèet hodnotení") +
    coord_flip()

ggsave(file = "hodnotenia.pdf",
       dpi=900)

### Graf zobrazenych obrazoviek
obrazovky <- data.frame(rank = c(1, 1, 1),
                        label = factor(c("Aplikácia je prebalená",
                                  "Aplikácia je originál",
                                  "Nedostatok dát"),
                                  levels = c("Aplikácia je prebalená",
                                             "Aplikácia je originál",
                                             "Nedostatok dát")),
                        value = c(166, 801, 713))
obrazovky.color <- c("#D25A5A", "#00A75C", "#DEDEDE")

ggplot(obrazovky,
       mapping = aes(x = rank,
                     y = value,
                     fill = label)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y = element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank()) +
    labs(x = "",
         y = "",
         fill = "") +
    scale_fill_manual(values = obrazovky.color)

ggsave(file = "obrazovky.pdf",
       width = 8,
       height = 2,
       dpi=900)
