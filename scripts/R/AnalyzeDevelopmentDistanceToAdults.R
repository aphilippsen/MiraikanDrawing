library(tidyverse)
library(lme4)
source("scripts/R/ReadDataMTurk.R")

# read the tables from python code
# Distance of each child to adults
all_dists = read.table("results/csv/all-dist-to-adults.csv", header=T, sep=",")

# Analysis and plots for ICDL20 paper

### Statistics ###

# which layer to use (0-6)
for (l in 1:6) {

    all_dists_outer = all_dists %>% filter(pic_type == 'type-O') %>% filter(layer==l)
    dist.lme = lmer(dist ~ age + (1|pic_class), data=all_dists_outer, REML=FALSE)
    none.lme = lmer(dist ~ 1 + (1|pic_class), data=all_dists_outer, REML=FALSE)
    anova(dist.lme, none.lme)

    all_dists_inner = all_dists %>% filter(pic_type == 'type-I') %>% filter(layer==l)
    dist.lme = lmer(dist ~ age + (1|pic_class), data=all_dists_inner, REML=FALSE)
    none.lme = lmer(dist ~ 1 + (1|pic_class), data=all_dists_inner, REML=FALSE)
    anova(dist.lme, none.lme)

    all_dists_scrambled = all_dists %>% filter(pic_type == 'type-S') %>% filter(layer==l)
    dist.lme = lmer(dist ~ age + (1|pic_class), data=all_dists_scrambled, REML=FALSE)
    none.lme = lmer(dist ~ 1 + (1|pic_class), data=all_dists_scrambled, REML=FALSE)
    anova(dist.lme, none.lme)

    # with AQ

    all_dists_outer = all_dists %>% filter(pic_type == 'type-O') %>% filter(layer==l)
    dist.lme = lmer(dist ~ aq + age + (1|pic_class), data=all_dists_outer, REML=FALSE)
    none.lme = lmer(dist ~ age + (1|pic_class), data=all_dists_outer, REML=FALSE)
    anova(dist.lme, none.lme)

    all_dists_inner = all_dists %>% filter(pic_type == 'type-I') %>% filter(layer==l)
    dist.lme = lmer(dist ~ aq + age + (1|pic_class), data=all_dists_inner, REML=FALSE)
    none.lme = lmer(dist ~ age + (1|pic_class), data=all_dists_inner, REML=FALSE)
    anova(dist.lme, none.lme)

    all_dists_scrambled = all_dists %>% filter(pic_type == 'type-S') %>% filter(layer==l)
    dist.lme = lmer(dist ~ aq + age + (1|pic_class), data=all_dists_scrambled, REML=FALSE)
    none.lme = lmer(dist ~ age + (1|pic_class), data=all_dists_scrambled, REML=FALSE)
    anova(dist.lme, none.lme)
}

### PLOTS ###
all_dists$color[all_dists$layer==0]="layer 1"
all_dists$color[all_dists$layer==1]="layer 2"
all_dists$color[all_dists$layer==2]="layer 3"
all_dists$color[all_dists$layer==3]="layer 4"
all_dists$color[all_dists$layer==4]="layer 5"
all_dists$color[all_dists$layer==5]="layer 6"
all_dists$color[all_dists$layer==6]="layer 7"

pdf("results/R_plot/all-dists-outer-all-layers.pdf", width = 6.5, height = 4.5, onefile = FALSE)
#geom_point(size=1) +
ggplot(all_dists%>% filter(pic_type == 'type-O'), aes(x = age, y = dist, color=color, fill=color))  + geom_smooth(size=1.5, method="lm", se=T, alpha=0.3) + coord_cartesian(ylim = c(0.1, 0.62)) + labs(title = "Outline presented", x = "Age (in months)", y = "Pearson distance to adult drawings", color = "") + scale_color_manual(labels = c("1st pooling layer", "2nd pooling layer", "3rd pooling layer", "4th pooling layer", "5th pooling layer", "1st fully conn. layer", "2nd fully conn. layer"), values = c("#e41a1c", "#ff7f00", "#dede16", "#4daf4a", "#72bcd4", "#377eb8", "#984ea3")) + scale_fill_manual(labels = c("1st pooling layer", "2nd pooling layer", "3rd pooling layer", "4th pooling layer", "5th pooling layer", "1st fully conn. layer", "2nd fully conn. layer"), values = c("#e41a1c", "#ff7f00", "#dede16", "#4daf4a", "#72bcd4", "#377eb8", "#984ea3")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

pdf("results/R_plot/all-dists-inner-all-layers.pdf", width = 6.5, height = 4.5, onefile = FALSE)
ggplot(all_dists%>% filter(pic_type == 'type-I'), aes(x = age, y = dist, color=color, fill=color))  + geom_smooth(size=1.5, method="lm", se=T) + coord_cartesian(ylim = c(0.1, 0.62)) + labs(title = "Inner features presented", x = "Age (in months)", y = "", color = "") + scale_color_manual(labels = c("1st pooling layer", "2nd pooling layer", "3rd pooling layer", "4th pooling layer", "5th pooling layer", "1st fully conn. layer", "2nd fully conn. layer"), values = c("#e41a1c", "#ff7f00", "#dede16", "#4daf4a", "#add8e6", "#377eb8", "#984ea3")) + scale_fill_manual(labels = c("1st pooling layer", "2nd pooling layer", "3rd pooling layer", "4th pooling layer", "5th pooling layer", "1st fully conn. layer", "2nd fully conn. layer"), values = c("#e41a1c", "#ff7f00", "#dede16", "#4daf4a", "#add8e6", "#377eb8", "#984ea3")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

pdf("results/R_plot/all-dists-scrambled-all-layers.pdf", width = 6.5, height = 4.5, onefile = FALSE)
ggplot(all_dists%>% filter(pic_type == 'type-S'), aes(x = age, y = dist, color=color, fill=color)) + geom_smooth(size=1.5, method="lm", se=T) + coord_cartesian(ylim = c(0.1, 1)) + labs(title = "Scrambled inner features presented", x = "Age (in months)", y = "", color = "") + scale_color_manual(labels = c("1st pooling layer", "2nd pooling layer", "3rd pooling layer", "4th pooling layer", "5th pooling layer", "1st fully conn. layer", "2nd fully conn. layer"), values = c("#e41a1c", "#ff7f00", "#dede16", "#4daf4a", "#add8e6", "#377eb8", "#984ea3")) + scale_fill_manual(labels = c("1st pooling layer", "2nd pooling layer", "3rd pooling layer", "4th pooling layer", "5th pooling layer", "1st fully conn. layer", "2nd fully conn. layer"), values = c("#e41a1c", "#ff7f00", "#dede16", "#4daf4a", "#add8e6", "#377eb8", "#984ea3")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()


# SAME PLOTS FOR AQ SCORE
pdf("results/R_plot/all-dists_to-AQ-outer-all-layers.pdf", width = 6.5, height = 4.5, onefile = FALSE)
# geom_point(size=1) +
ggplot(all_dists%>% filter(pic_type == 'type-O'), aes(x = aq, y = dist, color=color, fill=color))  + geom_smooth(size=1.5, method="lm", se=T) + coord_cartesian(ylim = c(0,1)) + labs(title = "Outline presented", x = "AQ", y = "Pearson distance to adult drawings", color = "") + scale_color_manual(labels = c("1st pooling layer", "2nd pooling layer", "3rd pooling layer", "4th pooling layer", "5th pooling layer", "1st fully conn. layer", "2nd fully conn. layer"), values = c("#e41a1c", "#ff7f00", "#dede16", "#4daf4a", "#add8e6", "#377eb8", "#984ea3")) + scale_fill_manual(labels = c("1st pooling layer", "2nd pooling layer", "3rd pooling layer", "4th pooling layer", "5th pooling layer", "1st fully conn. layer", "2nd fully conn. layer"), values = c("#e41a1c", "#ff7f00", "#dede16", "#4daf4a", "#add8e6", "#377eb8", "#984ea3")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

pdf("results/R_plot/all-dists_to-AQ-inner-all-layers.pdf", width = 6.5, height = 4.5, onefile = FALSE)
ggplot(all_dists%>% filter(pic_type == 'type-I'), aes(x = aq, y = dist, color=color, fill=color))  + geom_smooth(size=1.5, method="lm", se=T) + coord_cartesian(ylim = c(0, 1)) + labs(title = "Inner features presented", x = "AQ", y = "", color = "") + scale_color_manual(labels = c("1st pooling layer", "2nd pooling layer", "3rd pooling layer", "4th pooling layer", "5th pooling layer", "1st fully conn. layer", "2nd fully conn. layer"), values = c("#e41a1c", "#ff7f00", "#dede16", "#4daf4a", "#add8e6", "#377eb8", "#984ea3")) + scale_fill_manual(labels = c("1st pooling layer", "2nd pooling layer", "3rd pooling layer", "4th pooling layer", "5th pooling layer", "1st fully conn. layer", "2nd fully conn. layer"), values = c("#e41a1c", "#ff7f00", "#dede16", "#4daf4a", "#add8e6", "#377eb8", "#984ea3")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

pdf("results/R_plot/all-dists_to-AQ-scrambled-all-layers.pdf", width = 6.5, height = 4.5, onefile = FALSE)
ggplot(all_dists%>% filter(pic_type == 'type-S'), aes(x = aq, y = dist, color=color, fill=color)) + geom_smooth(size=1.5, method="lm", se=T) + coord_cartesian(ylim = c(0, 1)) + labs(title = "Scrambled inner features presented", x = "AQ", y = "", color = "") + scale_color_manual(labels = c("1st pooling layer", "2nd pooling layer", "3rd pooling layer", "4th pooling layer", "5th pooling layer", "1st fully conn. layer", "2nd fully conn. layer"), values = c("#e41a1c", "#ff7f00", "#dede16", "#4daf4a", "#add8e6", "#377eb8", "#984ea3")) + scale_fill_manual(labels = c("1st pooling layer", "2nd pooling layer", "3rd pooling layer", "4th pooling layer", "5th pooling layer", "1st fully conn. layer", "2nd fully conn. layer"), values = c("#e41a1c", "#ff7f00", "#dede16", "#4daf4a", "#add8e6", "#377eb8", "#984ea3")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

