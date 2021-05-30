library(tidyverse)
library(lme4)
source("scripts/R/ReadDataMTurk.R")

# STATISTICS score evaluation
all_score = read.table("results/csv/score-evaluation.csv", header=T, sep=",")
l=6
all_score_layer = all_score %>% filter(layer==l)# %>% filter(aq != -1)# %>% filter(aq<=17)
dist.interact = lm(scoreByMean ~ aq * age, data=all_score_layer)
dist.sum = lm(scoreByMean ~ aq + age, data=all_score_layer)
dist.onlyage = lm(scoreByMean ~ age, data = all_score_layer)
dist.onlyaq = lm(scoreByMean ~ aq, data = all_score_layer)
anova(dist.interact, dist.sum) # Interaction test
anova(dist.onlyage, dist.sum) # Effect of AQ
anova(dist.onlyaq, dist.sum) # Effect of age

png(capture.output(cat(cat("results/R_plot/score",l,sep="-"), ".png",sep="")), width = 350, height = 300)
ggplot(all_score_layer, aes(x = aq, y = scoreByMean, color=age, size=10)) + geom_point(width=0.2,height=0.2) + geom_smooth(method="lm", se=T)
dev.off()

# comparing score with scores
compl_per_child = all_compl %>% group_by(Child) %>% summarize(MeanCompl = mean(COMP))
scr_per_child = all_scribble %>% group_by(Child) %>% summarize(MeanScribble = mean(SCR))
mea_per_child = all_meaning %>% group_by(Child) %>% summarize(MeanMeaning = mean(MEANING))
tra_per_child = all_tracing %>% group_by(Child) %>% summarize(MeanTracing = mean(TRACING))
col_per_child = all_coloring %>% group_by(Child) %>% summarize(MeanColoring = mean(COLORING))

temp = merge(all_score, compl_per_child, by=c('Child'))
temp = merge(temp, mea_per_child, by=c('Child'))
temp = merge(temp, tra_per_child, by=c('Child'))
temp = merge(temp, col_per_child, by=c('Child'))
score_vs_ratings = merge(temp, scr_per_child, by=c('Child'))

png(capture.output(cat(cat("results/R_plot/score-vs-compl",l,sep="-"), ".png",sep="")), width = 350, height = 300)
ggplot(score_vs_ratings, aes(x = MeanCompl, y = scoreByMean, color=age, size=10)) + geom_point(width=0.2,height=0.2) + geom_smooth(method="lm", se=T)
dev.off()

png(capture.output(cat(cat("results/R_plot/score-vs-scr",l,sep="-"), ".png",sep="")), width = 350, height = 300)
ggplot(score_vs_ratings, aes(x = MeanScribble, y = scoreByMean, color=age, size=10)) + geom_point(width=0.2,height=0.2) + geom_smooth(method="lm", se=T)
dev.off()

png(capture.output(cat(cat("results/R_plot/score-vs-mea",l,sep="-"), ".png",sep="")), width = 350, height = 300)
ggplot(score_vs_ratings, aes(x = MeanMeaning, y = scoreByMean, color=age, size=10)) + geom_point(width=0.2,height=0.2) + geom_smooth(method="lm", se=T)
dev.off()

png(capture.output(cat(cat("results/R_plot/score-vs-tra",l,sep="-"), ".png",sep="")), width = 350, height = 300)
ggplot(score_vs_ratings, aes(x = MeanTracing, y = scoreByMean, color=age, size=10)) + geom_point(width=0.2,height=0.2) + geom_smooth(method="lm", se=T)
dev.off()

png(capture.output(cat(cat("results/R_plot/score-vs-col",l,sep="-"), ".png",sep="")), width = 350, height = 300)
ggplot(score_vs_ratings, aes(x = MeanColoring, y = scoreByMean, color=age, size=10)) + geom_point(width=0.2,height=0.2) + geom_smooth(method="lm", se=T)
dev.off()

write.csv(score_vs_ratings %>% filter(layer==6),"results/csv/score-vs-ratings-6.csv", row.names = T)
write.csv(score_vs_ratings %>% filter(layer==5),"results/csv/score-vs-ratings-5.csv", row.names = T)
write.csv(score_vs_ratings %>% filter(layer==4),"results/csv/score-vs-ratings-4.csv", row.names = T)
write.csv(score_vs_ratings %>% filter(layer==3),"results/csv/score-vs-ratings-3.csv", row.names = T)
write.csv(score_vs_ratings %>% filter(layer==2),"results/csv/score-vs-ratings-2.csv", row.names = T)
write.csv(score_vs_ratings %>% filter(layer==1),"results/csv/score-vs-ratings-1.csv", row.names = T)
write.csv(score_vs_ratings %>% filter(layer==0),"results/csv/score-vs-ratings-0.csv", row.names = T)


# STATISTICS

score_vs_ratings_6 = score_vs_ratings %>% filter(layer==6)

dist.interact = lm(scoreByMean ~ MeanScribble * MeanCompl, data=score_vs_ratings_6)
dist.sum = lm(scoreByMean ~ MeanScribble + MeanCompl, data=score_vs_ratings_6)
dist.onlycom = lm(scoreByMean ~ MeanCompl, data = score_vs_ratings_6)
dist.onlyscr = lm(scoreByMean ~ MeanScribble, data = score_vs_ratings_6)
dist.onlymea = lm(scoreByMean ~ MeanMeaning, data = score_vs_ratings_6)
dist.onlytra = lm(scoreByMean ~ MeanTracing, data = score_vs_ratings_6)
dist.onlycol = lm(scoreByMean ~ MeanColoring, data = score_vs_ratings_6)
anova(dist.interact, dist.sum) # Interaction test
anova(dist.onlycom) # Effect of scribble
anova(dist.onlyscr) # Effect of completion
anova(dist.onlymea)
anova(dist.onlytra)
anova(dist.onlycol)

# correlation tests (SRCD21)
cor.test(score_vs_ratings_6$scoreByMean, score_vs_ratings_6$MeanCompl, method='pearson')
cor.test(score_vs_ratings_6$scoreByMean, score_vs_ratings_6$MeanScribble, method='pearson')
cor.test(score_vs_ratings_6$scoreByMean, score_vs_ratings_6$MeanMeaning, method='pearson')
cor.test(score_vs_ratings_6$scoreByMean, score_vs_ratings_6$MeanTracing, method='pearson')
cor.test(score_vs_ratings_6$scoreByMean, score_vs_ratings_6$MeanColoring, method='pearson')

pdf(capture.output(cat(cat("results/R_plot/MeanCompl_scoreByMean",l,sep="-"), ".pdf",sep="")), width = 6, height = 4.5, onefile = FALSE)
ggplot(score_vs_ratings_6, aes(x = MeanCompl, y = scoreByMean, size=10)) + geom_point(size=3) + geom_smooth(method="lm", se=T)
dev.off()

