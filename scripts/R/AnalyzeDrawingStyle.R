library(tidyverse)
library(lme4)
source("scripts/R/ReadDataMTurk.R")

### ICDL20 statistics analysis of drawing style ###

style.data = read.csv('results/python_csv/drawing-style-data-80.csv')

# layer 0 to 6
for (l in 1:6) {
    print("\n\nlayer " + str(l))

    data = style.data %>% filter(layer==l) %>% filter(style==0 | style==3)
    dist.lm = lm(dist ~ style, data=data)
    summary(dist.lm)
    # summary gives me automatically the p-value for the null hypothesis, same as:
    #none.lm = lm(dist ~ 1, data=data)
    #anova(dist.lm, none.lm)

    data = style.data %>% filter(layer==l) %>% filter(style==1 | style==3)
    dist.lm = lm(dist ~ style, data=data)
    summary(dist.lm)

    data = style.data %>% filter(layer==l) %>% filter(style==2 | style==3)
    dist.lm = lm(dist ~ style, data=data)
    summary(dist.lm)
}

