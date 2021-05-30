library(tidyverse)
library(lme4)
source("scripts/R/ReadDataMTurk.R")

### statistics analysis of drawing style ###

style.data = read.csv('results/csv/drawing-style-data-70.csv')

# layer 0 to 6
for (l in 0:6) {
    print("Current layer is: ")
    print(l)

    print("Scribbling vs. completion")
    data = style.data %>% filter(layer==l) %>% filter(style==0 | style==3)
    dist.lm = lm(dist ~ style, data=data)
    # summary provides p-value for the null hypothesis
    print(summary(dist.lm))
    # which is the same as doing:
    # none.lm = lm(dist ~ 1, data=data)
    # anova(dist.lm, none.lm)

    print("Coloring vs. completion")
    data = style.data %>% filter(layer==l) %>% filter(style==1 | style==3)
    dist.lm = lm(dist ~ style, data=data)
    print(summary(dist.lm))

    print("Tracing vs. completion")
    data = style.data %>% filter(layer==l) %>% filter(style==2 | style==3)
    dist.lm = lm(dist ~ style, data=data)
    print(summary(dist.lm))
}

