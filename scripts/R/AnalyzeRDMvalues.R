library(tidyverse)
library(lme4)

rdm.data = read.csv('results/python_csv/RDM_values.csv')

print("Difference between kid1 group and adults?")
data = rdm.data %>% filter(group=="kid1" | group=="adult")
dist.lm = lm(dist ~ group, data=data)
print(summary(dist.lm))

print("Difference between kid2 group and adults?")
data = rdm.data %>% filter(group=="kid2" | group=="adult")
dist.lm = lm(dist ~ group, data=data)
print(summary(dist.lm))

print("Difference between kid3 group and adults?")
data = rdm.data %>% filter(group=="kid3" | group=="adult")
dist.lm = lm(dist ~ group, data=data)
print(summary(dist.lm))

print("Difference between kid4 group and adults?")
data = rdm.data %>% filter(group=="kid4" | group=="adult")
dist.lm = lm(dist ~ group, data=data)
print(summary(dist.lm))
