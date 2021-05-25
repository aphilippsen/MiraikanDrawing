source("scripts/R/ReadDataMTurk.R")

##### Hypothesis 1
print("")
print("")
print("Hypothesis 1: With increasing age children will exhibit more representational drawing.")

# We test whether scribbling can be explained by the fixed effects: age, AQ.
# (1|WorkerId) accounts for individual differences in ratings
# (1|Child) for generally different tendencies of children to draw
# (1|PictureIdx) for the order of drawing
# (worker trial information is not available in MTurk data)

#repr.lme = lmer(COMP ~ age * AQ + (1|WorkerId) + (1|PictureIdx) + (1|Child) + (1|PicType) + (1|PicClass),data = all_compl, REML=FALSE)
#summary(repr.lme)
#anova(repr.lme)

# using only PictureClass leads to singularity, therefore, only Child is used as a random effect (plus additionally PictureType in the whole-data-set test)

# for whole data set
repr.lme = lmer(COMP ~ age * AQ + (1|PicType) + (1|Child),data = all_compl, REML=FALSE)
repr.middle = lmer(COMP ~ age + AQ + (1|PicType) + (1|Child),data = all_compl, REML=FALSE)
repr.none = lmer(COMP ~ AQ + (1|PicType) + (1|Child),data = all_compl, REML=FALSE)

print("Interaction?")
anova(repr.lme, repr.middle)
print("Influence of age?")
anova(repr.middle, repr.none)

# for C1
print("Hypothesis 1 – for C1")
repr.lme = lmer(COMP ~ age * AQ + (1|Child),data = all_compl_C1, REML=FALSE)
repr.middle = lmer(COMP ~ age + AQ + (1|Child), data = all_compl_C1, REML=FALSE)
repr.none = lmer(COMP ~ AQ + (1|Child), data = all_compl_C1,  REML=FALSE)
print("Interaction?")
anova(repr.lme, repr.middle)
print("Influence of age?")
anova(repr.middle, repr.none)

# for C2
print("Hypothesis 1 – for C2")
repr.lme = lmer(COMP ~ age * AQ + (1|Child),data = all_compl_C2, REML=FALSE)
repr.middle = lmer(COMP ~ age + AQ + (1|Child),data = all_compl_C2, REML=FALSE)
repr.none = lmer(COMP ~ AQ + (1|Child),data = all_compl_C2, REML=FALSE)
print("Interaction?")
anova(repr.lme, repr.middle)
print("Influence of age?")
anova(repr.middle, repr.none)

# for C3
print("Hypothesis 1 – for C3")
repr.lme = lmer(COMP ~ age * AQ + (1|Child),data = all_compl_C3, REML=FALSE)
repr.middle = lmer(COMP ~ age + AQ + (1|Child) ,data = all_compl_C3, REML=FALSE)
repr.none = lmer(COMP ~ AQ + (1|Child),data = all_compl_C3, REML=FALSE)
print("Interaction?")
anova(repr.lme, repr.middle)
print("Influence of age?")
anova(repr.middle, repr.none)



##### Hypothesis 2:
# With increasing age children will exhibit less scribbling.
# We test whether scribbling can be explained by the fixed effects: age, AQ.
# (1|WorkerId) accounts for individual differences in ratings
# (1|Child) for generally different tendencies of children to draw and (1|PictureIdx) for the order of drawing
print("")
print("")
print("Hypothesis 2: With increasing age children will exhibit less scribbling.")

# for the whole data set
scribble.lme = lmer(SCR ~ age * AQ + (1|Child) + (1|PicType),data = all_scribble, REML=FALSE)
scribble.middle = lmer(SCR ~ age + AQ + (1|Child) + (1|PicType),data = all_scribble, REML=FALSE)
scribble.none = lmer(SCR ~ AQ + (1|Child) + (1|PicType),data = all_scribble, REML=FALSE)
anova(scribble.lme, scribble.middle)
anova(scribble.middle, scribble.none)


# for C1
print("Hypothesis 2 – for C1")
scribble.lme = lmer(SCR ~ age * AQ + (1|Child) + (1|PicClass),data = all_scribble_C1, REML=FALSE)
scribble.middle = lmer(SCR ~ age + AQ + (1|Child) + (1|PicClass),data = all_scribble_C1, REML=FALSE)
scribble.none = lmer(SCR ~ AQ + (1|Child) + (1|PicClass),data = all_scribble_C1, REML=FALSE)
print("Interaction?")
anova(scribble.lme, scribble.middle)
print("Influence of age?")
anova(scribble.middle, scribble.none)

# for C2
print("Hypothesis 2 – for C2")
scribble.lme = lmer(SCR ~ age * AQ  + (1|Child) + (1|PicClass),data = all_scribble_C2, REML=FALSE)
scribble.middle = lmer(SCR ~ age + AQ + (1|Child) + (1|PicClass),data = all_scribble_C2, REML=FALSE)
scribble.none = lmer(SCR ~ AQ + (1|Child) + (1|PicClass),data = all_scribble_C2, REML=FALSE)
print("Interaction?")
anova(scribble.lme, scribble.middle)
print("Influence of age?")
anova(scribble.middle, scribble.none)

# for C3
print("Hypothesis 2 – for C3")
scribble.lme = lmer(SCR ~ age * AQ + (1|Child) + (1|PicClass),data = all_scribble_C3, REML=FALSE)
scribble.middle = lmer(SCR ~ age + AQ + (1|Child) + (1|PicClass),data = all_scribble_C3, REML=FALSE)
scribble.none = lmer(SCR ~ AQ + (1|Child) + (1|PicClass),data = all_scribble_C3, REML=FALSE)
print("Interaction?")
anova(scribble.lme, scribble.middle)
print("Influence of age?")
anova(scribble.middle, scribble.none)

##### Hypothesis 3:
# Children with a higher AQ score will be less likely to perform representational drawings.
print("")
print("")
print("Hypothesis 3: Children with a higher AQ score will be less likely to perform representational drawings.")
#repr.lme = lmer(COMP ~ age * AQ + (1|WorkerId) + (1|Child) + (1|PicType) + (1|PicClass),data = all_compl)
#summary(repr.lme)
#anova(repr.lme)

# for whole data set
#repr.lme = lmer(COMP ~ age * AQ + (1|WorkerId) + (1|Child) + (1|PicType) + (1|PicClass),data = all_compl, REML=FALSE)
#repr.middle = lmer(COMP ~ age + AQ  + (1|WorkerId) + (1|Child) + (1|PicType) + (1|PicClass),data = all_compl, REML=FALSE)
#repr.none = lmer(COMP ~ age  + (1|WorkerId) + (1|Child) + (1|PicType) + (1|PicClass),data = all_compl, REML=FALSE)
#print("Interaction?")
#anova(repr.lme, repr.middle)
#print("Influence of AQ?")
#anova(repr.middle, repr.none)

repr.lme = lmer(COMP ~ age * AQ + (1|PicType) + (1|Child),data = all_compl, REML=FALSE)
repr.middle = lmer(COMP ~ age + AQ + (1|PicType) + (1|Child),data = all_compl, REML=FALSE)
repr.none = lmer(COMP ~ age + (1|PicType) + (1|Child),data = all_compl, REML=FALSE)
print("Interaction?")
anova(repr.lme, repr.middle)
print("Influence of AQ?")
anova(repr.middle, repr.none)


 
# for C1
print("Hypothesis 3 – for C1")
repr.lme = lmer(COMP ~ age * AQ  + (1|Child),data = all_compl_C1, REML=FALSE)
repr.middle = lmer(COMP ~ age + AQ + (1|Child),data = all_compl_C1, REML=FALSE)
repr.none = lmer(COMP ~ age + (1|Child),data = all_compl_C1, REML=FALSE)
print("Interaction?")
anova(repr.lme, repr.middle)
print("Influence of AQ?")
anova(repr.middle, repr.none)

# for C2
print("Hypothesis 3 – for C2")
repr.lme = lmer(COMP ~ age * AQ + (1|Child),data = all_compl_C2, REML=FALSE)
repr.middle = lmer(COMP ~ age + AQ + (1|Child),data = all_compl_C2, REML=FALSE)
repr.none = lmer(COMP ~ age + (1|Child),data = all_compl_C2, REML=FALSE)
print("Interaction?")
anova(repr.lme, repr.middle)
print("Influence of AQ?")
anova(repr.middle, repr.none)

# for C3
print("Hypothesis 3 – for C3")
repr.lme = lmer(COMP ~ age * AQ + (1|Child) ,data = all_compl_C3, REML=FALSE)
repr.middle = lmer(COMP ~ age + AQ + (1|Child),data = all_compl_C3, REML=FALSE)
repr.none = lmer(COMP ~ age + (1|Child),data = all_compl_C3, REML=FALSE)
print("Interaction?")
anova(repr.lme, repr.middle)
print("Influence of AQ?")
anova(repr.middle, repr.none)

# Hypothesis 3-2: scribbling vs. AQ

scribble.lme = lmer(SCR ~ age * AQ + (1|Child),data = all_scribble, REML=FALSE)
scribble.middle = lmer(SCR ~ age + AQ + (1|Child),data = all_scribble, REML=FALSE)
scribble.none = lmer(SCR ~ age + (1|Child),data = all_scribble, REML=FALSE)
print("Interaction?")
anova(scribble.lme, scribble.middle)
print("Influence of AQ?")
anova(scribble.middle, scribble.none)

# for C1
print("Hypothesis 3-2 – for C1")
scribble.lme = lmer(SCR ~ age * AQ + (1|Child),data = all_scribble_C1, REML=FALSE)
scribble.middle = lmer(SCR ~ age + AQ + (1|Child),data = all_scribble_C1, REML=FALSE)
scribble.none = lmer(SCR ~ age + (1|Child),data = all_scribble_C1, REML=FALSE)
print("Interaction?")
anova(scribble.lme, scribble.middle)
print("Influence of AQ?")
anova(scribble.middle, scribble.none)
 
 
# for C2
print("Hypothesis 3-2 – for C2")
scribble.lme = lmer(SCR ~ age * AQ + (1|Child),data = all_scribble_C2, REML=FALSE)
scribble.middle = lmer(SCR ~ age + AQ + (1|Child),data = all_scribble_C2, REML=FALSE)
scribble.none = lmer(SCR ~ age + (1|Child),data = all_scribble_C2, REML=FALSE)
print("Interaction?")
anova(scribble.lme, scribble.middle)
print("Influence of AQ?")
anova(scribble.middle, scribble.none)

# for C3
print("Hypothesis 3-2 – for C3")
scribble.lme = lmer(SCR ~ age * AQ + (1|Child),data = all_scribble_C3, REML=FALSE)
scribble.middle = lmer(SCR ~ age + AQ + (1|Child),data = all_scribble_C3, REML=FALSE)
scribble.none = lmer(SCR ~ age + (1|Child),data = all_scribble_C3, REML=FALSE)
print("Interaction?")
anova(scribble.lme, scribble.middle)
print("Influence of AQ?")
anova(scribble.middle, scribble.none)

#### Hypothesis 4:
print("")
print("")
print("Hypothesis 4: Children will show similar drawing ability when the outline is presented or when inner parts are presented.")
# Children will show similar drawing ability when the outline is presented or when inner parts are presented.
repr.lme = lmer(COMP ~ age * AQ * PicType + (1|WorkerId) + (1|Child) + (1|PicClass),data = all_compl_C1C2)
summary(repr.lme)
anova(repr.lme)

scribble.lme = lmer(SCR ~ age * AQ * PicType +  (1|WorkerId) + (1|Child) + (1|PicClass),data = all_scribble_C1C2)
summary(scribble.lme)
anova(scribble.lme)

# test for representational drawing score
print("For representational drawing:")
repr.lme = lmer(COMP ~ age * AQ * PicType +  (1|Child),data = all_compl_C1C2, REML=FALSE)
repr.middle = lmer(COMP ~ age * AQ + PicType +  (1|Child),data = all_compl_C1C2, REML=FALSE)
repr.none = lmer(COMP ~ age * AQ + (1|Child),data = all_compl_C1C2, REML=FALSE)
anova(repr.lme, repr.middle)
anova(repr.middle, repr.none)


# test for scribbling score
print("For scribbling:")
scribble.lme = lmer(SCR ~ age * AQ * PicType + (1|Child),data = all_scribble_C1C2, REML=FALSE)
scribble.middle = lmer(SCR ~ age * AQ + PicType + (1|Child) ,data = all_scribble_C1C2, REML=FALSE)
scribble.none = lmer(SCR ~ age * AQ + (1|Child),data = all_scribble_C1C2, REML=FALSE)
anova(scribble.lme, scribble.middle)
anova(scribble.middle, scribble.none)
summary(scribble.lme)

# test for tracing score
print("For tracing:")
tracing.lme = lmer(TRACING ~ age * AQ * PicType + (1|Child),data = all_tracing_C1C2, REML=FALSE)
tracing.middle = lmer(TRACING ~ age * AQ + PicType + (1|Child) ,data = all_tracing_C1C2, REML=FALSE)
tracing.none = lmer(TRACING ~ age * AQ + (1|Child),data = all_tracing_C1C2, REML=FALSE)
anova(tracing.lme, tracing.middle)
anova(tracing.middle, tracing.none)
summary(tracing.lme)


# test for coloring score
print("For coloring:")
coloring.lme = lmer(COLORING ~ age * AQ * PicType + (1|Child),data = all_coloring_C1C2, REML=FALSE)
coloring.middle = lmer(COLORING ~ age * AQ + PicType + (1|Child) ,data = all_coloring_C1C2, REML=FALSE)
coloring.none = lmer(COLORING ~ age * AQ + (1|Child),data = all_coloring_C1C2, REML=FALSE)
anova(coloring.lme, coloring.middle)
anova(coloring.middle, coloring.none)
summary(coloring.lme)



# Hypothesis 5:
print("")
print("")
print("Hypothesis 5: Children with a higher AQ score will show less variability when drawing on scrambled and non-scrambled conditions.")


# Children with a higher AQ score will show less variability when drawing on scrambled and non-scrambled conditions.

print("Using the version of the test analogous to Hypothesis 4:")

print("For representational drawing:")
repr.lme = lmer(COMP ~ age * AQ * PicType + (1|PicClass),data = all_compl_C2C3, REML=FALSE)
repr.middle = lmer(COMP ~ age * AQ + PicType + (1|PicClass),data = all_compl_C2C3, REML=FALSE)
repr.none = lmer(COMP ~ age * AQ + PicType + (1|PicClass),data = all_compl_C2C3, REML=FALSE)
anova(repr.lme, repr.middle)
anova(repr.middle, repr.none)

print("For representational drawing (separately for age and AQ score):")
# looking for an interaction of age and PicType or AQ and PicType
repr.lme = lmer(COMP ~ age * PicType  + (1|PicClass),data = all_compl_C2C3, REML=FALSE)
repr.middle = lmer(COMP ~ age + PicType + (1|PicClass),data = all_compl_C2C3, REML=FALSE)
repr.none = lmer(COMP ~ age + 1 + (1|PicClass),data = all_compl_C2C3, REML=FALSE)
anova(repr.lme, repr.middle)
anova(repr.middle, repr.none)

repr.lme = lmer(COMP ~ AQ * PicType + (1|PicClass),data = all_compl_C2C3, REML=FALSE)
repr.middle = lmer(COMP ~ AQ + PicType + (1|PicClass),data = all_compl_C2C3, REML=FALSE)
repr.none = lmer(COMP ~ AQ + 1 + (1|PicClass),data = all_compl_C2C3, REML=FALSE)
anova(repr.lme, repr.middle)
anova(repr.middle, repr.none)

print("For scribbling:")
scribble.lme = lmer(SCR ~ age * AQ * PicType + (1|Child),data = all_scribble_C2C3, REML=FALSE)
scribble.middle = lmer(SCR ~ age * AQ + PicType + (1|Child),data = all_scribble_C2C3, REML=FALSE)
scribble.none = lmer(SCR ~ age * AQ + PicType + (1|Child),data = all_scribble_C2C3, REML=FALSE)
anova(scribble.lme, scribble.middle)
anova(scribble.middle, scribble.none)

print("For scribbling (separately for age and AQ score):")
# looking for an interaction of age and PicType or AQ and PicType
scribble.lme = lmer(SCR ~ age * PicType +  (1|Child),data = all_scribble_C2C3, REML=FALSE)
scribble.middle = lmer(SCR ~ age + PicType +  (1|Child),data = all_scribble_C2C3, REML=FALSE)
scribble.none = lmer(SCR ~ age + 1 + (1|Child),data = all_scribble_C2C3, REML=FALSE)
anova(scribble.lme, scribble.middle)
anova(scribble.middle, scribble.none)

scribble.lme = lmer(SCR ~  AQ * PicType + (1|Child),data = all_scribble_C2C3, REML=FALSE)
scribble.middle = lmer(SCR ~ AQ + PicType + (1|Child),data = all_scribble_C2C3, REML=FALSE)
scribble.none = lmer(SCR ~ AQ + 1 + (1|Child),data = all_scribble_C2C3, REML=FALSE)
anova(scribble.lme, scribble.middle)
anova(scribble.middle, scribble.none)


ggplot(all_scribble, aes(x = age, y = SCR, size=10, color=PicType)) + geom_jitter(width=0.2,height=0.2) + geom_smooth(method="lm", se=T) + ggsave("results/R_plot/scribble-age-PicType.png")
ggplot(all_scribble, aes(x = AQ, y = SCR, size=10, color=PicType)) + geom_jitter(width=0.2,height=0.2) + geom_smooth(method="lm", se=T) + ggsave("results/R_plot/scribble-AQ-PicType.png")

ggplot(all_compl, aes(x = age, y = COMP, size=10, color=PicType)) + geom_jitter(width=0.2,height=0.2) + geom_smooth(method="lm", se=T) + ggsave("results/R_plot/compl-age-PicType.png")
ggplot(all_compl, aes(x = AQ, y = COMP, size=10, color=PicType)) + geom_jitter(width=0.2,height=0.2) + geom_smooth(method="lm", se=T) + ggsave("results/R_plot/compl-AQ-PicType.png")

ggplot(all_tracing, aes(x = age, y = TRACING, size=10, color=PicType)) + geom_jitter(width=0.2,height=0.2) + geom_smooth(method="lm", se=T) + ggsave("results/R_plot/tracing-age-PicType.png")
ggplot(all_tracing, aes(x = AQ, y = TRACING, size=10, color=PicType)) + geom_jitter(width=0.2,height=0.2) + geom_smooth(method="lm", se=T) + ggsave("results/R_plot/tracing-AQ-PicType.png")


# => Potential problem: different drawings of one Childs are not necessarily consistent, even if it's the same type of drawing, just taking the overall mean of one Child for one PicType might not be enough to discover regularities in the data
# but the stimuli sets were constructed in a way such that there are two pictures of the same picClass where one is "scrambled" and one is "inner". Try to compare those two, they could be more consistent and interesting.

# Similarity score (NEW VERSION)

scribble_C2_perClass = all_scribble_C2 %>% group_by(Child, age, AQ, PicClass) %>% summarize(MeanScribble = mean(SCR))
scribble_C3_perClass = all_scribble_C3 %>% group_by(Child, age, AQ, PicClass) %>% summarize(MeanScribble = mean(SCR))

# put them in one table and compute difference
scribble_diff = merge(scribble_C2_perClass, scribble_C3_perClass, by=c("Child", "age", "AQ", "PicClass")) %>% group_by(Child, age, AQ, PicClass) %>% summarize(ScribbleDiff = abs(MeanScribble.x - MeanScribble.y))

hyp5.lme = lmer(ScribbleDiff ~ age * AQ + (1|PicClass), data=scribble_diff, REML=FALSE)
hyp5.middle = lmer(ScribbleDiff ~ age + AQ + (1|PicClass), data=scribble_diff, REML=FALSE)
hyp5.none = lmer(ScribbleDiff ~ age + (1|PicClass), data=scribble_diff, REML=FALSE)
anova(hyp5.lme, hyp5.middle)
anova(hyp5.middle, hyp5.none)

ggplot(scribble_diff, aes(x = age, y = ScribbleDiff, size=10, color=AQ)) + geom_jitter(width=0.2,height=0.2) + geom_smooth(method="lm", se=T) + ggsave("results/R_plot/scribblediff-age.png")
ggplot(scribble_diff, aes(x = AQ, y = ScribbleDiff, size=10, color=age)) + geom_jitter(width=0.2,height=0.2) + geom_smooth(method="lm", se=T) + ggsave("results/R_plot/scribblediff-AQ.png")


png("results/R_plot/scribblediff-age.png", width = 350, height = 300)
ggplot(scribble_diff, aes(x = age, y = ScribbleDiff, size=10, color=AQ)) + geom_jitter(width=0.2,height=0.2) + geom_smooth(method="lm", se=T)
dev.off()

png("results/R_plot/scribblediff-AQ.png", width = 350, height = 300)
ggplot(scribble_diff, aes(x = AQ, y = ScribbleDiff, size=10, color=age)) + geom_jitter(width=0.2,height=0.2) + geom_smooth(method="lm", se=T)
dev.off()

# for completion
repr_C2_perClass = all_compl_C2 %>% group_by(Child, age, AQ, PicClass) %>% summarize(MeanRepr = mean(COMP))
repr_C3_perClass = all_compl_C3 %>% group_by(Child, age, AQ, PicClass) %>% summarize(MeanRepr = mean(COMP))

# put them in one table and compute difference
repr_diff = merge(repr_C2_perClass, repr_C3_perClass, by=c("Child", "age", "AQ", "PicClass")) %>% group_by(Child, age, AQ) %>% summarize(ReprDiff = abs(MeanRepr.x - MeanRepr.y))

# hyp5.lme = lmer(ReprDiff ~ age * AQ + (1|PicClass) + (1|PictureIdx.x), data=repr_diff, REML=FALSE)
# singular, so =>
hyp5.lme = lm(ReprDiff ~ age * AQ, data=repr_diff)
hyp5.middle = lm(ReprDiff ~ age + AQ, data=repr_diff)
hyp5.none = lm(ReprDiff ~ age, data=repr_diff)
anova(hyp5.lme, hyp5.middle)
anova(hyp5.middle, hyp5.none)

png("results/R_plot/reprdiff-age.png", width = 350, height = 300)
ggplot(repr_diff, aes(x = age, y = ReprDiff, size=10, color=AQ)) + geom_point(width=0.2,height=0.2) + geom_smooth(method="lm", se=T)
dev.off()

png("results/R_plot/reprdiff-AQ.png", width = 350, height = 300)
ggplot(repr_diff, aes(x = AQ, y = ReprDiff, size=10, color=age)) + geom_point(width=0.2,height=0.2) + geom_smooth(method="lm", se=T)
dev.off()

repr_diff_sorted = repr_diff %>% arrange(desc(ReprDiff))
write.csv(repr_diff_sorted, file="results/python_csv/repr_diff_sorted.csv")


### test for hypothesis 1+2 for the separate PicClass categories
print("test for hypothesis 1+2 for the separate PicClass categories")

print("for whole data set")
repr.lme = lmer(COMP ~ age + (1|Child) + (1|WorkerId) + (1|PicType) + (1|PicClass), data = all_compl, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child) + (1|WorkerId) + (1|PicType) + (1|PicClass), data = all_compl, REML=FALSE)
print("Influence of age?")
anova(repr.lme, repr.none)

# for C1
print("Hypothesis 1 – for C1")
repr.lme = lmer(COMP ~ age + (1|PicClass),data = all_compl_C1, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|PicClass), data = all_compl_C1,  REML=FALSE)
print("Influence of age?")
anova(repr.lme, repr.none)

# for C2
print("Hypothesis 1 – for C2")
repr.lme = lmer(COMP ~ age + (1|PicClass),data = all_compl_C2, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|PicClass),data = all_compl_C2, REML=FALSE)
print("Influence of age?")
anova(repr.lme, repr.none)

# for C3
print("Hypothesis 1 – for C3")
repr.lme = lmer(COMP ~ age + (1|Child),data = all_compl_C3, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child),data = all_compl_C3, REML=FALSE)
print("Influence of age?")
anova(repr.lme, repr.none)


print("face")

# for C1
print("Hypothesis 1 – for C1")
data = all_compl_C1 %>% filter(PicClass=="class-F")
repr.lme = lmer(COMP ~ age + (1|Child),data = data, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child), data = data,  REML=FALSE)
print("Influence of age?")
anova(repr.lme, repr.none)

# for C2
print("Hypothesis 1 – for C2")
data = all_compl_C2 %>% filter(PicClass=="class-F")
repr.lme = lmer(COMP ~ age + (1|Child),data = data, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child),data = data, REML=FALSE)
print("Influence of age?")
anova(repr.lme, repr.none)

# for C3
print("Hypothesis 1 – for C3")
data = all_compl_C3 %>% filter(PicClass=="class-F")
repr.lme = lmer(COMP ~ age + (1|Child),data = data, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child),data = data, REML=FALSE)
print("Influence of age?")
anova(repr.lme, repr.none)

print("human")

# for C1
print("Hypothesis 1 – for C1")
data = all_compl_C1 %>% filter(PicClass=="class-M")
repr.lme = lmer(COMP ~ age + (1|Child),data = data, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child), data = data,  REML=FALSE)
print("Influence of age?")
anova(repr.lme, repr.none)

# for C2
print("Hypothesis 1 – for C2")
data = all_compl_C2 %>% filter(PicClass=="class-M")
repr.lme = lmer(COMP ~ age + (1|Child),data = data, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child),data = data, REML=FALSE)
print("Influence of age?")
anova(repr.lme, repr.none)

# for C3
print("Hypothesis 1 – for C3")
data = all_compl_C3 %>% filter(PicClass=="class-M")
repr.lme = lmer(COMP ~ age + (1|Child),data = data, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child),data = data, REML=FALSE)
print("Influence of age?")
anova(repr.lme, repr.none)


print("car")

# for C1
print("Hypothesis 1 – for C1")
data = all_compl_C1 %>% filter(PicClass=="class-C")
repr.lme = lmer(COMP ~ age + (1|Child),data = data, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child), data = data,  REML=FALSE)
print("Influence of age?")
anova(repr.lme, repr.none)

# for C2
print("Hypothesis 1 – for C2")
data = all_compl_C2 %>% filter(PicClass=="class-C")
repr.lme = lmer(COMP ~ age + (1|Child),data = data, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child),data = data, REML=FALSE)
print("Influence of age?")
anova(repr.lme, repr.none)

# for C3
print("Hypothesis 1 – for C3")
data = all_compl_C3 %>% filter(PicClass=="class-C")
repr.lme = lmer(COMP ~ age + (1|Child),data = data, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child),data = data, REML=FALSE)
print("Influence of age?")
anova(repr.lme, repr.none)

print("house")

# for C1
print("Hypothesis 1 – for C1")
data = all_compl_C1 %>% filter(PicClass=="class-H")
repr.lme = lmer(COMP ~ age + (1|Child),data = data, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child), data = data,  REML=FALSE)
print("Influence of age?")
anova(repr.lme, repr.none)

# for C2
print("Hypothesis 1 – for C2")
data = all_compl_C2 %>% filter(PicClass=="class-H")
repr.lme = lmer(COMP ~ age + (1|Child),data = data, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child),data = data, REML=FALSE)
print("Influence of age?")
anova(repr.lme, repr.none)

# for C3
print("Hypothesis 1 – for C3")
data = all_compl_C3 %>% filter(PicClass=="class-H")
repr.lme = lmer(COMP ~ age + (1|Child),data = data, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child),data = data, REML=FALSE)
print("Influence of age?")
anova(repr.lme, repr.none)


print("for the whole data set")

scribble.lme = lmer(SCR ~ age + (1|Child) + (1|PicType) + (1|PicClass),data = all_scribble, REML=FALSE)
scribble.none = lmer(SCR ~ 1 + (1|Child) + (1|PicType) + (1|PicClass),data = all_scribble, REML=FALSE)
anova(scribble.lme, scribble.none)

# for C1
print("Hypothesis 2 – for C1")
scribble.lme = lmer(SCR ~ age  + (1|PicClass),data = all_scribble_C1, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|PicClass),data = all_scribble_C1, REML=FALSE)
print("Influence of age?")
anova(scribble.lme, scribble.none)

# for C2
print("Hypothesis 2 – for C2")
scribble.lme = lmer(SCR ~ age + (1|Child),data = all_scribble_C2, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|Child),data = all_scribble_C2, REML=FALSE)
print("Influence of age?")
anova(scribble.lme, scribble.none)

# for C3
print("Hypothesis 2 – for C3")
scribble.lme = lmer(SCR ~ age + (1|Child),data = all_scribble_C3, REML=FALSE)
scribble.none = lmer(SCR ~ 1 + (1|Child),data = all_scribble_C3, REML=FALSE)
print("Influence of age?")
anova(scribble.lme, scribble.none)


print("face only")

# for C1
print("Hypothesis 2 – for C1")
data = all_scribble_C1 %>% filter(PicClass=="class-F")
scribble.lme = lmer(SCR ~ age  + (1|Child),data = data, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|Child),data = data, REML=FALSE)
print("Influence of age?")
anova(scribble.lme, scribble.none)

# for C2
print("Hypothesis 2 – for C2")
data = all_scribble_C2 %>% filter(PicClass=="class-F")
scribble.lme = lmer(SCR ~ age + (1|Child),data = data, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|Child),data = data, REML=FALSE)
print("Influence of age?")
anova(scribble.lme, scribble.none)

# for C3
print("Hypothesis 2 – for C3")
data = all_scribble_C3 %>% filter(PicClass=="class-F")
scribble.lme = lmer(SCR ~ age + (1|Child),data = data, REML=FALSE)
scribble.none = lmer(SCR ~ 1 + (1|Child),data = data, REML=FALSE)
print("Influence of age?")
anova(scribble.lme, scribble.none)


print("car only")

# for C1
print("Hypothesis 2 – for C1")
data = all_scribble_C1 %>% filter(PicClass=="class-C")
scribble.lme = lmer(SCR ~ age  + (1|Child),data = data, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|Child),data = data, REML=FALSE)
print("Influence of age?")
anova(scribble.lme, scribble.none)

# for C2
print("Hypothesis 2 – for C2")
data = all_scribble_C2 %>% filter(PicClass=="class-C")
scribble.lme = lmer(SCR ~ age + (1|Child),data = data, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|Child),data = data, REML=FALSE)
print("Influence of age?")
anova(scribble.lme, scribble.none)

# for C3
print("Hypothesis 2 – for C3")
data = all_scribble_C3 %>% filter(PicClass=="class-C")
scribble.lme = lmer(SCR ~ age + (1|Child),data = data, REML=FALSE)
scribble.none = lmer(SCR ~ 1 + (1|Child),data = data, REML=FALSE)
print("Influence of age?")
anova(scribble.lme, scribble.none)



print("human only")

# for C1
print("Hypothesis 2 – for C1")
data = all_scribble_C1 %>% filter(PicClass=="class-M")
scribble.lme = lmer(SCR ~ age  + (1|Child),data = data, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|Child),data = data, REML=FALSE)
print("Influence of age?")
anova(scribble.lme, scribble.none)

# for C2
print("Hypothesis 2 – for C2")
data = all_scribble_C2 %>% filter(PicClass=="class-M")
scribble.lme = lmer(SCR ~ age + (1|Child),data = data, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|Child),data = data, REML=FALSE)
print("Influence of age?")
anova(scribble.lme, scribble.none)

# for C3
print("Hypothesis 2 – for C3")
data = all_scribble_C3 %>% filter(PicClass=="class-M")
scribble.lme = lmer(SCR ~ age + (1|Child),data = data, REML=FALSE)
scribble.none = lmer(SCR ~ 1 + (1|Child),data = data, REML=FALSE)
print("Influence of age?")
anova(scribble.lme, scribble.none)



print("house only")

# for C1
print("Hypothesis 2 – for C1")
data = all_scribble_C1 %>% filter(PicClass=="class-H")
scribble.lme = lmer(SCR ~ age  + (1|Child),data = data, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|Child),data = data, REML=FALSE)
print("Influence of age?")
anova(scribble.lme, scribble.none)

# for C2
print("Hypothesis 2 – for C2")
data = all_scribble_C2 %>% filter(PicClass=="class-H")
scribble.lme = lmer(SCR ~ age + (1|Child),data = data, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|Child),data = data, REML=FALSE)
print("Influence of age?")
anova(scribble.lme, scribble.none)

# for C3
print("Hypothesis 2 – for C3")
data = all_scribble_C3 %>% filter(PicClass=="class-H")
scribble.lme = lmer(SCR ~ age + (1|Child),data = data, REML=FALSE)
scribble.none = lmer(SCR ~ 1 + (1|Child),data = data, REML=FALSE)
print("Influence of age?")
anova(scribble.lme, scribble.none)



print("SAME TESTS FOR AQ")


print("for whole data set")
repr.lme = lmer(COMP ~ AQ + (1|PicType) + (1|PicClass), data = all_compl, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|PicType) + (1|PicClass), data = all_compl, REML=FALSE)
print("Influence of AQ?")
anova(repr.lme, repr.none)

# for C1
print("Hypothesis 3 – for C1")
repr.lme = lmer(COMP ~ AQ + (1|PicClass),data = all_compl_C1, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|PicClass), data = all_compl_C1,  REML=FALSE)
print("Influence of AQ?")
anova(repr.lme, repr.none)

# for C2
print("Hypothesis 3 – for C2")
repr.lme = lmer(COMP ~ AQ + (1|PicClass),data = all_compl_C2, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|PicClass),data = all_compl_C2, REML=FALSE)
print("Influence of AQ?")
anova(repr.lme, repr.none)

# for C3
print("Hypothesis 3 – for C3")
repr.lme = lmer(COMP ~ AQ + (1|Child),data = all_compl_C3, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child),data = all_compl_C3, REML=FALSE)
print("Influence of AQ?")
anova(repr.lme, repr.none)


print("face")

# for C1
print("Hypothesis 3 – for C1")
data = all_compl_C1 %>% filter(PicClass=="class-F")
repr.lme = lmer(COMP ~ AQ + 1 + (1|Child),data = data, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child),data = data, REML=FALSE)
print("Influence of AQ?")
anova(repr.lme, repr.none)

# for C2
print("Hypothesis 3 – for C2")
data = all_compl_C2 %>% filter(PicClass=="class-F")
repr.lme = lmer(COMP ~ AQ + 1 + (1|Child),data = data, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child),data = data, REML=FALSE)
print("Influence of AQ?")
anova(repr.lme, repr.none)

# for C3
print("Hypothesis 3 – for C3")
data = all_compl_C3 %>% filter(PicClass=="class-F")
repr.lme = lmer(COMP ~ AQ + 1 + (1|Child),data = data, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child),data = data, REML=FALSE)
print("Influence of AQ?")
anova(repr.lme, repr.none)

print("human")

# for C1
print("Hypothesis 3 – for C1")
data = all_compl_C1 %>% filter(PicClass=="class-M")
repr.lme = lmer(COMP ~ AQ + 1 + (1|Child),data = data, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child),data = data, REML=FALSE)
print("Influence of AQ?")
anova(repr.lme, repr.none)

# for C2
print("Hypothesis 3 – for C2")
data = all_compl_C2 %>% filter(PicClass=="class-M")
repr.lme = lmer(COMP ~ AQ + 1 + (1|Child),data = data, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child),data = data, REML=FALSE)
print("Influence of AQ?")
anova(repr.lme, repr.none)

# for C3
print("Hypothesis 3 – for C3")
data = all_compl_C3 %>% filter(PicClass=="class-M")
repr.lme = lmer(COMP ~ AQ + 1 + (1|Child),data = data, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child),data = data, REML=FALSE)
print("Influence of AQ?")
anova(repr.lme, repr.none)


print("car")

# for C1
print("Hypothesis 3 – for C1")
data = all_compl_C1 %>% filter(PicClass=="class-C")
repr.lme = lmer(COMP ~ AQ + 1 + (1|Child),data = data, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child),data = data, REML=FALSE)
print("Influence of AQ?")
anova(repr.lme, repr.none)

# for C2
print("Hypothesis 3 – for C2")
data = all_compl_C2 %>% filter(PicClass=="class-C")
repr.lme = lmer(COMP ~ AQ + 1 + (1|Child),data = data, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child),data = data, REML=FALSE)
print("Influence of AQ?")
anova(repr.lme, repr.none)

# for C3
print("Hypothesis 3 – for C3")
data = all_compl_C3 %>% filter(PicClass=="class-C")
repr.lme = lmer(COMP ~ AQ + 1 + (1|Child),data = data, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child),data = data, REML=FALSE)
print("Influence of AQ?")
anova(repr.lme, repr.none)

print("house")

# for C1
print("Hypothesis 3 – for C1")
data = all_compl_C1 %>% filter(PicClass=="class-H")
repr.lme = lmer(COMP ~ AQ + 1 + (1|Child),data = data, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child),data = data, REML=FALSE)
print("Influence of AQ?")
anova(repr.lme, repr.none)

# for C2
print("Hypothesis 3 – for C2")
data = all_compl_C2 %>% filter(PicClass=="class-H")
repr.lme = lmer(COMP ~ AQ + 1 + (1|Child),data = data, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child),data = data, REML=FALSE)
print("Influence of AQ?")
anova(repr.lme, repr.none)

# for C3
print("Hypothesis 3 – for C3")
data = all_compl_C3 %>% filter(PicClass=="class-H")
repr.lme = lmer(COMP ~ AQ + 1 + (1|Child),data = data, REML=FALSE)
repr.none = lmer(COMP ~ 1 + (1|Child),data = data, REML=FALSE)
print("Influence of AQ?")
anova(repr.lme, repr.none)


# for the whole data set
scribble.lme = lmer(SCR ~ AQ + (1|PicType) + (1|PicClass),data = all_scribble, REML=FALSE)
scribble.none = lmer(SCR ~ 1 + (1|PicType) + (1|PicClass),data = all_scribble, REML=FALSE)
anova(scribble.lme, scribble.none)


print("for the whole data set")

# for C1
print("Hypothesis 3b – for C1")
scribble.lme = lmer(SCR ~ AQ + 1 + (1|PicClass),data = all_scribble_C1, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|PicClass),data = all_scribble_C1, REML=FALSE)
print("Influence of AQ?")
anova(scribble.lme, scribble.none)

# for C2
print("Hypothesis 3b – for C2")
scribble.lme = lmer(SCR ~ AQ + 1 + (1|PicClass),data = all_scribble_C1, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|PicClass),data = all_scribble_C1, REML=FALSE)
print("Influence of AQ?")
anova(scribble.lme, scribble.none)

# for C3
print("Hypothesis 3b – for C3")
scribble.lme = lmer(SCR ~ AQ + 1 + (1|PicClass),data = all_scribble_C1, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|PicClass),data = all_scribble_C1, REML=FALSE)
print("Influence of AQ?")
anova(scribble.lme, scribble.none)


print("face only")

# for C1
print("Hypothesis 3b – for C1")
data = all_scribble_C1 %>% filter(PicClass=="class-F")
scribble.lme = lmer(SCR ~ AQ + 1 + (1|PicClass),data = all_scribble_C1, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|PicClass),data = all_scribble_C1, REML=FALSE)
print("Influence of AQ?")
anova(scribble.lme, scribble.none)

# for C2
print("Hypothesis 3b – for C2")
data = all_scribble_C2 %>% filter(PicClass=="class-F")
scribble.lme = lmer(SCR ~ AQ + 1 + (1|PicClass),data = all_scribble_C1, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|PicClass),data = all_scribble_C1, REML=FALSE)
print("Influence of AQ?")
anova(scribble.lme, scribble.none)

# for C3
print("Hypothesis 3b2 – for C3")
data = all_scribble_C3 %>% filter(PicClass=="class-F")
scribble.lme = lmer(SCR ~ AQ + 1 + (1|PicClass),data = all_scribble_C1, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|PicClass),data = all_scribble_C1, REML=FALSE)
print("Influence of AQ?")
anova(scribble.lme, scribble.none)

print("car only")

# for C1
print("Hypothesis 3b – for C1")
data = all_scribble_C1 %>% filter(PicClass=="class-C")
scribble.lme = lmer(SCR ~ AQ + 1 + (1|PicClass),data = all_scribble_C1, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|PicClass),data = all_scribble_C1, REML=FALSE)
print("Influence of AQ?")
anova(scribble.lme, scribble.none)

# for C2
print("Hypothesis 3b – for C2")
data = all_scribble_C2 %>% filter(PicClass=="class-C")
scribble.lme = lmer(SCR ~ AQ + 1 + (1|PicClass),data = all_scribble_C1, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|PicClass),data = all_scribble_C1, REML=FALSE)
print("Influence of AQ?")
anova(scribble.lme, scribble.none)

# for C3
print("Hypothesis 3b – for C3")
data = all_scribble_C3 %>% filter(PicClass=="class-C")
scribble.lme = lmer(SCR ~ AQ + 1 + (1|PicClass),data = all_scribble_C1, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|PicClass),data = all_scribble_C1, REML=FALSE)
print("Influence of AQ?")
anova(scribble.lme, scribble.none)


print("Hypothesis 3b – for C2")
data = all_compl_C2 %>% filter(PicClass=="class-F")
scribble.lme = lmer(SCR ~ AQ + 1 + (1|PicClass),data = all_scribble_C1, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|PicClass),data = all_scribble_C1, REML=FALSE)
print("Influence of AQ?")
anova(repr.lme, repr.none)

print("human only")

# for C1
print("Hypothesis 3b – for C1")
data = all_scribble_C1 %>% filter(PicClass=="class-M")
scribble.lme = lmer(SCR ~ AQ + 1 + (1|PicClass),data = all_scribble_C1, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|PicClass),data = all_scribble_C1, REML=FALSE)
print("Influence of AQ?")
anova(scribble.lme, scribble.none)

# for C2
print("Hypothesis 3b – for C2")
data = all_scribble_C2 %>% filter(PicClass=="class-M")
scribble.lme = lmer(SCR ~ AQ + 1 + (1|PicClass),data = all_scribble_C1, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|PicClass),data = all_scribble_C1, REML=FALSE)
print("Influence of AQ?")
anova(scribble.lme, scribble.none)

# for C3
print("Hypothesis 3b – for C3")
data = all_scribble_C3 %>% filter(PicClass=="class-M")
scribble.lme = lmer(SCR ~ AQ + 1 + (1|PicClass),data = all_scribble_C1, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|PicClass),data = all_scribble_C1, REML=FALSE)
print("Influence of AQ?")
anova(scribble.lme, scribble.none)

print("house only")

# for C1
print("Hypothesis 3b – for C1")
data = all_scribble_C1 %>% filter(PicClass=="class-H")
scribble.lme = lmer(SCR ~ AQ + 1 + (1|PicClass),data = all_scribble_C1, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|PicClass),data = all_scribble_C1, REML=FALSE)
print("Influence of AQ?")
anova(scribble.lme, scribble.none)

# for C2
print("Hypothesis 3b – for C2")
data = all_scribble_C2 %>% filter(PicClass=="class-H")
scribble.lme = lmer(SCR ~ AQ + 1 + (1|PicClass),data = all_scribble_C1, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|PicClass),data = all_scribble_C1, REML=FALSE)
print("Influence of AQ?")
anova(scribble.lme, scribble.none)

# for C3
print("Hypothesis 3b – for C3")
data = all_scribble_C3 %>% filter(PicClass=="class-H")
scribble.lme = lmer(SCR ~ AQ + 1 + (1|PicClass),data = all_scribble_C1, REML=FALSE)
scribble.none = lmer(SCR ~ 1  + (1|PicClass),data = all_scribble_C1, REML=FALSE)
print("Influence of AQ?")
anova(scribble.lme, scribble.none)

