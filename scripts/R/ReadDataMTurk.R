library(tidyverse)
library(lme4)

#read in participant's ratings
data_path <- "data/mturk/"   
files <- dir(data_path, pattern = "*.csv")

for (i in 1:length(files)){
  print(i)
  subject = read.csv(paste(data_path,files[i],sep=""),fill=TRUE,stringsAsFactors=FALSE)[ ,c('WorkerId', 'WorkTimeInSeconds', 'LifetimeApprovalRate','Input.image_url', 'Answer.Car.Car', 'Answer.Face.Face', 'Answer.House.House', 'Answer.Human.Human', 'Answer.NoAnswer.NoAnswer', 'Answer.QColoring', 'Answer.QCompleteOut', 'Answer.QCompleteParts', 'Answer.QRelated', 'Answer.QScribbleBlank', 'Answer.QScribbleDrawing', 'Answer.QTracing')]#'Answer.QSomething', 'Answer.QTracing')]
  if (i == 1) {
    write.table(subject, file="data/all-mturk.txt", col.names=TRUE, append = FALSE,row.names=FALSE, sep="\t") 
  } else
  { 
    write.table(subject, file="data/all-mturk.txt", append = TRUE, col.names = FALSE, row.names=FALSE, sep="\t")
  }
  i = i +1
}


#read in full dataset
all_ratings = read.table("data/all-mturk.txt",header=T)
all_ratings$Input.image_url = as.character(all_ratings$Input.image_url)
all_ratings$image_url = all_ratings$Input.image_url
all_ratings = all_ratings %>% 
  separate(Input.image_url,c("Child","PictureIdx","PicType", "PicClassImage"),sep = "_") %>%
  separate(PicClassImage,c("PicClass","FileExt"),sep = "[.]")

# participant=>Child, "Picture"=>"PictureIdx"

# Check the reliability of the ratings and filter out bad ones

# find workers which classify wrong most of the time
# generate a list of each worker id, the number of items they filled in, and the number of misclassified items

# Get all correctly classified images
all_ratings_correct = all_ratings %>% filter(PicClass == "class-F" & Answer.Face.Face == "true" | PicClass == "class-H" & Answer.House.House == "true" | PicClass == "class-C" & Answer.Car.Car == "true" | PicClass == "class-M" & Answer.Human.Human == "true")

all_ratings_correct %>% count("Worker")


# get the number of images per worker in all_ratings and all_ratings_correct to get the reliability

for (i in 1:nrow(all_ratings)) {
    all_ratings$workerTotal[i] = nrow(all_ratings %>% filter(WorkerId==all_ratings$WorkerId[i]))
    all_ratings$workerCorrect[i] = nrow(all_ratings_correct %>% filter(WorkerId==all_ratings$WorkerId[i]))
    all_ratings$correctPercentage[i] = all_ratings$workerCorrect[i] / all_ratings$workerTotal[i]
    all_ratings$aboveThreshold[i] = all_ratings$correctPercentage[i] > 0.7
}

# get a list of all workers
#workers = all_ratings %>% group_by(WorkerId) %>% summarize()

# display the number of replies for each worker
#tableAll = summary(all_ratings$WorkerId)
#tableCorrect = summary(all_ratings_correct$WorkerId)
#(tableAll - tableCorrect) / tableAll

# MTurk: add columns ToApprove and ToReject to the table and store it

all_ratings$ToApprove = (all_ratings$PicClass=="class-F" & all_ratings$Answer.Face.Face == "true") | (all_ratings$PicClass == "class-H" & all_ratings$Answer.House.House == "true") | (all_ratings$PicClass=="class-C" & all_ratings$Answer.Car.Car == "true") | (all_ratings$PicClass=="class-M" & all_ratings$Answer.Human.Human == "true")

#all_ratings$workerTotal = tableAll[all_ratings$WorkerId]
#all_ratings$workerTotalCorrect = tableCorrect[all_ratings$WorkerId]
all_ratings$approve[all_ratings$ToApprove==T] = 'x'
write.csv(all_ratings, 'approveInfo.csv')

# copy questionable ones into a folder for easier inspection

#questionable_images = all_ratings$image_url[all_ratings$ToApprove==F]
#for (i in questionable_images) {
#    print(i)
#    file.copy(file.path('/home/anja/Documents/Experiments/DrawingExperiment/ResultsData/RatingData/Drawings_for_rating/ONLINE-RATING/', i), '/home/anja/github/MiraikanDrawing/images_to_inspect/')
#}

exclude_ratings = all_ratings %>% filter(aboveThreshold==F)
re_rate_images = exclude_ratings %>% group_by(image_url) %>% summarize()
re_rate_images$times = 0
# count for each rerating image, how many times it has to be rerated
for (i in 1:nrow(re_rate_images)) {
    re_rate_images$times[i] = nrow(exclude_ratings %>% filter(image_url==exclude_ratings$image_url[i]))
}
write.csv(re_rate_images, 'rerating.csv')

# use ratings of people only which are above threshold of 70% accuracy for the
all_ratings = all_ratings %>% filter(aboveThreshold==T)

# one image where a child drew nothing was accidently rated, exclude that rating
# P80_image-3_type-S_class-F.JPG
# but keep the automatically added zero-ratings for images where child did not draw anything
all_ratings = all_ratings %>% filter(!(Child=="P80" & PicClass=="class-F" & PicType=="type-S" & WorkerId != "AUTO"))

# link to children's age and AQ score values

#context = read.table("data/participants-age-aq.txt",header=T)
participants = read.table("data/participants-age-aq.txt",header=T)
participants_with_aq = participants %>%
    filter(numQuestions > 39)
# filter male/female
participants_m = participants_with_aq %>% filter(gender == "Male")
participants_f = participants_with_aq %>% filter(gender == "Female")
# filter participants by age
participants_older = participants_with_aq %>%
    filter(age >= 48)

# Only take into account participants with AQ score (>80%)
all = merge(all_ratings,participants,by="Child",all_ratings.x=F)
#all = merge(all_ratings,participants_older,by="Child",all_ratings.x=F)

# only male/female
all_m = merge(all,participants_m,by="Child",all_ratings.x=F)
all_f = merge(all,participants_f,by="Child",all_ratings.x=F)

### Define the scores for scribbling and representational drawing

#QuestionList 2/3: Scribbling score
all_scribble = cbind(all, SCR=apply(all[,18:19],1,mean,na.rm=T))

# QuestionList 6/7: Representational drawing score
all_compl = cbind(all, COMP=apply(all[,15:16],1,mean,na.rm=T))

# related/meaningful
all_meaning = all
all_meaning$MEANING = all_meaning$Answer.QRelated

all_tracing = all
all_tracing$TRACING = all_tracing$Answer.QTracing

all_coloring = all
all_coloring$COLORING = all_coloring$Answer.QColoring



all_scribble %>% write.csv(.,file = "data/all_scribble.csv")
all_compl %>% write.csv(.,file = "data/all_compl.csv")
all_meaning %>% write.csv(.,file = "data/all_meaning.csv")

# prepare subsets of the data according to picture type
all_scribble_C1C2 = all_scribble %>%
  ungroup() %>%
  filter(PicType == "type-O" | PicType == "type-I")

all_scribble_C1C3 = all_scribble %>%
  ungroup() %>%
  filter(PicType == "type-O" | PicType == "type-S")

all_scribble_C2C3 = all_scribble %>%
  ungroup() %>%
  filter(PicType == "type-I" | PicType == "type-S")

all_scribble_C1 = all_scribble %>%
  ungroup() %>%
  filter(PicType == "type-O")

all_scribble_C2 = all_scribble %>%
  ungroup() %>%
  filter(PicType == "type-I")

all_scribble_C3 = all_scribble %>%
  ungroup() %>%
  filter(PicType == "type-S")

all_compl_C1C2 = all_compl %>%
  ungroup() %>%
  filter(PicType == "type-O" | PicType == "type-I")

all_compl_C1C3 = all_compl %>%
  ungroup() %>%
  filter(PicType == "type-O" | PicType == "type-S")

all_compl_C2C3 = all_compl %>%
  ungroup() %>%
  filter(PicType == "type-I" | PicType == "type-S")

all_compl_C1 = all_compl %>%
  ungroup() %>%
  filter(PicType == "type-O")

all_compl_C2 = all_compl %>%
  ungroup() %>%
  filter(PicType == "type-I")

all_compl_C3 = all_compl %>%
  ungroup() %>%
  filter(PicType == "type-S")

all_tracing_C1C2 = all_tracing %>%
  ungroup() %>%
  filter(PicType == "type-O" | PicType == "type-I")

all_coloring_C1C2 = all_coloring %>%
  ungroup() %>%
  filter(PicType == "type-O" | PicType == "type-I")



#ggplot(all_scribble, aes(x = age, y = SCR, size=10, color=PicType)) + geom_jitter(width=1,height=1) + geom_smooth(method="lm", se=T) + ggsave("results/R_plot/scribble-age-PicType.png")
#ggplot(all_scribble, aes(x = AQ, y = SCR, size=10, color=PicType)) + geom_jitter(width=1,height=1) + geom_smooth(method="lm", se=T) + ggsave("results/R_plot/scribble-AQ-PicType.png")

#ggplot(all_compl, aes(x = age, y = COMP, size=10, color=PicType)) + geom_jitter(width=1,height=1) + geom_smooth(method="lm", se=T) + ggsave("results/R_plot/compl-age-PicType.png")
#ggplot(all_compl, aes(x = AQ, y = COMP, size=10, color=PicType)) + geom_jitter(width=1,height=1) + geom_smooth(method="lm", se=T) + ggsave("results/R_plot/compl-AQ-PicType.png")

#ggplot(all_tracing, aes(x = age, y = TRACING, size=10, color=PicType)) + geom_jitter(width=1,height=1) + geom_smooth(method="lm", se=T) + ggsave("results/R_plot/tracing-age-PicType.png")
#ggplot(all_tracing, aes(x = AQ, y = TRACING, size=10, color=PicType)) + geom_jitter(width=1,height=1) + geom_smooth(method="lm", se=T) + ggsave("results/R_plot/tracing-AQ-PicType.png")


### Plots for ICDL20 paper ###

pdf("results/R_plot/scribble-outer-age.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_scribble %>% filter(PicType == 'type-O') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, age) %>% summarize(MeanSCR = mean(SCR)), aes(x = age, y = MeanSCR, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Outline presented", x = "Age (in months)", y = "Scribbling score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

pdf("results/R_plot/scribble-inner-age.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_scribble %>% filter(PicType == 'type-I') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, age) %>% summarize(MeanSCR = mean(SCR)), aes(x = age, y = MeanSCR, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Inner features presented", x = "Age (in months)", y = "Scribbling score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

pdf("results/R_plot/scribble-scram-age.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_scribble %>% filter(PicType == 'type-S') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, age) %>% summarize(MeanSCR = mean(SCR)), aes(x = age, y = MeanSCR, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Scrambled inner features presented", x = "Age (in months)", y = "Scribbling score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()


pdf("results/R_plot/compl-outer-age.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_compl %>% filter(PicType == 'type-O') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, age) %>% summarize(MeanCOMP = mean(COMP)), aes(x = age, y = MeanCOMP, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Outline presented", x = "Age (in months)", y = "Completion score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()
# For adjusting the colors of legend
# scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("blue", "red", "green", "orange"))

pdf("results/R_plot/compl-inner-age.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_compl %>% filter(PicType == 'type-I') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, age) %>% summarize(MeanCOMP = mean(COMP)), aes(x = age, y = MeanCOMP, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Inner features presented", x = "Age (in months)", y = "Completion score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

pdf("results/R_plot/compl-scram-age.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_compl %>% filter(PicType == 'type-S') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, age) %>% summarize(MeanCOMP = mean(COMP)), aes(x = age, y = MeanCOMP, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Scrambled inner features presented", x = "Age (in months)", y = "Completion score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()



### Additionally: corresponding plots for AQ score

pdf("results/R_plot/scribble-outer-AQ.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_scribble %>% filter(PicType == 'type-O') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, AQ) %>% summarize(MeanSCR = mean(SCR)), aes(x = AQ, y = MeanSCR, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Outline presented", x = "AQ score", y = "Scribbling score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

pdf("results/R_plot/scribble-inner-AQ.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_scribble %>% filter(PicType == 'type-I') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, AQ) %>% summarize(MeanSCR = mean(SCR)), aes(x = AQ, y = MeanSCR, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Inner features presented", x = "AQ score", y = "Scribbling score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

pdf("results/R_plot/scribble-scram-AQ.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_scribble %>% filter(PicType == 'type-S') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, AQ) %>% summarize(MeanSCR = mean(SCR)), aes(x = AQ, y = MeanSCR, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Scrambled inner features presented", x = "AQ score", y = "Scribbling score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()


pdf("results/R_plot/compl-outer-AQ.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_compl %>% filter(PicType == 'type-O') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, AQ) %>% summarize(MeanCOMP = mean(COMP)), aes(x = AQ, y = MeanCOMP, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Outline presented", x = "AQ score", y = "Completion score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()
# For adjusting the colors of legend
# scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("blue", "red", "green", "orange"))

pdf("results/R_plot/compl-inner-AQ.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_compl %>% filter(PicType == 'type-I') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, AQ) %>% summarize(MeanCOMP = mean(COMP)), aes(x = AQ, y = MeanCOMP, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Inner features presented", x = "AQ score", y = "Completion score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

pdf("results/R_plot/compl-scram-AQ.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_compl %>% filter(PicType == 'type-S') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, AQ) %>% summarize(MeanCOMP = mean(COMP)), aes(x = AQ, y = MeanCOMP, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Scrambled inner features presented", x = "AQ score", y = "Completion score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()



### Additionally: corresponding plots for AQ score for subscales

pdf("results/R_plot/scribble-outer-detailS.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_scribble %>% filter(PicType == 'type-O') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, detailS) %>% summarize(MeanSCR = mean(SCR)), aes(x = detailS, y = MeanSCR, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Outline presented", x = "AQ score", y = "Scribbling score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

pdf("results/R_plot/scribble-inner-detailS.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_scribble %>% filter(PicType == 'type-I') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, detailS) %>% summarize(MeanSCR = mean(SCR)), aes(x = detailS, y = MeanSCR, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Inner features presented", x = "AQ score", y = "Scribbling score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

pdf("results/R_plot/scribble-scram-detailS.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_scribble %>% filter(PicType == 'type-S') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, detailS) %>% summarize(MeanSCR = mean(SCR)), aes(x = detailS, y = MeanSCR, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Scrambled inner features presented", x = "AQ score", y = "Scribbling score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()


pdf("results/R_plot/compl-outer-detailS.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_compl %>% filter(PicType == 'type-O') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, detailS) %>% summarize(MeanCOMP = mean(COMP)), aes(x = detailS, y = MeanCOMP, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Outline presented", x = "AQ score", y = "Completion score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()
# For adjusting the colors of legend
# scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("blue", "red", "green", "orange"))

pdf("results/R_plot/compl-inner-detailS.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_compl %>% filter(PicType == 'type-I') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, detailS) %>% summarize(MeanCOMP = mean(COMP)), aes(x = detailS, y = MeanCOMP, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Inner features presented", x = "AQ score", y = "Completion score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

pdf("results/R_plot/compl-scram-detailS.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_compl %>% filter(PicType == 'type-S') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, detailS) %>% summarize(MeanCOMP = mean(COMP)), aes(x = detailS, y = MeanCOMP, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Scrambled inner features presented", x = "AQ score", y = "Completion score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

### plot age vs. AQ score

pdf("results/R_plot/age-vs-AQ.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(participants_with_aq, aes(x = age, y = AQ, color=gender)) + geom_point(size=3) + labs(title = "", x = "Age (in months)", y = "AQ score", color = "") + scale_color_manual(labels = c("Female", "Male"), values = c("#9f9f9f", "#222222")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

### plot for other drawing styles

# Tracing

pdf("results/R_plot/tracing-outer-age.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_tracing %>% filter(PicType == 'type-O') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, age) %>% summarize(MeanTracing = mean(TRACING)), aes(x = age, y = MeanTracing, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Outline presented", x = "Age (in months)", y = "Tracing score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

pdf("results/R_plot/tracing-inner-age.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_tracing %>% filter(PicType == 'type-I') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, age) %>% summarize(MeanTracing = mean(TRACING)), aes(x = age, y = MeanTracing, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Inner features presented", x = "Age (in months)", y = "Tracing score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

pdf("results/R_plot/tracing-scram-age.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_tracing %>% filter(PicType == 'type-S') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, age) %>% summarize(MeanTracing = mean(TRACING)), aes(x = age, y = MeanTracing, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Scrambled inner features presented", x = "Age (in months)", y = "Tracing score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

# Coloring

pdf("results/R_plot/coloring-outer-age.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_coloring %>% filter(PicType == 'type-O') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, age) %>% summarize(MeanColoring = mean(COLORING)), aes(x = age, y = MeanColoring, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Outline presented", x = "Age (in months)", y = "Coloring score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

pdf("results/R_plot/coloring-inner-age.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_coloring %>% filter(PicType == 'type-I') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, age) %>% summarize(MeanColoring = mean(COLORING)), aes(x = age, y = MeanColoring, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Inner features presented", x = "Age (in months)", y = "Coloring score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

pdf("results/R_plot/coloring-scram-age.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_coloring %>% filter(PicType == 'type-S') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, age) %>% summarize(MeanColoring = mean(COLORING)), aes(x = age, y = MeanColoring, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Scrambled inner features presented", x = "Age (in months)", y = "Coloring score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

# Relatedness score

pdf("results/R_plot/related-outer-age.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_meaning %>% filter(PicType == 'type-O') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, age) %>% summarize(MeanMeaning = mean(MEANING)), aes(x = age, y = MeanMeaning, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Outline presented", x = "Age (in months)", y = "Meaning score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

pdf("results/R_plot/related-inner-age.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_meaning %>% filter(PicType == 'type-I') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, age) %>% summarize(MeanMeaning = mean(MEANING)), aes(x = age, y = MeanMeaning, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Inner features presented", x = "Age (in months)", y = "Meaning score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

pdf("results/R_plot/related-scram-age.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_meaning %>% filter(PicType == 'type-S') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, age) %>% summarize(MeanMeaning = mean(MEANING)), aes(x = age, y = MeanMeaning, color=PicClass, fill=PicClass)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Scrambled inner features presented", x = "Age (in months)", y = "Meaning score (in %)", color = "") + scale_color_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + scale_fill_manual(labels = c("Car", "Face", "House", "Human"), values = c("#0072b2", "#d55e00", "#009e73", "#e69f00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

# relatedness score, colored depending on drawing style
all_together <- all
all_together = cbind(all_together, SCR=apply(all_together[,18:19],1,mean,na.rm=T))
all_together = cbind(all_together, COMP=apply(all_together[,15:16],1,mean,na.rm=T))
all_together$MEANING = all_together$Answer.QRelated
all_together$TRACING = all_together$Answer.QTracing
all_together$COLORING = all_together$Answer.QColoring

all_together$styleScribbling = all_together$SCR > all_together$COMP

pdf("results/R_plot/related-with-style-outer-age.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_together %>% filter(PicType == 'type-O') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, age, styleScribbling) %>% summarize(MeanMeaning = mean(MEANING)), aes(x = age, y = MeanMeaning, color=styleScribbling, fill=styleScribbling)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Outline presented", x = "Age (in months)", y = "Meaning score (in %)", color = "") + scale_color_manual(labels = c("completion", "scribbling"), values = c("#0072b2", "#d55e00")) + scale_fill_manual(labels = c("completion", "scribbling"), values = c("#0072b2", "#d55e00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()
# styleScribbling, alphabetical order: FALSE (=completion) TRUE (=scribbling)

pdf("results/R_plot/related-with-style-inner-age.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_together %>% filter(PicType == 'type-I') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, age, styleScribbling) %>% summarize(MeanMeaning = mean(MEANING)), aes(x = age, y = MeanMeaning, color=styleScribbling, fill=styleScribbling)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Inner features presented", x = "Age (in months)", y = "Meaning score (in %)", color = "") + scale_color_manual(labels = c("completion", "scribbling"), values = c("#0072b2", "#d55e00")) + scale_fill_manual(labels = c("completion", "scribbling"), values = c("#0072b2", "#d55e00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

pdf("results/R_plot/related-with-style-scram-age.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_together %>% filter(PicType == 'type-S') %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, age, styleScribbling) %>% summarize(MeanMeaning = mean(MEANING)), aes(x = age, y = MeanMeaning, color=styleScribbling, fill=styleScribbling)) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Scrambled inner features presented", x = "Age (in months)", y = "Meaning score (in %)", color = "") + scale_color_manual(labels = c("completion", "scribbling"), values = c("#0072b2", "#d55e00")) + scale_fill_manual(labels = c("completion", "scribbling"), values = c("#0072b2", "#d55e00")) + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()



### get highest ranked scribbling/completion etc. pictures ###

#num_highest = 20
higher_than = 80

all_scribble_sorted = all_scribble %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, age) %>% summarize(MeanSCR = mean(SCR)) %>% arrange(desc(MeanSCR))
subset = all_scribble_sorted %>% filter(MeanSCR > higher_than)#[1:num_highest,]
write.csv(subset, file=paste0("results/python_csv/highest_ranked_scribbling-", higher_than, ".csv"))

all_compl_sorted = all_compl %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, age) %>% summarize(MeanCOMP = mean(COMP)) %>% arrange(desc(MeanCOMP))
subset = all_compl_sorted %>% filter(MeanCOMP > higher_than)#[1:num_highest,]
write.csv(subset, file=paste0("results/python_csv/highest_ranked_completion-", higher_than, ".csv"))

all_tracing_sorted = all_tracing %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, age) %>% summarize(MeanTRACING = mean(TRACING)) %>% arrange(desc(MeanTRACING))
subset = all_tracing_sorted %>% filter(MeanTRACING > higher_than)#[1:num_highest,]
write.csv(subset, file=paste0("results/python_csv/highest_ranked_tracing-", higher_than, ".csv"))

all_coloring_sorted = all_coloring %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, age) %>% summarize(MeanCOLORING = mean(COLORING)) %>% arrange(desc(MeanCOLORING))
subset = all_coloring_sorted %>% filter(MeanCOLORING > higher_than)#[1:num_highest,]
write.csv(subset, file=paste0("results/python_csv/highest_ranked_coloring-", higher_than, ".csv"))







pdf("results/R_plot/all-drawing-styles-across-age.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_meaning %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, age) %>% summarize(MeanMeaning = mean(MEANING)), aes(x = age, y = MeanMeaning, color="#0072b2", fill="#0072b2")) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Outline presented", x = "Age (in months)", y = "Meaning score (in %)", color = "") + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()


pdf("results/R_plot/all-drawing-styles-across-age-scribbling.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_scribble %>% group_by(Child, PictureIdx, PicType, PicClass, image_url, age) %>% summarize(MeanSCR = mean(SCR)), aes(x = age, y = MeanSCR, color="#0072b2", fill="#0072b2")) + geom_point(size=3) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Inner features presented", x = "AQ score", y = "Scribbling score (in %)", color = "") + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()

all_scribble_mean <- all_scribble %>% group_by(Child, age) %>% summarize(MeanSCR = mean(SCR))
all_tracing_mean <- all_tracing %>% group_by(Child) %>% summarize(MeanTRACING = mean(TRACING))
all_coloring_mean <- all_coloring %>% group_by(Child) %>% summarize(MeanCOLORING = mean(COLORING))
all_compl_mean <- all_compl %>% group_by(Child) %>% summarize(MeanCOMP = mean(COMP))

all_scores = cbind(all_scribble_mean, MeanCOMP=all_compl_mean$MeanCOMP)
all_scores = cbind(all_scores, MeanTRACING=all_tracing_mean$MeanTRACING)
all_scores = cbind(all_scores, MeanCOLORING=all_coloring_mean$MeanCOLORING)


color1 <- "#0072b2"
color2 <- "#d55e00"
color3 <- "#009e73"
color4 <- "#e69f00"

pdf("results/R_plot/all-drawing-styles-across-age_compl.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_scores, aes(x = age, y = MeanCOMP)) + geom_point(size=3, color=color1) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Rating of degree of completion", x = "Age (in months)", y = "Human rating score (in %)", color = "") + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()
pdf("results/R_plot/all-drawing-styles-across-age_scr.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_scores, aes(x = age, y = MeanSCR)) + geom_point(size=3, color=color1) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Rating of degree of scribbling", x = "Age (in months)", y = "Human rating score (in %)", color = "") + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()
pdf("results/R_plot/all-drawing-styles-across-age_coloring.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_scores, aes(x = age, y = MeanCOLORING)) + geom_point(size=3, color=color1) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Rating of degree of coloring in", x = "Age (in months)", y = "Human rating score (in %)", color = "") + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()
pdf("results/R_plot/all-drawing-styles-across-age_tracing.pdf", width = 6, height = 4.5, onefile = FALSE)
ggplot(all_scores, aes(x = age, y = MeanTRACING)) + geom_point(size=3, color=color1) + geom_smooth(size=2, alpha=0.2, method="lm", se=T) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Rating of degree of tracing", x = "Age (in months)", y = "Human rating score (in %)", color = "") + theme_bw() + theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), plot.title = element_text(size = 16, face = "bold", color = "black"))
dev.off()




