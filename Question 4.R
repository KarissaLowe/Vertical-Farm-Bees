####Remove from what you upload to git hub:

setwd("/Users/karis/Documents/2023_Learning/Portfolio/Cleaning Up Thesis/Question 4")


### Question 4 - Does the root:shoot ratio of the plants differ between treatments?  ####

#In order to holistically consider the strawberry plants’ health, we compared the root:shoot ratio of the plants between both season trials and room treatments

#install.packages("ggplot2")  
library("ggplot2")
dryweight_data <- read.csv("dryweight.csv")
head(dryweight_data)
dryweight_data = dryweight_data[which(dryweight_data$RootShoot > 0), ]  #removed five samples because of less than zero value



####################### Visualization   ####################

#Figure 4: Root:Shoot Ratios of Strawberry Plants by Treatment and Season Trial

dryweight_data$Treatment_f = factor(dryweight_data$Treatment, levels=c('Control','Bee','Bee UV'))


ggplot(dryweight_data, aes(x=Treatment_f, y=RootShoot, fill=Trial)) + 
  geom_boxplot()+
  geom_point(position=position_jitterdodge(),alpha=0.3)+
  labs(x="Treatment",   y="Root/Shoot Ratio", fill = "Season")+
  scale_fill_manual(values=c("#0057A1", "#93CDFF"))



#################### Statistical Analysis ######################


##Is this data Normally distributed? Can we assume equal variance?
my_anova <- aov(RootShoot ~ Treatment * Trial, data = dryweight_data)
summary(my_anova)
aov_residuals <- residuals(object = my_anova)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals ) ## 2.2e-16 not normal
leveneTest(dryweight_data$RootShoot ~ dryweight_data$Treatment * dryweight_data$Trial) #p = 1.318e-07 cannot assume equal variances
##Not normal so check results with ANOVA




##Use t tests to compare each treatment with one another

#First compare Spring season trial to Summer season trial
t.test(dryweight_data$RootShoot~dryweight_data$Trial) #t = -5.8568, df = 89.801, p-value = 7.64e-08


##Now compare the Treatments
#Compare Control to Bee
rootshoot_noUV = dryweight_data[which(dryweight_data$Treatment!='Bee UV'), ]
t.test(rootshoot_noUV$RootShoot~rootshoot_noUV$Treatment) #t = -0.86864, df = 92.525, p-value = 0.3873

#compare Control to Bee UV
rootshoot_noBee = dryweight_data[which(dryweight_data$Treatment!='Bee'), ]
t.test(rootshoot_noBee$RootShoot~rootshoot_noBee$Treatment)#t = 0.70356, df = 136.51, p-value = 0.4829

#compare Bee to Bee UV
rootshoot_noControl = dryweight_data[which(dryweight_data$Treatment!='Control'), ]
t.test(rootshoot_noControl$RootShoot~rootshoot_noControl$Treatment)#t = -1.7132, df = 90.885, p-value = 0.09009

#Only significant p value different between season trials not UV treatments



##Additional confirmation with ANOVA, due to not normal distribution 

dryweight_data.aov<-aov(dryweight_data$RootShoot~dryweight_data$Treatment)
summary(dryweight_data.aov)

#post hoc
pairwise.t.test(dryweight_data$RootShoot,dryweight_data$Treatment,p.adjust.method="BH")
pairwise.t.test(dryweight_data$RootShoot,dryweight_data$Trial,p.adjust.method="BH")









