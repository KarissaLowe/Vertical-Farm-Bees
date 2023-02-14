### Question 2 - Volume ####

#Does the volume of strawberries differ between different treatments?
#To further explore the ability of O. lignaria to pollinate strawberries in a vertical farm, we compared the volume of strawberries grown in the different treatments


#install.packages("ggplot2")  
library("ggplot2")
volume_data <- read.csv("trial_1_berry_volume.csv")  #read in the volume data of the strawberries from the spring trial
head(volume_data)



#####################################
##Calculating the averages of Berry Volume in each of the different treatments:
avg_volume_bee = volume_data[which(volume_data$treatment=='Bee'),]
mean(avg_volume_bee$volume) #627.4895
length(avg_volume_bee$volume) #192

avg_volume_control = volume_data[which(volume_data$treatment=='Control'),]
mean(avg_volume_control$volume) #312.145
length(avg_volume_control$volume) #49

avg_volume_beeuv = volume_data[which(volume_data$treatment=='Bee/UV'),]
mean(avg_volume_beeuv$volume)#685.3308
length(avg_volume_beeuv$volume) #205




#######################    Visualization   ############
## Create Figure 2 to show spread of berry volume compared between the three treatment rooms.
volume_data_RemoveAbove3000 = volume_data[which(volume_data$volume<3000), ] ##Remove data points above 3000 for the visual

volume_data_RemoveAbove3000$treatment_f = factor(volume_data_RemoveAbove3000$treatment, levels=c('Control','Bee','Bee/UV'))

###FIGURE 2: Strawberry Volumes (mm3 ) by Treatment in the Spring Trial
ggplot(data =volume_data_RemoveAbove3000, aes(x=treatment_f, y=volume)) +
  geom_boxplot(aes(fill = factor(treatment_f)), outlier.shape = NA) + geom_jitter(width = 0.2, alpha = .12)+
  labs(x="Treatment of Room",   y="Volume of Strawberries (mm3)", fill = "Room Treatments")+
  scale_fill_manual(values=c("#93CDFF", "#549FE0", "#1066AF"))




#################### Statistical Analysis ######################


#check for equal variances:
install.packages("car")
library(car)
leveneTest(volume_data$volume,volume_data$treatment)
#A resulting p-value under 0.05 means that variances are not equal
#p value is .04699, variances are unequal

###Pairwise T Test
pairwise.t.test(volume_data$volume,volume_data$treatment,p.adjust.method="BH")


###Also use individual t tests to compare each treatment with one another
#Compare Control to Bee
volume_noUV = volume_data[which(volume_data$treatment!='Bee/UV'), ]
t.test(volume_noUV$volume~volume_noUV$treatment) ##t = 4.7293, df = 181.88, p-value = 4.504e-06
#Compare Control to BeeUV
volume_noBEE = volume_data[which(volume_data$treatment!='Bee'), ]
t.test(volume_noBEE$volume~volume_noBEE$treatment) #t = 5.3668, df = 199.84, p-value = 2.21e-07
#Compare Bee to BEeUV
volume_noControl = volume_data[which(volume_data$treatment!='Control'), ]
t.test(volume_noControl$volume~volume_noControl$treatment) ##t = -0.77999, df = 394.43, p-value = 0.4359

#####Additional confirmation
### Kruskal-Wallis test was used to check for extreme differences in p-values. 
###Similarly, we confirmed p-value differences with a one-way ANOVA
#ANOVA 
volume.aov<-aov(volume_data$volume~volume_data$treatment)
summary(volume.aov)

#Kruskal walis Test
kruskal.test(volume_data$volume~volume_data$treatment)
#p-value = 0.003647


