### Question 3 - Compare Dry Matter Content between the different Treatments ####


#Does the Dry Matter Content (DMC) of the strawberries differ between different treatments?
#To further explore the ability of O. lignaria to pollinate strawberries in a vertical farm, we compared the dry matter content of strawberries grown in the different treatments
#dry matter content of each plant’s berries was calculated using the following formula: DMC = (Pooled Dry Weight of Berries per plant / Pooled Fresh Weight of Berries per plant) * 100

#install.packages("ggplot2")  
library("ggplot2")
DryMatterContent <- read.csv("percentdrymatter.csv") #read in the Dry Matter Content Data of the strawberries from the spring trial
head(DryMatterContent)



################ Review Averages and N values ###########################################

##Review Average DMC in each Treatment
control_percentdry = DryMatterContent[which(DryMatterContent$Treatment=='Control'),]
mean(control_percentdry$percent_dry_matter) #15.66967 in Control

bee_percentdry = DryMatterContent[which(DryMatterContent$Treatment=='Bee'),]
mean(bee_percentdry$percent_dry_matter) #15.58375 in Bee

beeuv_percentdry = DryMatterContent[which(DryMatterContent$Treatment=='Bee/UV'),]
mean(beeuv_percentdry$percent_dry_matter) #12.40685 in Bee/UV

##Review number of plants that produced strawberries in each treatment, n values
length(control_percentdry$percent_dry_matter) #19
length(bee_percentdry$percent_dry_matter) #39
length(beeuv_percentdry$percent_dry_matter) #40



#######################    Visualization   ####################
# Create Figure 3: Strawberry Dry Matter Content in the Spring Trial
DryMatterContent$Treatment <- factor(DryMatterContent$Treatment,
                                            levels = c('Control','Bee','Bee/UV'),ordered = TRUE)

ggplot(data = DryMatterContent, aes(x=Treatment, y=percent_dry_matter)) +
  geom_boxplot(aes(fill = factor(Treatment)), outlier.shape = NA) + geom_jitter(width = 0.2, alpha = .12)+
  labs(x="Treatment",   y="Percent Dry Matter", fill = "Room Treatments")+
  scale_fill_manual(values=c("#93CDFF", "#549FE0", "#1066AF"))





#################### Statistical Analysis ######################

###use individual t tests to compare each treatment with one another

#compare Control to Bee
dry_noUV = DryMatterContent[which(DryMatterContent$Treatment!='Bee/UV'), ]
t.test(dry_noUV$percent_dry_matter~dry_noUV$Treatment) #t = -0.04395, df = 32.313, p-value = 0.9652
#compare Control to BeeUV
dry_nobee = DryMatterContent[which(DryMatterContent$Treatment!='Bee'), ]
t.test(dry_nobee$percent_dry_matter~dry_nobee$Treatment) #t = -1.8935, df = 20.831, p-value = 0.07226
#compare Bee to BeeUV
dry_nocontrol = DryMatterContent[which(DryMatterContent$Treatment!='Control'), ]
t.test(dry_nocontrol$percent_dry_matter~dry_nocontrol$Treatment)#t = 2.8103, df = 52.661, p-value = 0.006934




###Perform Mann-Whitney U test
#checking results with Mann Whitney Test because sample size is smaller in certain treatments and data not normally distributed

#compare Control to Bee
wilcox.test(percent_dry_matter ~ Treatment, data=dry_noUV) #W = 443, p-value = 0.2329
#compare Control to BeeUV
wilcox.test(percent_dry_matter ~ Treatment, data=dry_nobee) #W = 304, p-value = 0.2206
#compare Bee to BeeUV
wilcox.test(percent_dry_matter ~ Treatment, data=dry_nocontrol) #W = 1140.5, p-value = 0.0004154






#####Additional confirmation
### Kruskal-Wallis test was used to check for extreme differences in p-values. 
###Similarly, we confirmed p-value differences with a one-way ANOVA
#ANOVA 
drymatter.aov<-aov(DryMatterContent$percent_dry_matter~DryMatterContent$Treatment)
summary(drymatter.aov)

kruskal.test(DryMatterContent$percent_dry_matter~DryMatterContent$Treatment)
##0.002686













