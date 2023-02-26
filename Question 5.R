### Question 5 - Bee Mortality Rate ####

#Part a: Comparing the mortality rate of the bees between the spring and summer trials.
#Part b: Comparing the mortality rate of the bees between the presence and absence of UV light treatments.

#install.packages("ggplot2")  
library("ggplot2")

Bee_Death_Rate <- read.csv("BeeDeathRate.csv")
head(Bee_Death_Rate)
Bee_Death_Rate <- na.omit(Bee_Death_Rate)





#######Part a: Comparing the mortality rate of the bees between the spring and summer trials.

#Create two models, one taking into account Season conditions and one not. See which model is a better fit using AIC scores
#model without Season grouping variable
model1 <- lm(day ~ poly(percent_dead, 2), data = Bee_Death_Rate)

#model with Season grouping variable
model2 <- lm(day ~ poly(percent_dead, 2) * Season, data = Bee_Death_Rate)

#compare models 
anova(model1, model2) ##p =8.668e-07
AIC(model1,model2) #AIC is lower with the season grouping, better fit
#model1  4 454.7697
#model2  7 427.5154


############# Figure 5 ######################## 
## Load Edited table for clear graph:

Bee_Death_Rate_Graph_Season <- read.csv("BeeDeathRate_SeasonGraph.csv")
Bee_Death_Rate_Graph_Season <- na.omit(Bee_Death_Rate_Graph_Season)


ggplot(data=Bee_Death_Rate_Graph_Season, aes(x=day, y=percent_dead))+ 
  geom_smooth(mapping= aes(x=day, y=percent_dead, colour=Model), method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 2))+
  labs(x="Trial Day",   y="Cumulative Percent of Bees Deceased")+
  scale_color_manual(values=c("#000000", "#0057A1", "#93CDFF"))










####################################
#######Part b: Comparing the mortality rate of the bees between the presence and absence of UV light treatments.

##Create two models, one taking into account UV treatments and one not. See which model is a better fit using AIC scores
#model without UV Treatment grouping variable
model3 <- lm(day ~ poly(percent_dead, 2), data = Bee_Death_Rate)
summary(model3)

#model grouping by UV Treatment
model4 <- lm(day ~ poly(percent_dead, 2) * UV, data = Bee_Death_Rate)
summary(model4)

#compare models 
anova(model3, model4) #p =9.881e-08 
AIC(model3, model4) #AIC is lower with the UV grouping, better fit
#model without UV grouping  454.7697
#model with UV grouping  422.7122



############# Figure 6 ######################## 
## Load Edited table for clear graph:
Bee_Death_Rate_Graph <- read.csv("BeeDeathRate_UVGraph.csv")
Bee_Death_Rate_Graph <- na.omit(Bee_Death_Rate_Graph)

ggplot(data=Bee_Death_Rate_Graph, aes(x=day, y=percent_dead))+ 
  geom_smooth(mapping= aes(x=day, y=percent_dead, colour=Model), method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 2))+
  labs(x="Trial Day",   y="Cumulative Percent of Bees Deceased")+
  scale_color_manual(values=c("#000000", "#0057A1", "#93CDFF"))







