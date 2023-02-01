#install.packages("ggplot2")  
library("ggplot2")

trial_both_berry_num_data <- read.csv("berrydata.csv") 
#This table includes data about each strawberry plant at the end of each trial. 
#This information includes: number of berries, status of plant at end of trial, weight of root and weight of shoot
#And which variables were applied to each plant
head(trial_both_berry_num_data)

##### Summer Trial had less plants, because some died during the waiting period
###removing these blank rows:
trial_both_berry_num_data_2 = trial_both_berry_num_data[which(trial_both_berry_num_data$plant_status!='blank'), ]


##############################################################
#Question 1 - Were the number of berries produced by plants different between treatments and season trials?

#To consider if the number of berries produced per plant differed between treatments and season conditions, 
#we used a Zero-inflated regression using the “zeroinfl” function from the “pscl” package

###############################################
#####FIGURE 1: Number of Berries Produced per Plant #####
trial_both_berry_num_data_2$treatment_f = factor(trial_both_berry_num_data_2$treatment, levels=c('Control','Bee','Bee/UV'))


library(plyr)
mu <- ddply(trial_both_berry_num_data_2, "trial", summarise, grp.mean=mean(berrynum))
head(mu)


ggplot(trial_both_berry_num_data_2, aes(x=berrynum))+
  geom_histogram(color="black", fill="grey", binwidth=1)+
  facet_grid(trial ~ treatment_f)+
  labs(x="Number of Berrries Produced by Plant", y = "Count of Plants")+ 
  scale_color_brewer(palette="Accent") + 
  theme_minimal()+theme(legend.position="top")



#############################################
##### Zero Inflated Regression #####
#install.packages("pscl")
#library(pscl)



data_m = trial_both_berry_num_data_2
head(data_m)

data_m$treatment = relevel(factor(data_m$treatment),
                           ref= 2)

model.zero = zeroinfl(data_m$berrynum ~ data_m$trial + data_m$treatment, 
                      data = data_m, 
                      dist = "poisson")
summary(model.zero)



########################################
###Additional comparison using t test to compare each treatment with one another and season trials with one another
data_m = trial_both_berry_num_data_2
head(data_m)

#compare Summer and Spring trials
t.test(data_m$berrynum~data_m$trial)
#t = 12.759, df = 120.64, p-value < 2.2e-16


####compare Control to Bee
data_m_noUV = data_m[which(data_m$treatment!='Bee/UV'), ] # Remove Bee/UV:
t.test(data_m_noUV$berrynum~data_m_noUV$treatment)
###t = 4.6172, df = 105.91, p-value = 1.098e-05

#### compare control to BeeUV
data_m_nobee = data_m[which(data_m$treatment!='Bee'), ] # Remove Bee:
head(data_m_nobee)
t.test(data_m_nobee$berrynum~data_m_nobee$treatment)
###t = 4.7057, df = 108.41, p-value = 7.511e-06

####Compare BeeUV to Bee
data_m_nocontrol = data_m[which(data_m$treatment!='Control'), ] #Remove control
head(data_m_nocontrol)
t.test(data_m_nocontrol$berrynum~data_m_nocontrol$treatment)
###t = 0.087131, df = 137.99, p-value = 0.9307

#### These above results are very similar to what the zero inflated poison regression shows



















#