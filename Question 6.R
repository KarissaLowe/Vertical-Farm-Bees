### Question 6 - Supplemental Food Choices ####
#We provided four alternative food choices to investigate the best food supplement for O. lignaria. Which food suppliement had the most visits and was this affected by season condition?

#install.packages("ggplot2")  
library("ggplot2")
#install.packages("corrplot")  
library(corrplot)




################## What Food Supplement did the Bees prefer overall? #########

#Goodness of Fit - Compare Total number of visits to different Food Supplements

observed = c(99, 201, 273, 1391)        # observed frequencies
expected = c(0.25, 0.25, 0.25, 0.25)      # expected proportions
chisq.test(x = observed,
           p = expected) ## X-squared = 2230.7, df = 3, p-value < 2.2e-16

#Post Hoc test to compare Sugar water Visits to All other supplements visits combined 
prop.test(x = c(1391, 573), n = c(1964, 1964),
          alternative = "greater") #X-squared = 679.72, df = 1, p-value < 2.2e-16



#### FIGURE 7: Supplement Visit Proportions ###

total_count_prop <- read.csv("total_count_prop.csv")
head(total_count_prop)


ggplot(total_count_prop, aes(x=Supplement, y=Proportion, fill=Supplement)) +
  geom_bar(stat = "identity", width = 0.8) +
  #geom_text(aes(label=Proportion))+
  scale_y_continuous(breaks = seq(0, 0.80, 0.10),
                     limits = c(0, 0.80),
                     expand = c(0, 0))+
  scale_fill_manual(values=c("#93CDFF", "#93CDFF", "#93CDFF", "#93CDFF")) +
  theme(legend.position="none")+
  labs(x = "Food Supplement Type", y = "Proportion of Visits")+
  geom_text(
    aes(label=sprintf("%0.2f", round(Proportion, digits = 2))), 
    vjust = -0.3, size = 3.5)





############### Did the Season Conditions or UV Light Treatment affect these food supplement preferences? ########


##### Season Conditions Comparison ########
season_count_prop <- read.csv("season_count_prop.csv", row.names = 1)


chisq <- chisq.test(season_count_prop)
chisq ## X-squared = 46.419, df = 3, p-value = 4.618e-10
round(chisq$residuals, 3)
corrplot(chisq$residuals, is.cor = FALSE)
#strong positive association with Honeybee pollen and summer, honey bee pollen was next to sugar in uv but not in no uv room



##### UV Treatment Comparison #####
uv_count_prop <- read.csv("uv_count_prop.csv", row.names = 1)
head(uv_count_prop)


chisq <- chisq.test(uv_count_prop)
chisq ##X-squared = 139.62, df = 3, p-value < 2.2e-16

corrplot(chisq$residuals, is.cor = FALSE)

#positive correlation of honey bee pollen and +UV treatment
#and positive correlation of Artificial pollen and No UV treatment
#but it just so happens that even though food supplement placement was chosen randomly both of those were near the sugar water - add to discussion  






















