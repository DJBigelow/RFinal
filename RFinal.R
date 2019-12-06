library('ggplot2')
library(plyr)




#Checklist (! means something is unfinished or missing)
#1.1
#1.2
#!1.3!
#!1.4!
#Anova
#2 Sample T-test
#1Sample T-test
#!Paired Sample T-test!
#T confidence interval
#!Conditional Probability!
#Contingency Table
#Chi-Square for Independence
#!2-Prop Z-test!
#Multilinear Regression
#!Logistic Regression!



#This data set was scraped from Edmunds and Twitter by the user Sam Keene on kaggle.com.
#This data set is found on https://www.kaggle.com/CooperUnion/cardataset and was collected
#December of 2017.

vehicles <- read.csv('data.csv')

#The 2017 Audi A6 has a highway mpg of 354, which is obviously a data entry error. 
#For this reason, we are removing it from the data set.
vehicles$highway.MPG[c(1120)]
vehicles <- vehicles[-c(1120),]


#We may want to remove electric vehicles from our data set since they skew the data on mpg to the right pretty hard
qqnorm(vehicles$highway.MPG)
qqline(vehicles$highway.MPG)
vehicles <- vehicles[!(vehicles$Engine.Fuel.Type == 'electric'), ]


#There are a few vehicles with unknown transmission types which we will be removing
vehicles <- vehicles[!(vehicles$Transmission.Type == 'UNKNOWN'), ]
vehicles$Transmission.Type <- factor(vehicles$Transmission.Type)



#Mosaic plot of vehicle transmission type and driven wheels
mosaicplot(~ vehicles$Transmission.Type + vehicles$Driven_Wheels)


#Barplot and Pie Chart
compact <- vehicles[vehicles$Vehicle.Size == 'Compact',]
midsize <- vehicles[vehicles$Vehicle.Size == 'Midsize',]
large <- vehicles[vehicles$Vehicle.Size == 'Large',]

vhc_size <- nrow(vehicles)

df <- data.frame(group = c('Compact', 'Midsize', 'Large'),
                 value = c(100*nrow(compact) / vhc_size, 100*nrow(midsize) / vhc_size, 100*nrow(large) / vhc_size))

barplot <- ggplot(df, aes(x = '', y = value, fill = group)) + 
  geom_bar(width = 1, stat = 'identity')
barplot 

barplot + coord_polar('y', start = 0)


#Grouped Barplot


#Paired sample t-test
t.test(compact$Engine.HP, midsize$Engine.HP)



#One-sample t-test
t.test(vehicles$Engine.HP, mu = 250)



#ANOVA hypothesis test to determine if there is a difference in highway MPG between vehicle makes
qqnorm(vehicles$highway.MPG)
qqline(vehicles$highway.MPG)

ggplot(data = vehicles, aes(x = highway.MPG)) +
  geom_histogram()

mpg_make_anova <-aov(highway.MPG ~ Make, data = vehicles)



#Our p value is less than 2e-16, strongly suggesting that at least one vehicle make differs from the rest in terms of highway MPG
summary(mpg_make_anova)



#Two-sample t-test
qqnorm(vehicles$city.mpg)
qqline(vehicles$city.mpg)
ggplot(data = vehicles, aes(x = city.mpg)) +
  geom_histogram()

t.test(x = vehicles$city.mpg, y = vehicles$highway.MPG, alternative = 'two.sided')



#Contingency Table
tbl <- table(vehicles$Transmission.Type, vehicles$Driven_Wheels)
tbl

#Chi Squared Test
chisq.test(tbl)

summary(lm(vehicles$city.mpg ~ vehicles$highway.MPG))


#Scatterplot with regression line
ggplot(vehicles, aes(x = highway.MPG, y = city.mpg)) + 
  geom_point() + 
  geom_abline()

summary(vehicles$city.mpg)
summary(vehicles$highway.MPG)

#T- confidence interval
#this confidence interval tell us that the true highway mpg average is between 26.09001 and 26.31684
hist(vehicles$highway.MPG)
error <- qt(0.975, df = nrow(vehicles)-1)* sd(vehicles$highway.MPG)/sqrt(nrow(vehicles))
mean(vehicles$highway.MPG)- error
mean(vehicles$highway.MPG)+ error


#Multilinear regression
plot(x= vehicles$Driven_Wheels, y=vehicles$Engine.HP)
model <- lm(vehicles$highway.MPG ~ vehicles$Engine.HP + vehicles$Driven_Wheels)
summary(model)