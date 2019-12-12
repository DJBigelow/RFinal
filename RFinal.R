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
#Paired Sample T-test
#T confidence interval
#!Conditional Probability!
#Contingency Table
#Chi-Square for Independence
#!2-Prop Z-test!
#Multilinear Regression
#Logistic Regression!



#This data set was scraped from Edmunds and Twitter by the user Sam Keene on kaggle.com.
#This data set is found on https://www.kaggle.com/CooperUnion/cardataset and was collected
#December of 2017.
getwd()
setwd("C:/Users/koolm/documents/pricemaster/rfinal")
vehicles <- read.csv('data.csv')

#First thing's first, the data needs to be cleaned up a bit

#The 2017 Audi A6 has a highway mpg of 354, which is obviously a data entry error. 
#For this reason, we are removing it from the data set.
qqnorm(jitter(vehicles$highway.MPG, factor = 2))
vehicles$highway.MPG[c(1120)]
vehicles <- vehicles[-c(1120),]

#There are a few vehicles with an unknown number of cylinders
vehicles <- vehicles[!(is.na(vehicles$Engine.Cylinders)), ]

#Three vehicles don't have a fuel type
vehicles <-  vehicles[!(vehicles$Engine.Fuel.Type == ''), ]

#There are a few vehicles with unknown transmission types which we will be removing
vehicles <- vehicles[!(vehicles$Transmission.Type == 'UNKNOWN'), ]
vehicles$Transmission.Type <- factor(vehicles$Transmission.Type)

#While the data on electric vehicles is valid, we consider electric vehicles to be 
#sufficiently different from from conventional vehicles to be excluded from our analysis
#Electric vehicles have a significantly higher than average fuel efficiency
mean(vehicles$highway.MPG[vehicles$Engine.Fuel.Type == 'electric'])
mean(vehicles$highway.MPG[!(vehicles$Engine.Fuel.Type == 'electric')])

#Q-Q Plot of highway mpg before removing electric vehicles
qqnorm(jitter(vehicles$highway.MPG, factor = 2))
qqline(vehicles$highway.MPG)
vehicles <- vehicles[!(vehicles$Engine.Fuel.Type == 'electric'), ]

#Q-Q Plot if highway mpg after removing electric vehicles
qqnorm(jitter(vehicles$highway.MPG, factor = 2))
qqline(vehicles$highway.MPG)


#Summaries of numerical data (narrow down to 5)
summary(vehicles$Year)
boxplot(vehicles$Year, col = "skyblue2", ylab= "Year")
summary(vehicles$Engine.HP)
boxplot(vehicles$Engine.HP, col = "orange", ylab = "Horsepower")
summary(vehicles$Engine.Cylinders)

summary(vehicles$city.mpg)
summary(vehicles$highway.MPG)
corh= c("City", "Highway")
boxplot(vehicles$city.mpg, vehicles$highway.MPG, names = corh, col="skyblue2", main = "Fuel Economy")


summary(vehicles$MSRP)
boxplot(vehicles$MSRP, col= "purple", ylab = "MSRP")

hist(vehicles$highway.MPG, breaks= 8, col = "skyblue2", xlab="Highway MPG")


#Mosaic plot of vehicle transmission type and driven wheels
mosaicplot(~ vehicles$Transmission.Type + vehicles$Driven_Wheels, col= 2:13, 
           xlab= "Transmission Type", ylab= "Driven Wheels", main = "Mosaic Plot")



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

################################################
barplot(table(vehicles$Vehicle.Size), main= "Vehicle Size", xlab= "Size", ylab= "Count", col="deepskyblue")

#Grouped Barplot



#Paired sample t-test
#This is a random sample of 50 of compact and midsize cars. 
compacthp <- compact$Engine.HP[sample.int(compact$Engine.HP, 50)]
midenginehp <- midsize$Engine.HP[sample.int(midsize$Engine.HP, 50)]
t.test(compacthp, midenginehp, paired = TRUE)



#One-sample t-test
#Comparing to see if the true mean is equal to 250hp 
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

#Linear regression
summary(lm(vehicles$city.mpg ~ vehicles$highway.MPG))


#Scatterplot with regression line
ggplot(vehicles, aes(x = highway.MPG, y = city.mpg)) + 
  geom_point() + 
  geom_abline()
summary(vehicles$city.mpg)
summary(vehicles$highway.MPG)



#T- confidence interval
#this confidence interval tell us that the true highway mpg average is between 26.09001 and 26.31684
hist(vehicles$highway.MPG, col = "deepskyblue")
error <- qt(0.975, df = nrow(vehicles)-1)* sd(vehicles$highway.MPG)/sqrt(nrow(vehicles))
mean(vehicles$highway.MPG)- error
mean(vehicles$highway.MPG)+ error



#Multilinear regression
plot(x= vehicles$Driven_Wheels, y=vehicles$Engine.HP)
model <- lm(vehicles$highway.MPG ~ vehicles$Engine.HP + vehicles$Driven_Wheels)
summary(model)

#two prop Z-test ##############################################################still needs work!
#how many manual transmission cars get better than 30 mpg (highway)
#how many automatic transmission cars get better than 30 mpg (highway)
table(vehicles$Transmission.Type)
table(vehicles$highway.MPG)
#automated manual is included in manual because they work the same way for the most part. 
manual= 625+2922
automatic = 8256
table(vehicles$transmission.type, vehicles$highway.MPG)



#Logistic Regression
for (row in 1:nrow(vehicles)){
  if(vehicles$Engine.HP[row]>299){vehicles$overthree[row]= 1} else {vehicles$overthree[row]= 0}
}
logistic <- glm(overthree~ vehicles$highway.MPG, family= 'binomial', data = vehicles)
plot(vehicles$highway.MPG, vehicles$overthree, main = 'Using fuel economy to predict if a vehicle has more than 300 hp', xlab= 'Fuel Ecomony', ylab= 'If a car has more than 300HP' )
abline(lm(overthree ~ vehicles$highway.MPG, data = vehicles))
predY<- predict.glm(logistic, type="response")
points(vehicles$highway.MPG, predY, col = 2, lty = 2)
barplot(table(vehicles$overthree))


