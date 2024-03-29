library('ggplot2')
library(plyr)
library(tinytex)

#This data set was scraped from Edmunds and Twitter by the user Sam Keene on kaggle.com.
#This data set is found on https://www.kaggle.com/CooperUnion/cardataset and was 
#collected December of 2017.

#In our data set we had 8 variables that are catagorical, they are make, model, engine
#fuel type, transmission type, drive system type (driven wheels),market catagory, vehicle 
#size, and, vehicle style. Our data set also had some discrete and continuous numerical
#variables, with the year, engine cylinders, the number of doors, and popularity being 
#discrete. The continuous numerical variables include engine horsepower, city and highway 
#MPG, and MSRP. It also contains just under 12000 observations with the catagorical 
#variables having a minimum of three levels. 

vehicles <- read.csv('data.csv')
set.seed(0)

#First thing's first, the data needs to be cleaned up a bit
#The 2017 Audi A6 has a highway mpg of 354, which is obviously a data entry error. 
#For this reason, we are removing it from the data set.
vehicles$highway.MPG[c(1120)]
vehicles <- vehicles[-c(1120), ]


#We removed any entries with incomplete data
vehicles <- vehicles[!(is.na(vehicles$Engine.Cylinders)), ]
vehicles <- vehicles[!(vehicles$Engine.Fuel.Type == ''), ]
vehicles <- vehicles[!(is.na(vehicles$Engine.HP) ), ]



#There are a few vehicles with unknown transmission types which we also removed
vehicles <- vehicles[!(vehicles$Transmission.Type == 'UNKNOWN'), ]
vehicles$Transmission.Type <- factor(vehicles$Transmission.Type)


#While the data on electric vehicles is valid, we consider electric vehicles to be 
#sufficiently different from from conventional vehicles to be excluded from our analysis
#Electric vehicles have a significantly higher than average fuel efficiency.
mean(vehicles$highway.MPG[vehicles$Engine.Fuel.Type == 'electric'])
mean(vehicles$highway.MPG[!(vehicles$Engine.Fuel.Type == 'electric')])

#Q-Q Plot of highway mpg before removing electric vehicles
qqnorm(jitter(vehicles$highway.MPG, factor = 2), main = 'Q-Q Plot With Electric Vehicles')
qqline(vehicles$highway.MPG)
vehicles <- vehicles[!(vehicles$Engine.Fuel.Type == 'electric'), ]

#Q-Q Plot if highway mpg after removing electric vehicles
qqnorm(jitter(vehicles$highway.MPG, factor = 2), main = 'Q-Q Plot Without Electric Vehicles')
qqline(vehicles$highway.MPG)


#Summaries of numerical variables
summary(vehicles$Year)
boxplot(vehicles$Year, 
        col = "skyblue2", 
        ylab= "Year", 
        main = 'Vehicle Year')
#If we look at the summary statistics and the box plot we can see that our data has a 
#year range of 1990 to 2017 and has a mean of 2010. We can see that most of the vehicles
#in our dataset lie between 2007 and 2016. Since most of our vehicles are pretty new, 
#this could correlate to higher mean values for city and\or highway fuel economy.

summary(vehicles$Engine.HP)
boxplot(vehicles$Engine.HP, 
        col = "orange", 
        ylab = "Horsepower",
        main = 'Vehicle Horsepower')
#If we look at our Horsepower boxplot we can see that most of our data set has between 
#170 and 300 horsepower with a mean of 249.8 horsepower. If we take a look at the fuel 
#economy summaries and side by side boxplots in a later slide, we can hypothesize that
#the horsepower range in this summary and the fuel economy values correlate

summary(vehicles$Engine.Cylinders)
#Looking at this summary we see the mean number of cylinders is 5.656, this low mean
#in cylinders could be linked to our city and highway fuel economy.

summary(vehicles$city.mpg)
summary(vehicles$highway.MPG)
corh= c("City", "Highway")
boxplot(vehicles$city.mpg, 
        vehicles$highway.MPG, 
        names = corh, 
        col="skyblue2", 
        main = "Fuel Economy")
#looking at city and highway fuel economy in our data set we see that our city mean
#is 19.22 and highway mean is 26.22. These could be low due to relationships between
#higher numbers of engine cyliders correlating to lower fuel economy numbers as well
#as a vehicle having more than two driven wheels, requiring more power to move, and
#therefore using less fuel to move them. 

summary(vehicles$MSRP)
boxplot(vehicles$MSRP, 
        col= "purple",
        ylab = "MSRP",
        main = 'Vehicle MSRP')
#Here on our vehicle MSRP summary information it's interesing to note that there are
#a lot of outliers. This could possibly suggest that cost has either no correlation 
#to fuel economy or perhaps even a negative correlation. 

hist(vehicles$highway.MPG, 
     breaks= 8, 
     col = "skyblue2", 
     xlab="Highway MPG",
     main = 'Distribution of Vehicle Highway MPG')

#Here we have a histogram that shows how our highway fuel economies are distributed.
#The way the are distributed suggests that are cars are mostly compact and midsized
#cars. 

##################################################
#Visual exploration of data

#Mosaic plot of vehicle transmission type and driven wheels
mosaicplot(~ vehicles$Transmission.Type + vehicles$Driven_Wheels, 
           col= 2:13, 
           xlab= "Transmission Type", ylab= "Driven Wheels", 
           main = "Proportions of Driven Wheels and Transmission Type")

#This Mosaic plot of our data gives us a nice breakdown of our transmission and driven
#wheel configurations and how proportional they are to each other. As you can see, most
#of vehicles in our dataset have an automatic transmission and most of those are front
#wheel drive. It's also interesting to note that there are no vehicles that are dedicated 
#four wheel drive vehicles that use automated manual transmissions. 


#Barplot and Pie Chart
vehicles$Number.of.Doors = factor(vehicles$Number.of.Doors)
pie(table(vehicles$Number.of.Doors), 
    main = 'Proportions of Number of Vehicle Doors')
#This pie chart tells us how the numbers of vehicle doors are distributed in our data
#set, with four door vehicles being the most popular.

barplot(table(vehicles$Vehicle.Size), 
        main= "Distribution of Vehicles by Size", 
        xlab= "Size", 
        ylab= "Frequency",
        col="deepskyblue")
#This bar plot tells us that compact and midsized vehicles are the most popular, with
#large vehicles trailing behind.

#Clustered Barplot
vehicles$Engine.Cylinders = factor(vehicles$Engine.Cylinders)
barplot(table(vehicles$Engine.Cylinders, vehicles$Transmission.Type), 
        beside = TRUE,
        legend.text = levels(vehicles$Engine.Cylinders),
        main = 'Driven Wheels and Transmission Type')

#This bar plot tells us how the number of cylinders vehicles are distributed according 
#to the transmission type. The four and six cylinder engines with automatic transmissions
#are by far the most popular. The number of six cylider cars with automatic transmissions
#could contribute to the left skewness of the highway MPG histogram. 


################################################




#Paired sample t-test
#This is a random sample of 1000 compact and midsize cars. 
compacthp <- sample(vehicles[vehicles$Vehicle.Size == 'Compact', ]$Engine.HP, 1000)
midsizehp <- sample(vehicles[vehicles$Vehicle.Size == 'Midsize', ]$Engine.HP, 1000)
t.test(compacthp, midsizehp, paired = TRUE, alternative = 'less')


#One-sample t-test to determine if the true mean of 
#Comparing to see if the true mean is greater than 250
t.test(vehicles$Engine.HP, mu = 250, alternative = 'greater')
#Our P value of 0.5894 suggests that the true mean is not greater than 250 HP


#ANOVA hypothesis test to determine if there is a difference in highway MPG between
#vehicle makes
qqnorm(vehicles$highway.MPG)
qqline(vehicles$highway.MPG)

ggplot(data = vehicles, aes(x = highway.MPG)) +
  geom_histogram()

mpg_make_anova <-aov(highway.MPG ~ Make, data = vehicles)



#Our p value is less than 2e-16, strongly suggesting that at least one vehicle make
#differs from the rest in terms of highway MPG
summary(mpg_make_anova)




#Two-sample t-test to determine if the means of city mpg and highway mpg are equal
qqnorm(jitter(vehicles$city.mpg, 2), main = 'Q-Q Plot for Vehicle City MPG')
qqline(vehicles$city.mpg)
t.test(x = vehicles$city.mpg, y = vehicles$highway.MPG, alternative = 'two.sided')
#Ho: The difference in means between city and highway fuel economy is zero
#Ha: The difference in means betweencity and highway is not equal to zero
#Alpha= 0.05
#Data is in the table
#t=-59.702
#p= 2.2e-16
#reject Ho
#We have sufficient evidence to reject the Ho and state that the mean MPG between
#city and highway is not equal to zero. 





#Contingency Table of vehicle transmission type and driven wheels
tbl <- table(vehicles$Transmission.Type, vehicles$Driven_Wheels)
tbl
#Here we have a contingency table that we will be using for our Chi Squared test


#Chi Squared Test
chisq.test(tbl)
#The low p-value in our chi squared test for independence tells us that city and
#highway fuel economy are not independent.


#An additional Chi Squared test to find evidence of correlation between city and
#highway fuel economy.
tbl2<- table(vehicles$city.mpg, vehicles$highway.MPG)
tbl2
chisq.test(tbl2)
#The low p-value from this chi squared test suggests that there is a strong correlation
#between city and highway fuel economy in cars from our data set.

#Linear regression
summary(lm(vehicles$city.mpg ~ vehicles$highway.MPG))




#Scatterplot with regression line. We have a coefficient of regression of 0.8743,
#indicating that there is a strong relationship between highway and city MPG
ggplot(vehicles, 
       aes(x = highway.MPG, y = city.mpg)) + 
  geom_point() + 
  geom_abline() +
  xlab('Highway MPG') + 
  ylab('City MPG') +
  ggtitle('Highway MPG vs City MPG') 
  
summary(lm(vehicles$highway.MPG ~ vehicles$city.mpg + vehicles$Driven_Wheels))



#T-confidence interval
#This confidence interval tell us that there is a probability of 0.975 that 
#the true highway mpg average is between 26.10324 and 26.33089
hist(vehicles$highway.MPG,
     col = "deepskyblue",
     main = 'Distribution of Vehicle Highway MPGs',
     xlab = 'Highway MPG')
error <- qt(0.975, df = nrow(vehicles)-1)* sd(vehicles$highway.MPG)/sqrt(nrow(vehicles))
mean(vehicles$highway.MPG)- error
mean(vehicles$highway.MPG)+ error



#Conditional probability that a compact vehicle has a horsepower of 300 or greater
#The results tell us that there is a probability of ~0.1441 that any given compact
#vehicle has a horsepower of 300 or greater
mean(vehicles$Engine.HP[vehicles$Vehicle.Size == 'Compact'] >= 300)




#Multilinear regression to see if if a vehicle's number of engine cylinders can be
#determined by its horsepower and highway MPG. The coefficient of corellation is 0.7405,
#suggesting a relationship between an engine's number of cylinders and its vehicles 
#horsepower and highway MPG, albeit not an extremely strong one.
layout(matrix(c(1, 2, 3, 4), 2, 2 ))
vehicles$Engine.Cylinders =  as.numeric(as.character(vehicles$Engine.Cylinders))
model <- lm(vehicles$Engine.Cylinders ~ vehicles$Engine.HP  + vehicles$highway.MPG)
plot(model)
summary(model)
layout(1)




#Two Proportion Z-test to determine if the true number of manual transmission vehicles
#is less than the true number automatic transmission vehicles. Our P-value is less than
#2e^-16, very strongly suggesting that there are indeed less manual transmission
#vehicles than there are automatic transmission.
manual = nrow(vehicles[vehicles$Transmission.Type == 'MANUAL', ]) + 
          nrow(vehicles[vehicles$Transmission.Type == 'AUTOMATED_MANUAL', ])
automatic = nrow(vehicles) - manual
prop.test(x = c(manual, automatic), n = c(nrow(vehicles), nrow(vehicles)),alternative = 'less')




#Logistic Regression to use highway MPG to predict if a car will have more than
#300 horsepower.
for (row in 1:nrow(vehicles)){
  if(vehicles$Engine.HP[row] >= 300) {
    vehicles$overthree[row] = 1
  }
  else {
    vehicles$overthree[row] = 0
  }
}
logistic <- glm(overthree~ vehicles$highway.MPG, family= 'binomial', data = vehicles)
summary(logistic)
plot(vehicles$highway.MPG, vehicles$overthree, main = 'Using fuel economy to predict if a vehicle has more than 300 hp', xlab= 'Fuel Ecomony', ylab= 'If a car has more than 300HP' )
abline(lm(overthree ~ vehicles$highway.MPG, data = vehicles))
predY<- predict.glm(logistic, type="response")
points(vehicles$highway.MPG, predY, col = 2, lty = 2)
barplot(table(vehicles$overthree), main = 'Horsepower of Cars', 
        names.arg = c('Less Than 300 HP', '300 or More HP'),
        ylab= 'Frequency')


#The most initially obvious trend in our data was the significantly above-average. 
#The mean highway MPG of electric vehicles was 99.59 while the mean highway MPG
#of non-electric vehicles was only 26.23. This immediately concerned, as having
#a small class of radically different vehicles would skew any analysis on vehicle 
#fuel efficiency. As such, we decided that our analysis of the dataset would not 
#include electric vehicles.
#
#Through several hypothesis tests, we found that the true difference in mean horsepower
#of compact and midsize vehicles are less than zero, that highway mpg differs by vehicle
#make, and that the mean city and highway mpg's were not equal. It was also found that 
#vehicle transmission type and driven wheels are related and that true number of manual
#vehicles is less than the true number of automatic vehicles.
#
#A multi-linear regression suggested that an engine's number of cylinders had a 
#negative correlation with its vehicle's highway MPG and a positive correlation with 
#horsepower.
#
#A logistic regression suggests that vehicles are more likely to achieve a highway MPG that
#is greater than 30MPG have less than 300 horsepower and that high horsepower values 
#are negatively correlated with highway fuel economy. Our Chi Squared test for indepencence 
#that was ran on city and highway MPG would also suggest that high horsepower also is 
#negatively correlated to city MPG. 
###########################################################

