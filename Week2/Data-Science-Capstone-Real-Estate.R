library(dplyr)
library(plyr)
library(imputeTS)
library(ggplot2)
library(maps)
library(mapdata)

#Path of dataset
setwd("D:/aVDHOOT/SimpliLearn/Data Science Caption/Project_1/Project 1")
getwd()

#Loading Dataset
data_train<-read.csv("train.csv")
data_test<-read.csv("test.csv")

#Discriptive Analysis
View(data_train)
str(data_train)
summary(data_train)

View(data_test)
str(data_test)
summary(data_test)

#Handling Missing Values
sapply(data_train, function(x) sum(is.na(x)))
sapply(data_test, function(x) sum(is.na(x)))

#Drop column
data_train<-data_train[,-c(2,3)]
data_test<-data_test[,-c(2,3)]

#Filling missing values with Mean
data_train<-na_mean(data_train)
data_test<-na_mean(data_test)

#Top 2500 with a second mortgage is the highest and percent ownership is above 10 percent
top2500<-subset(data_train, pct_own > 0.1 & second_mortgage<0.5)
head(arrange(top2500,desc(second_mortgage)), n = 2500)

colnames(top2500)
mapsr<-top2500[,c(7,12,13)]

map("county")
points(mapsr$lng,mapsr$lat, col = "Orange", cex = .01)

#Pie chart
bad_debt<-sum((data_train$second_mortgage+data_train$home_equity)-data_train$home_equity_second_mortgage)
debta<-sum(data_train$debt)
debts<-c(debta,bad_debt)
lbl<-c("Debt","Bad Debt")
pie( debts, labels = lbl,
     main="Debt Pie Chart")

#Box and whisker plot
# Store the graph
box<-subset(data_train, city=="Chicago" | city=="Los Angeles")

#Box plot second mortage
box_plot <- ggplot(box, aes(x = city, y = second_mortgage))
# Add the geometric object box plot
box_plot +
  geom_boxplot()

#Box plot Home equity
box_plot <- ggplot(box, aes(x = city, y = home_equity))
# Add the geometric object box plot
box_plot +
  geom_boxplot()

#Box plot debt
box_plot <- ggplot(box, aes(x = city, y = debt))
# Add the geometric object box plot
box_plot +
  geom_boxplot()

#Box plot bad_debt
bad_debt<-(box$second_mortgage+box$home_equity)-box$home_equity_second_mortgage
bad<-cbind(box,bad_debt)
box_plot <- ggplot(bad, aes(x = city, y = bad_debt))
# Add the geometric object box plot
box_plot +
  geom_boxplot()

#Distribution chart for family income, house hold income, and remaining income
hist(data_train$hi_mean,main="Household income distribution chart",breaks = 8,xlab = "Household Income",col="darkorange")
hist(data_train$family_mean,main="Family income distribution chart",breaks = 8,xlab = "Family Income",col="darkorange")
hist(data_train$family_mean-data_train$hi_mean,main="Remaining income distribution chart",xlab ='Reamaining Income' ,breaks = 8,col="darkorange")

## Week:2
#Distribution chart for total population,male and female population,male and female median age
hist(data_train$pop,main="Total population distribution chart",breaks = 8,xlab = "Population",col="darkorange")
hist(data_train$male_pop,main="Male population distribution chart",breaks = 8,xlab = "Male population",col="darkorange")
hist(data_train$pop,main="Female population distribution chart",breaks = 8,xlab = "Female population",col="darkorange")
hist(data_train$male_age_median,main="Age median of male distribution chart",breaks = 8,xlab = "Age median of male",col="darkorange")
hist(data_train$female_age_median,main="Age median of female distribution chart",breaks = 8,xlab = "Age median of female",col="darkorange")

data_train["pop_density"]<-data_train["pop"]/data_train["ALand"]
data_test["pop_density"]<-data_test["pop"]/data_test["ALand"]

hist(data_train$pop_density,main="Population density distribution chart",breaks = 8,xlab = "Population Density",col="darkorange")
hist(data_test$pop_density,main="Population density distribution chart",breaks = 8,xlab = "Population Density",col="darkorange")

#Use male_age_median, female_age_median, male_pop, and female_pop to create a new field called median age
data_train["median_age"]<-(data_train$male_age_median+data_train$female_age_median)/2
data_test["median_age"]<-(data_test$male_age_median+data_test$female_age_median)/2

data_train[,c('male_age_median','female_age_median','male_pop','female_pop','median_age')]

hist(data_train$median_age,main="Median age distribution chart",breaks = 8,xlab = "Median Age",col="darkorange")
hist(data_test$median_age,main="Median age distribution chart",breaks = 8,xlab = "Median Age",col="darkorange")

summary(data_train["pop"])

data_train["pop_bin"]<-cut(data_train$pop,breaks = 5,labels=c('very low','low','medium','high','very high'))
data_train[,c("pop","pop_bin")]
d<-data_train[,c("pop_bin",'married','separated','divorced')]

#rent as a percentage of income at an overall level, and for different states
a<-aggregate(rent_mean ~state,data_train, mean )
b<-aggregate(family_mean ~state,data_train, mean )
d<-inner_join(a, b, by = "state")
rent_perc_of_income<-data.frame(d$state,d$rent_mean/d$family_mean)
rent_perc_of_income
overall_rent_perc_of_income<-sum(data_train$rent_mean)/sum(data_train$family_mean)
overall_rent_perc_of_income

#correlation analysis and heatmap
cor(data_train[,c("COUNTYID","STATEID","zip_code","pop","family_mean",'second_mortgage', 'home_equity', 'debt','hs_degree','median_age','pct_own', 'married','separated', 'divorced')])
heatmap(cor(data_train[,c('COUNTYID','STATEID','zip_code','pop', 'family_mean',
                          'second_mortgage', 'home_equity', 'debt','hs_degree',
                          'median_age','pct_own', 'married','separated', 'divorced')]))

