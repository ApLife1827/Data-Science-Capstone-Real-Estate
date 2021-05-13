library(dplyr)
library(plyr)
library(imputeTS)
library(ggplot2)

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
good_debt<-sum(data_train$debt)-bad_debt

debts<-c(good_debt,bad_debt)
lbl<-c("Good Debt","Bad Debt")
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

