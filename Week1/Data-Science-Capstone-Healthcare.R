library(dplyr)

#Path of dataset
setwd("D:/aVDHOOT/SimpliLearn/Data Science Caption/Project 2/Healthcare - Diabetes")
getwd()

#Loading Dataset
data<-read.csv("health care diabetes.csv")

#Discriptive Analysis
View(data)
str(data)
summary(data)

#Handling Missing Values
table(is.na(data))

hist(data$Glucose,main="Frequency of Glucose",breaks = 8,col="darkorange")
table(data$Glucose)

data$Glucose[data$Glucose==0]<-mean(data$Glucose)

hist(data$BloodPressure,main="Frequency of BloodPressure",breaks = 8,col="darkorange")
table(data$Glucose)

data$BloodPressure[data$BloodPressure==0]<-mean(data$BloodPressure)

hist(data$SkinThickness,main="Frequency of SkinThickness",breaks = 8,col="darkorange")
table(data$SkinThickness)

data$SkinThickness[data$SkinThickness==0]<-mean(data$SkinThickness)

hist(data$Insulin,main="Frequency of Insulin",breaks = 8,col="darkorange")
table(data$Insulin)

data$Insulin[data$Insulin==0]<-mean(data$Insulin)

hist(data$BMI,main="Frequency of BMI",breaks = 8,col="darkorange")
table(data$BMI)

data$BMI[data$BMI==0]<-mean(data$BMI)

