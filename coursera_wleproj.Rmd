---
title: "Practical Machine Learning: Weight Lifting Exercise"
output: html_document
---
##Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 



##Data 


The training data for this project are available here: 

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here: 

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

###Load required libraries
```{r}
library(caret)
library(randomForest)
library(rattle)
library(sqldf)
library(doBy)

```

###Read data
```{r}
pmltraining <- read.csv("/media/S/Temp/pml-training.csv")
pmltesting <- read.csv("/media/S/Temp/pml-testing.csv")

```

###Data preprocessing - remove NA and redundant variables
```{r}
pmltraining[pmltraining=='#DIV/0!']<-NA
nona<-colSums(is.na(pmltraining)) == 0
mlData<-pmltraining[,nona]
mlData<-mlData[,-c(1,3:5)]

```

###Clean testing dataset according to training dataset
```{r}
testing<-pmltesting[,nona]
testing<-testing[,-c(1,3:5)]
```

###Partition training data for validation
```{r}
inTrain<-createDataPartition(y=mlData$classe,p=0.2,list=F)
training<-mlData[inTrain,]
validating<-mlData[-inTrain,]

```

###Exploratory plots
```{r}
toPlot<-sqldf('select
              classe,
              total_accel_belt,
              total_accel_arm,
              total_accel_dumbbell,
              total_accel_forearm
              from training ')
featurePlot(toPlot[,2:5],toPlot[,1],plot='pairs',xlab='total accel')

qplot(total_accel_belt,total_accel_arm,col=classe,data=training,
      main='Predictors by classe')
qplot(total_accel_belt,total_accel_forearm,col=classe,data=training,
      main='Predictors by classe')
qplot(total_accel_dumbbell,total_accel_arm,col=classe,data=training,
      main='Predictors by classe')
```

###Modeling
```{r}
date()
system.time(modFit<-train(classe~.,method='rf',data=training,prox=T))
date()
modFit$finalModel
```

###Validation
```{r}
pred<-predict(modFit,validating)
table(pred,validating$classe)
```

###Testing
```{r}
pred<-predict(modFit,testing)
pred
```
