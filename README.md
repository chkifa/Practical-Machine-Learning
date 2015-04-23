---
title: "PA-PML"
author: "chkifa"
date: "Monday, April 20, 2015"
output: html_document
---
##Goal of studie
Aim of this project is to predict the manner in which the six young health participants did the exercise.represented in the training set with the "classe" variable : 
  
- **Class A** : exactly according to the specification,  
- **Class B** : throwing the elbows to the front,   
- **Class C** : lifting the dumbbell only halfway,   
- **Class D** : lowering the dumbbell only halfway,   
- **Class E** : throwing the hips to the front.  

###Load the required packages  

```{r warning=FALSE}
library(caret)
library(rpart)
# set a random seed for reproducing results.
set.seed(8888)
```


##Load the dataset

first load the data in to **Rconsole** :

```{r echo = TRUE ,cache = TRUE}
pml_training <- read.csv("pml-training.csv",head=TRUE,sep=",")
dim(pml_training)
```

##Data preprocessing (Reducing the number of attributes)

Removing irrelevant attributes and missing information present in pml_training data set, so we focus our study in only 53 mesured variables:

###Training Dataset:
```{r}
missing_data<-sapply(pml_training,function(x) any(is.na(x)| x==""))
releventVar<-grepl("belt|arm|dumbbell|forearm|classe", names(missing_data))
predVar<-c(!missing_data & releventVar)
pml_train<-pml_training[,predVar]
dim(pml_train)
```

###Testing Dataset:
```{r}
pml_testing <- read.csv("pml-testing.csv",head=TRUE,sep=",")
missing_data_test<-sapply(pml_testing,function(x) any(is.na(x)| x==""))
releventVar_test<-grepl("belt|arm|dumbbell|forearm|problem_id", names(missing_data_test))
predVar_test<-c(!missing_data_test & releventVar_test)
pml_test<-pml_testing[,predVar_test]
dim(pml_test)
#verify identical variables for train and test?
identical(names(pml_train[,-53]),names(pml_test[,-53]))
```

##Data Cross Validation
Partioning the data set into two random sample, 75% for "training" and  25% for "testing". And fit the model using training data and then predit using testing data.

```{r}
inTrain <- createDataPartition(y=pml_train$classe,p=0.75, list=FALSE)
training <- pml_train[inTrain,]
testing <- pml_train[-inTrain,]
dim(training);dim(testing)
```

### algorithms for prediction : Decision Tree
we have choice the decision tree algorithme with complixity equal a 0 and 10 cross-validations.
```{r}
model <- rpart(classe ~ .,control=rpart.control(xval=10, cp=0), data=training)
plotcp(model)

```

##Model performance  

```{r}
prediction <- predict(model,type="class", newdata = testing)
tab1<-table(prediction,testing$classe)
tab1
```


The resubstitution error rate:
```{r}
1-sum(diag(tab1))/sum(tab1)
```

##Prediction result on the test data
Finally, using the model to predict how the six young health participants did the exercise in the 20 observation test data:

```{r}
prediction_id <- predict(model,newdata=pml_test,type="class")
prediction_id
```
