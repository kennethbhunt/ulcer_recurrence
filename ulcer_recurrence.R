#Data set: ulcer_recurence.csv
#Predict whether a patient presents ulcer (result), with the following variables: age,
#duration and visit. Use the following prediction methods:
#- support vector machine
#- neural networks





Ulcer <- read.csv('ulcer_recurrence.csv', stringsAsFactors = F)

#Check for missing values 
sapply(Ulcer, function(x) sum(is.na(x)))

###Suport Vector Machine

n<-sample(43, 20)
ulcer_train <- Ulcer[n, ]
Ulcer_test <-Ulcer[-n, ]

library(e1071)

fit <- svm(result~., data=ulcer_train, 
           type= "C-classification", kernel="linear", cost=4)
pred <-predict(fit, Ulcer_test)
mean(pred==Ulcer_test$result)

#73.9%

###To find best cost (Improve accuracy)
###10 fold cv 
t_lin <- tune.svm(result~., data=ulcer_train, cost = 2^(2:8), kernal="linear")
t_lin$best.parameters

#####Neural Networks 
library(neuralnet)

net <- neuralnet(result~age+duration+visit, 
                 data=ulcer_train, hidden=1, algorithm ="rprop+", 
                 err.fct = "sse", act.fct = "logistic", 
                 rep = 1, stepmax = 1e06, threshold = 0.01, linear.output = F)

plot(net)

#error 37.73%

##Plot neural network without weights
plot(net, show.weights = F)

#Generate the main results error
net$result.matrix

#Generate weights list 
net$weights

## Predictions in the test set 
pred <-compute(net, Ulcer_test[,-4])
pred2 <-pred$net.result
head(pred2,5)

## Create a categorical predicted value 
predcat <-ifelse(pred2<0.5, 0,1)
predcat

####KNN

Ulcer<-Ulcer[complete.cases(Ulcer),]

#standardize the predictors 
Ulcer2 <- data.frame(scale(Ulcer[1:3]))

Ulcer2 <-cbind(Ulcer2, result=Ulcer$result)

n <-sample(43, 20)
ulcer_train <- Ulcer2[n, ]
ulcer_test <-Ulcer2[-n, ]
library(class)

#define the target variable in the training set
result <-ulcer_train$result

pred <- knn(train=ulcer_train[,-4], test = ulcer_test[,-4], cl=result, k=10)

#get the prediction accuracy
mean(pred==ulcer_test$result)
#82%

###Find optimal number of neighbors
t_knn <-tune.knn(ulcer_train[,-4], factor(ulcer_train[,4]), k=1:100)
t_knn #best is 19
plot(t_knn)

#Run knn with optimal k 
pred <- knn(train=ulcer_train[,-4], test = ulcer_test[,-4], cl=result, k=7)

#get the prediction accuracy
mean(pred==ulcer_test$result)
#86.9%


