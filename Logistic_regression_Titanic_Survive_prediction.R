library(titanic)

titanic_train
head(titanic_train)
str(titanic_train)

summary(titanic_train)
library(dplyr)
tibble(titanic_train)

##Drop NA (missing value)
titanic_train<-na.omit(titanic_train)  ## จะลบobservation row ที่มี NA


##Split Data
set.seed(49)
n<-nrow(titanic_train)
id<-sample(1:n,n*0.7)  ###70% train data, 30% test Data


train_data<-titanic_train[id, ]
test_data<-titanic_train[-id, ]

##Model
titanic_train$Sex<-factor(titanic_train$Sex)
model<-glm(Survived~Pclass+Sex+Age+SibSp+Parch,data = train_data,family = "binomial")
summary(model)

##Prediction Train Value
Train_predicted<-predict(model,type = "response")
train_data$train_predicted<-Train_predicted
result<-ifelse(train_data$train_predicted>=0.5,1,0)
train_data$Result<-result


matrix_confusion_train<-table(train_data$Survived,train_data$Result,dnn = c("Actual","Predict"))

Accuracy_of_train_data<-(matrix_confusion_train[1,1]+matrix_confusion_train[2,2])/sum(matrix_confusion_train)


##Prediction Test Data
Test_predicted<-predict(model,newdata = test_data,type = "response")
test_data$test_prediction<-Test_predicted
test_data$Result<-ifelse(test_data$test_prediction>=0.5,1,0)

matrix_confusion_test<-table(test_data$Survived,test_data$Result,dnn = c("Actual","Predict"))


##Accuracy
Accuracy_of_test_data<-(matrix_confusion_test[1,1]+matrix_confusion_test[2,2])/sum(matrix_confusion_test)
precision<-matrix_confusion_test[2,2]/(matrix_confusion_test[1,2]+matrix_confusion_test[2,2])
recall<-matrix_confusion_test[2,2]/(matrix_confusion_test[2,1]+matrix_confusion_test[2,2])
f1_score<-2*((precision*recall)/(precision+recall))

cat("Accuracy : ",Accuracy_of_test_data, "\nPrecision : ", precision, "\nRecall : ",recall, "\nF1_Score : ",f1_score)




















