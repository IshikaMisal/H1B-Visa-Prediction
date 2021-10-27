#Final Code

#Cleaning Part 
rm(list=ls())
library(dplyr)
set.seed(123)
visa<-read.csv("C://Users/vinay/Desktop/H-1B_Disclosure_Data_FY2019.csv",na.strings = '')#Change the path accordingly.
View(visa)
summary(visa)
visa<-na.omit(visa)
visa_var<-c("CASE_STATUS","DECISION_MONTH","EMPLOYER_STATE","FULL_TIME_POSITION","PREVAILING_WAGE","H1B_DEPENDENT")
visa1<-visa[visa_var]
visa1<-mutate(visa1, Wage_rank = ntile(visa1$PREVAILING_WAGE,15))
visa1$DECISION_MONTH <- factor(visa1$DECISION_MONTH)
visa1$Wage_rank<-factor(visa1$Wage_rank)
visa1<-visa1[-5]
visa1<-visa1[-3]
idx<-sort(sample(nrow(visa1),as.integer(.10*nrow(visa1))))
visa2<-visa1[idx,]
idx1<-sort(sample(nrow(visa2),as.integer(.70*nrow(visa2))))
training<-visa2[idx1,]
test<-visa2[-idx1,]
#Till this part needs to be exicuted before each and every classification is exicuted.
#CART
library(rpart)
library(rpart.plot)
library(rattle)  
library(RColorBrewer)
model<-rpart(CASE_STATUS~EMPLOYER_STATE,training)
rpart.plot(model)
#Prediction using test 
prediction<-predict(model,test,type="class")
#c)Score the test dataset
conf_matrix<-table(test$CASE_STATUS)
conf_matrix
str(prediction)
#d)Measure the error rate.  
wrong<-sum(test$CASE_STATUS!=prediction)
error_rate<-wrong/length(test$CASE_STATUS)
error_rate

visa3<-visa2
visa3<-visa3[-3]
idx2<-sort(sample(nrow(visa3),as.integer(.70*nrow(visa3))))
training1<-visa3[idx2,]
test1<-visa3[-idx2,]

library(rpart)
library(rpart.plot)
library(rattle)  
library(RColorBrewer)
model<-rpart(CASE_STATUS~.,training1)
rpart.plot(CASE_STATUS~.,training1,box.palette = "blue")
rpart.plot(model)
#Prediction using test 
prediction<-predict(model,test1,type="class")
#c)Score the test dataset
conf_matrix<-table(test1$CASE_STATUS)
conf_matrix
str(prediction)
#d)Measure the error rate.  
wrong<-sum(test1$CASE_STATUS!=prediction)
error_rate<-wrong/length(test1$CASE_STATUS)
error_rate

fancyRpartPlot(model)
prp(model)
#C5.0
library(C50)
model<-C5.0(Wage_rank~.,training)
summary(model)
  plot(model)
#Prediction using test 
prediction<-predict(model,test,type="class")
#c)Score the test dataset
conf_matrix<-table(test$CASE_STATUS)
conf_matrix
str(prediction)
#d)Measure the error rate.  
wrong<-sum(test$CASE_STATUS!=prediction)
error_rate<-wrong/length(test$CASE_STATUS)
error_rate
#Random Forest
library(randomForest)
model<-randomForest(CASE_STATUS~.,training,importance=TRUE, ntree=1000)
importance(model)
varImpPlot(model)
Prediction <- predict(model, test)
#Forming the confusin matrix
table(actual=test$CASE_STATUS,Prediction)
#Showing the error rate
wrong<- (test$CASE_STATUS!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate 
#KNN
library(kknn)
predict_k3 <- kknn(formula=CASE_STATUS~., training, test, k=3,kernel ="rectangular" )
fit <- fitted(predict_k3)
table(test$CASE_STATUS,fit)
wrong<-sum(test$CASE_STATUS!=fit)
error_rate<-wrong/length(test$CASE_STATUS)
error_rate
a=0
for(i in 1:40){
  predict_k3<-kknn(formula=CASE_STATUS~., training, test, k=i,kernel ="rectangular" )
  fit <- fitted(predict_k3)
  table(test$CASE_STATUS,fit)
  wrong<-sum(test$CASE_STATUS!=fit)
  error_rate<-wrong/length(test$CASE_STATUS)
  accuracy<-(1-error_rate)
  a[i]<-accuracy
}

a

x<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40)

plot(x,a)



