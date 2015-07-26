##PRACTICAL MACHINE LEARNING COURSE ASSIGNMENT

#INTRODUCTION:
#In this project, your goal will be to use data from accelerometers on the belt,
#forearm, arm, and dumbell of 6 participants. They were asked to perform barbell
#lifts correctly and incorrectly in 5 different ways. 
#More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).  

#Findings:
#1)create a report describing how you built your model
#2)how you used cross validation
#3)what you think the expected out of sample error is
#4) why you made the choices you did
#5) use your prediction model to predict 20 different test cases


#Loading the required pakages  
library(AppliedPredictiveModeling)
library(caret)
library(rattle)
library(rpart.plot)
library(randomForest)

#Getting Data

train.url<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test.url<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
if(!file.exists("./practicalmachinelearning/pml-training.csv")){
  
  download.file(train.url,"./practicalmachinelearning/pml-training.csv")
  
}
if(!file.exists("./practicalmachinelearning/pml-testing.csv")){
download.file(test.url,"./practicalmachinelearning/pml-testing.csv")

}

train.raw<-read.csv("./practicalmachinelearning/pml-training.csv",na.strings=c("NA","#DIV/0!",""))
test.raw<-read.csv("./practicalmachinelearning/pml-testing.csv",na.strings=c("NA","#DIV/0!",""))

#Cleaning Data:
#Dropping all the columns having NA's
  train.clean<-train.raw[,colSums(is.na(train.raw))==0]
  test.clean<-test.raw[,colSums(is.na(test.raw))==0]
#Dropping first seven columns as they are unnecessary for prediction
  train.clean<-train.clean[,8:length(colnames(train.clean))]
test.clean<-test.clean[,8:length(colnames(test.clean))]
#check for nonzero covariants in the set
nvz<-nearZeroVar(train.clean,saveMetrics=TRUE)
zero.var.ind<-sum(nvz$nvz)
if(zero.var.ind>0){
  train.clean<-train.clean[,nvz$nvz==FALSE]
  
}

#Data Slicing

inTrain<-createDataPartition(y=train.clean$classe,p=.6,list=FALSE)
training<-train.clean[inTrain,]
testing<-train.clean[-inTrain,]
#Find the out of box classification tree
set.seed(999)
modelfit<-train(classe~.,data=training,method="rpart")
print(modelfit)
print(modelfit$finalModel)
library(rattle)
fancyRpartPlot(modelfit$finalModel)

#Runing on testing set
predictions<-predict(modelfit,newdata=testing)
print(confusionMatrix(predictions,testing$classe))

#Preprocessing the data to increase the accuracy
modelfit<-train(classe~.,data=training,preProcess=c("center","scale"),trControl=trainControl(method="cv",number=4),method="rpart")
print(modelfit)

predictions1<-predict(modelfit,newdata=testing)
print(confusionMatrix(predictions1,testing$classe))
#The impact of incorporating both preprocessing and cross validation appeared 
#to show some minimal improvement (accuracy rate rose from 0.519 to 0.531
#against training sets). 
#However, when run against the corresponding testing set,
#the accuracy rate was identical (0.4889) for both the "out of the box" 
#and the preprocessing/cross validation methods.

#Random Forest

modelfit<-train(classe~.,method="rf",trControl=trainControl(method = "cv", number = 4),data=training)
print(modelfit)


predictions<-predict(modelfit,newdata=testing)
print(confusionMatrix(predictions,testing$classe))



print(predict(modelfit,newdata=test.clean))

#Finally to submit answers as per assignment

predictionsf1 <- predict(modelfit, newdata=test.clean)
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictionsf1)

