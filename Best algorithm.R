#Imports/installs
install.packages("caret")
library(caret)

install.packages("e1071")
library(e1071)

install.packages("tree")
library(tree)

install.packages("e1071")
library(e1071)
sdate <- as.Date("2022-01-03")
edate <- as.Date("2022-12-05")
allData<-specialMerge(tickers,"2022-01-03","2022-12-05")

#1 train model function, Should train model to predict next week's stock price change
allData<-specialMerge(tickers,sdate,edate)
trainML<-function(trainingSplit,allData,factorColName)
{
  #partioning Data
  trainingRows<-createDataPartition(allData$vital, p=trainingSplit, list=FALSE)
  train <- allData[c(trainingRows),]
  test <- allData[c(-trainingRows),]
  
  #training Model
  modelT <- tree(vital ~ ., data = train) 
  
  #testing Model

  predT<-predict(modelT, newdata = test)
  
  return(predT) # assuming this return statement is valid!
}

plotVector<-as.vector(unlist(predT))
plotTest<-as.vector(unlist(test$vital))
plot(x=1:length(plotVector),y=plotVector)
par(new=TRUE)
plot(1:length(plotTest),plotTest,col=2)
legend(1,1,x="bottomright", legend=c("Actual","Predicted"), fill = c("red","black"))

