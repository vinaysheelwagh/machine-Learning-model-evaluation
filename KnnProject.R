# Load packages

packages <- c("dummies","descr","ROSE","ggplot2", "dplyr", "MASS" ,"pROC","caret","e1071","corrplot","data.table","Amelia","arm","ModelMetrics","class")
lapply(packages, library, character.only = TRUE)

knnShoppersData <- read.csv("C:\\Users\\Vinaysheel Wagh\\Downloads\\online_shoppers_intention.csv",header=TRUE,sep=",")

str(knnShoppersData)

knnShoppersData[ knnShoppersData == "?"] <- NA
colSums(is.na(knnShoppersData))

knnShoppersData$VisitorType <- as.numeric(knnShoppersData$VisitorType)
knnShoppersData$Month <- as.numeric(knnShoppersData$Month)
knnShoppersData$Weekend <- as.numeric(knnShoppersData$Weekend)
knnShoppersData$Revenue <- as.numeric(knnShoppersData$Revenue)

str(knnShoppersData)
M <- cor(knnShoppersData)
corrplot(M, method="circle")

knnShoppersData$VisitorType<- as.factor(knnShoppersData$VisitorType)
knnShoppersData$Month<- as.factor(knnShoppersData$Month)
knnShoppersData$Weekend<- as.factor(knnShoppersData$Weekend)
knnShoppersData$Revenue<- as.factor(knnShoppersData$Revenue)

n <- sapply(knnShoppersData, function(x){is.numeric(x)})

numerics <- knnShoppersData[,n]
summary(numerics)

normalize <- function(x){return((x-min(x))/(max(x)-min(x)))}
numericsNormal <- normalize(numerics)
summary(numericsNormal)

shoppersDataKnn <- knnShoppersData[,!n]
shoppersDataKnn <- cbind(shoppersDataKnn,numericsNormal)

str(shoppersDataKnn)

#############Feature selection
shoppersDataKnn<- subset(shoppersDataKnn,select = c(1,4,5,6,7,8,9,10,13))
str(shoppersDataKnn)
#########################
set.seed(1400)
id  <- sample(2,nrow(shoppersDataKnn),prob= c(0.70,0.30),replace=T)
knnTrain <-shoppersDataKnn[id==1,]
knnTest <-shoppersDataKnn[id==2,]
library(class)
knnmodel <- knn(train = knnTrain[,-2],test = knnTest[,-2],cl=knnTrain[,2],k=4)

### Model Evaluation
caret::confusionMatrix(knnmodel, knnTest[,2])     #Accuracy is 84.14%    k=4

#######Performance tunning#############
#knnModel value contains value of resampling results across tunning parameters that can be used to explain performance tunning
i=1                          # declaration to initiate for loop
k.optm=1                     # declaration to initiate for loop
for (i in 1:30){ 
  knn.mod <-  knn(train=knnTrain[,-2], test=knnTest[,-2], cl=knnTrain[,2], k=i)
  k.optm[i] <- 100 * sum(knnTestLabel == knn.mod)/NROW(knnTest[,2])
  k=i  
  cat(k,'=',k.optm[i],'\n')       # to print % accuracy    k=10 gives accuracy of 85.01% after that success rate becomes constant for increase in k value
}                                                       

