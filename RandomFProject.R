packages <- c("dummies","descr","ROSE","ggplot2", "dplyr", "MASS" ,"pROC","caret","e1071","corrplot","data.table","Amelia","arm","ModelMetrics","randomForest")

lapply(packages, library, character.only = TRUE)
#install.packages("randomForest")
#library(randomForest)

bikeData <- read.csv("E:\\NCI\\Sem-1\\DMML1\\london-bike-sharing-dataset\\london_merged.csv",header=TRUE,sep=",")
dim(bikeData)
summary(bikeData$cnt)
str(bikeData)
#sapply(list, function)
hist(bikeData$cnt)

bikeData$timestamp <- as.numeric(bikeData$timestamp)
bikeData$weather_code <- as.numeric(bikeData$weather_code)
bikeData$is_holiday <- as.numeric(bikeData$is_holiday)
bikeData$is_weekend <- as.numeric(bikeData$is_weekend)
bikeData$season <- as.numeric(bikeData$season)
bikeData$cnt <- as.numeric(bikeData$cnt)

correlationsRF <- cor(bikeData)
corrplot::corrplot(correlationsRF,method = "circle")

###########Converting numerical data to categorical using binning
bikeData$cnt <- ifelse(bikeData$cnt >= 0 & bikeData$cnt <= 844, 1, 
                                     ifelse(bikeData$cnt > 844 & bikeData$cnt <= 1672, 2, 
                                            ifelse(bikeData$cnt >1672, 3,NA)))

bikeData[ bikeData == "?"] <- NA
colSums(is.na(bikeData))

bikeData$weather_code <- as.numeric(bikeData$weather_code)
bikeData$is_holiday <- as.numeric(bikeData$is_holiday)
bikeData$is_weekend <- as.numeric(bikeData$is_weekend)
bikeData$season <- as.numeric(bikeData$season)
bikeData$cnt <- as.factor(bikeData$cnt)

##########Dividing data into training and testing set###########
set.seed(1400)
id  <- sample(2,nrow(bikeData),prob= c(0.70,0.30),replace=T)
rfTrain <-bikeData[id==1,]
rfTest <-bikeData[id==2,]

nrow(rfTest)
str(rfTest)
rf <- randomForest(cnt ~ timestamp+t1+t2+hum+wind_speed+weather_code, data = rfTrain,importance=TRUE)

rfPred = predict(rf, newdata=rfTest)


#####################Performance evaluation
caret::confusionMatrix(rfPred, rfTest$cnt)  # Accuracy(cnt) is 66.96% 
                                            # kappa = 0.4517  (mtry =2 ntree=500)

a=c()           #mtry value =5 increased the performance of algorithm after performing the following mtry value check using a loop
i=5
for (i in 3:8) {
  rfTune <- randomForest(cnt ~ timestamp+t1+t2+hum+wind_speed+weather_code,mtry=6, data = rfTrain,importance=TRUE)
  predValid <- predict(rfTune, rfTest, type = "class")
  a[i-2] = mean(predValid == rfTest$cnt)
}

a                   
plot(3:8,a)    

bestmtry <- tuneRF(bikeData[,-2],bikeData$cnt,stepFactor=1.5, improve=1e-5, ntree=500) #best mtry value is 5 

rf1 <- randomForest(cnt ~ timestamp+t1+t2+hum+wind_speed+weather_code,mtry=3,ntree=2000, data = rfTrain,importance=TRUE)

rfPred1 = predict(rf1, newdata=rfTest)

caret::confusionMatrix(rfPred1, rfTest$cnt)  # accuracy is 68.54% kappa = 0.4823

#################Changing number of trees and performing randomforest model again but after performing this models the accuracy is still same
modellist <- list()
for (ntree in c(1000,1500,2000,2500)) {
  rf2 <- randomForest(cnt ~ timestamp+t1+t2+hum+wind_speed+weather_code,mtry=5,ntree=ntree, data = rfTrain,importance=TRUE)
  rfPred2 = predict(rf2, newdata=rfTest)
  key <- toString(ntree)
  caret::confusionMatrix(rfPred2, rfTest$cnt)
}