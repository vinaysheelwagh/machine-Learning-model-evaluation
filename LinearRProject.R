packages <- c("dummies","descr","ROSE","ggplot2", "dplyr", "MASS" ,"pROC","caret","e1071","corrplot","data.table","Amelia","arm","ModelMetrics","randomForest","scipe=999","MLmetrics","psych","rpart.plot","forecast","lubridate","tidyverse","car")

lapply(packages, library, character.only = TRUE)
#install.packages("randomForest")
#library(randomForest)

rsq <- function (x, y) cor(x, y) ^ 2

bikeRegData <- read.csv("E:\\NCI\\Sem-1\\DMML1\\Bike-Sharing-Dataset\\hour.csv",header=TRUE,sep=",")

str(bikeRegData)
summary(bikeRegData$temp)
hist(bikeRegData$temp)
#Data selection########
correlations <- cor(bikeRegData[,-2])
corrplot(correlations)
#Removing unwanted variables
bikeRegData <- bikeRegData[,-c(1,2,15,16)]
bikeRegData <- bikeRegData[,-c(2,4)]

bikeRegData$season <- as.factor(bikeRegData$season)
bikeRegData$mnth <- as.factor(bikeRegData$mnth)
bikeRegData$holiday <- as.factor(bikeRegData$holiday)
bikeRegData$weekday <- as.factor(bikeRegData$weekday)
bikeRegData$workingday <- as.factor(bikeRegData$workingday)
bikeRegData$weathersit <- as.factor(bikeRegData$weathersit)
#df$day <- factor(df$day)
str(bikeRegData)

cor(bikeRegData[,c(7,8,9,10,11)])     #temp and atemp variables are almost the same, these are very similar

bikeRegData <- bikeRegData[,-c(8)]
str(bikeRegData)

###################Data Distribution###########################3

#bikeRegData <- subset(bikeRegData, select = c(7,8,9,10))
set.seed(1400)
id  <- sample(2,nrow(bikeRegData),prob= c(0.70,0.30),replace=T)
lrTrain <-bikeRegData[id==1,]
lrTest <-bikeRegData[id==2,]

bikeLinearModel <- lm(cnt ~ .,data = lrTrain)  #Applying basic Linear Regression model
summary(bikeLinearModel)
prediction <- predict(bikeLinearModel,lrTest[,-10])
head(prediction)

print("RMSE::")
rmse(lrTest$cnt,prediction)
rsq(lrTest$cnt,prediction)

#################Performance tunning#################################################################
stepLR <- stepAIC(bikeLinearModel, direction="both",steps = 250)

bikeLinearmodel <-lm(cnt ~ season + mnth + holiday + weathersit + temp + hum + windspeed,data = lrTrain)

summary(bikeLinearmodel)

lrPredict <- predict(bikeLinearModel,lrTest[,-10])
str(lrPredict)       

print("RMSE::")
rmse(lrTest$cnt,lrPredict)
#########################################Performance tunning#######################################################################

set.seed(1400)
cv_model_pcr <- train(
  cnt ~ season + mnth + holiday + weathersit + temp + hum + windspeed, 
  data = lrTrain, 
  method = "pcr",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 50
)
summary(cv_model_pcr)

ggplot(cv_model_pcr) #RMSE value is drastically decreased after applying pcr model

prediction <- predict(cv_model_pcr,lrTest[,-10])
rmse(lrTest$cnt,prediction)
rsq(lrTest$cnt,prediction)