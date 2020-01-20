packages <- c("dummies","descr","ROSE","ggplot2", "dplyr", "MASS" ,"pROC","caret","e1071","corrplot","data.table","Amelia","arm","ModelMetrics","randomForest","MLmetrics","psych","lubridate","car","rpart")

lapply(packages, library, character.only = TRUE)
#install.packages("randomForest")
#library(randomForest)
rsq <- function (x, y) cor(x, y) ^ 2
regressionTdf <- read.csv("E:\\NCI\\Sem-1\\DMML1\\Bike-Sharing-Dataset\\day.csv",header=TRUE,sep=",")

#########################Data Preprocessing###################################
str(regressionTdf)
#regressionTdf$dteday <- as.Date(regressionTdf$dteday)

regressionTdf$holiday <- as.factor(regressionTdf$holiday)
regressionTdf$weekday <- as.factor(regressionTdf$weekday)
regressionTdf$workingday <- as.factor(regressionTdf$workingday)
regressionTdf$holiday <- as.factor(regressionTdf$holiday)
regressionTdf$weathersit <- as.factor(regressionTdf$weathersit)
regressionTdf$yr <- as.factor(regressionTdf$yr)
regressionTdf$mnth <- as.factor(regressionTdf$mnth)

########################Data Distribution############################
hist(regressionTdf$cnt)

regressionTdf = regressionTdf[,c(-2,-11,-14,-15)]
set.seed(1400)
id  <- sample(2,nrow(regressionTdf),prob= c(0.70,0.30),replace=T)
rtTrain <-regressionTdf[id==1,]
rtTest <-regressionTdf[id==2,]

rtModel <- rpart(cnt ~ ., data = rtTrain, method = "anova")

summary(rtModel)
rsq.rpart(rtModel)
rtPredict <- predict(rtModel,rtTest)

cor(rtPredict,rtTest$cnt)

print("RMSE")
sqrt(mean((rtPredict-rtTest$cnt)^2))
rsq(rtTest$cnt,rtPredict)          #0.75
rmse(rtTest$cnt,rtPredict)        #135.25
