# Load packages

packages <- c("dummies","descr","ROSE","ggplot2", "dplyr", "MASS" ,"pROC","caret","e1071","corrplot","data.table","Amelia","arm","ModelMetrics")
lapply(packages, library, character.only = TRUE)

# Load data from csv

shoppersData <- read.csv("C:\\Users\\Vinaysheel Wagh\\Downloads\\online_shoppers_intention.csv",header=TRUE,sep=",")
str(shoppersData)

shoppersData$Revenue<- as.numeric(shoppersData$Revenue)
shoppersData$Month<- as.numeric(shoppersData$Month)
shoppersData$VisitorType<- as.numeric(shoppersData$VisitorType)
shoppersData$Weekend<- as.numeric(shoppersData$Weekend)
str(shoppersData)

shoppersData$Revenue<- as.factor(shoppersData$Revenue)
#shoppersData$Month<- as.factor(shoppersData$Month)
#shoppersData$VisitorType<- as.factor(shoppersData$VisitorType)
#shoppersData$Weekend<- as.factor(shoppersData$Weekend)

#### Data selection

correlations <- cor(shoppersData)
corrplot(correlations, method="circle")

#########TRAINING AND TESTING DATA##############
str(logitrain)
set.seed(1234)
id  <- sample(2,nrow(shoppersData),prob= c(0.70,0.30),replace=T)
logitrain <-shoppersData[id==1,]
logitest <-shoppersData[id==2,]

table(shoppersData$Revenue)             #Imbalance in data Revenue generation cases are very few 1908 and false cases are 10422

'%ni%' <- Negate('%in%')

set.seed(1234)
downlogi_train <- caret::downSample(x = logitrain[, colnames(logitrain) %ni% "Revenue"],y = logitrain$Revenue)

table(downlogi_train$Class)
str(downlogi_train)
#Apply logistic regression model

#logimodel <- glm(Revenue ~ Administrative+Administrative_Duration+Informational+Informational_Duration+ProductRelated+ProductRelated_Duration+BounceRates+ExitRates+PageValues+Month, data=logitrain,family = "binomial")
#library(class)
logimodel <- glm(Class ~ Administrative+Administrative_Duration+Informational+Informational_Duration+ProductRelated+
                   ProductRelated_Duration+PageValues+Month, data=downlogi_train,family = "binomial")
prediction <- predict(logimodel, newdata=logitest, type="response")


pred_cat <- ifelse(prediction > 0.5, 1, 0)
pred_cat_y <- factor(pred_cat,levels=c(0,1))
y_act <- logitest$Revenue
logitest$Revenue<- as.factor(logitest$Revenue)

caret::confusionMatrix(pred_cat_y,logitest$Revenue)   #accuracy is 85.11%


#############Performance tunning#########################
####K-cross fold validation
ctrl <- caret::trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod_fit <- caret::train(Class ~ Administrative+Administrative_Duration+Informational+Informational_Duration+ProductRelated+
                   ProductRelated_Duration+PageValues+Month,  data=downlogi_train, method="glm", family="binomial",trControl = ctrl, tuneLength = 5)

pred = predict(mod_fit, newdata=logitest)
caret::confusionMatrix(data=pred, logitest$Revenue)    #Accuracy is same as initial 
####################ROC########################

#library(ROCR)
#library(ModelMetrics)

logitest$Revenue<- as.numeric(logitest$Revenue)
pred<- as.numeric(pred)

knnTestLabel<- as.factor(knnTest[,2])
knnmodel<- as.factor(knnmodel)

auc(pred,logitest$Revenue)
auc(pred,logitest$Revenue)    #auc = 0.73
auc(knnmodel,knnTest[,2])    #auc = 0.671

roclogistics <- prediction(as.numeric(logitest$Revenue),as.numeric(pred))
rocmodelknn <- prediction(as.numeric(knnmodel),as.numeric(knnTest[,2]))

perf <- performance( roclogistics, "tpr", "fpr" )
perf2 <- performance(rocmodelknn, "tpr", "fpr")
plot( perf, main="AUC ROC Curve",col ="red")
plot(perf2, add = TRUE, col ="blue")


legend("bottomright", c( "Logistic Regression ROC curve","KNN ROC curve"), lty=5, 
       col = c("red", "blue"), bty="n", inset=c(0,0.15))
legend("bottomright", c( "Logistic Regression - AUC = 0.73 ","KNN - AUC = 0.70"), lty=8, 
       col = c("red", "blue"), bty="n", inset=c(0,0.50))
