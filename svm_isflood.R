# imports
library(kernlab)
library(e1071)        # SVM methodology
library(RColorBrewer) # customized coloring of plots
library(ggplot2)
library(caret)
library(pROC)

# data extraction
uniondata = read.csv("05054000+05082500_new_predict.csv")
uniondata = uniondata[-1][-1]
correlationmatrix = cor(uniondata)
highlycorr = findCorrelation(correlationmatrix, cutoff = 0.75)
print(highlycorr)
highlycorr = highlycorr[-15]
uniondata = uniondata[,-highlycorr]
uniondata = uniondata[-7][-2]
uniondata$Flood.type = ifelse((uniondata$Flood.type == 1 | uniondata$Flood.type == 2 | uniondata$Flood.type == 3), 1, 0)

# data split
set.seed(3033)
uniondata$Flood.type = as.factor(uniondata$Flood.type)
intrain <- createDataPartition(y = uniondata$Flood.type, p= 0.75, list = FALSE)
training <- uniondata[intrain,]
testing <- uniondata[-intrain,]

#names(uniondata)
#svmdata = uniondata[-1]
#svmdata <- scale(svmdata)
#svmdata <- data.frame(svmdata)
#svmdata$Severity = factor(svmdata$Severity)
#x=model.matrix(svmdata$Severity~.,svmdata)
#y=svmdata$Severity
#svmtrain = sample(1:nrow(svmdata),0.5*nrow(svmdata))
#svmrest = svmdata[-svmtrain,]
#svmvalid = sample(1:nrow(svmrest), 0.5*nrow(svmrest))
#svmtest = -svmvalid
#svmtv = c(svmtrain, svmvalid)
#svmcolnames = c("TurbidityMin", "TurbidityMean", "TurbidityMax", "Discrage", "OxygenMax", "OxygenMin", "OxygenMean", "TempMax", "TempMin", "TempMean", "ConductMax", "ConductMin", "ConductMean", "pHMax", "pHMin", "pHMed", "Flood", "Severity", "Month")
#svmdata$Severity = ifelse(svmdata$Severity == 1, "Yes", "No")

# linar svm
# attach(uniondata)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)

svm_Linear <- train(Flood.type~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 4)

svmlinear.pre <- predict(svm_Linear,testing)
confusionMatrix(svmlinear.pre, testing$Flood.type) #0.1271 
y_pre = as.numeric(svmlinear.pre)
y_acc = as.numeric(testing$Flood.type)
svmlinear.roc <- roc(y_acc, y_pre)
plot(svmlinear.roc, print.auc=TRUE, auc.polygon=TRUE, 
     grid=c(0.1, 0.2),grid.col=c("green", "red"), 
     max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE)
svmlinear.imp <- varImp(svm_Linear,scale=F)
plot(svmlinear.imp,top=10)

# reduce flood type, gage height and discharge
# uniondata = uniondata[-18]
# uniondata = uniondata[-8][-4]

# radial kernel
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)

svm_Radial <- train(Flood.type ~., data = training, method = "svmRadial",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svmradial.pre <- predict(svm_Radial,testing)
confusionMatrix(svmradial.pre, testing$Flood.type)# tpr = 0.5580 
acc = mean(svmradial.pre == testing$Flood.type) 
y_pre = as.numeric(svmradial.pre)
y_acc = as.numeric(testing$Flood.type)
svmradial.roc <- roc(y_acc, y_pre)
plot(svmradial.roc, print.auc=TRUE, auc.polygon=TRUE, 
     grid=c(0.1, 0.2),grid.col=c("green", "red"), 
     max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE)
ggroc(svmradial.roc, colour = "red")
svmradial.imp <- varImp(svm_Radial,scale=F)
plot(svmradial.imp,top=10, main = "is_flood")

# ploting
# uniondata = read.csv("05054000+05082500_new_predict.csv")
# uniondata = uniondata[-1][-1]
# correlationmatrix = cor(uniondata)
# highlycorr = findCorrelation(correlationmatrix, cutoff = 0.75)
# print(highlycorr)
# uniondata = uniondata[,-highlycorr]
# set.seed(3033)
# uniondata$Severity = as.factor(uniondata$Severity)
# intrain <- createDataPartition(y = uniondata$Severity, p= 0.75, list = FALSE)
# training <- uniondata[intrain,]
# testing <- uniondata[-intrain,]
# trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# set.seed(3233)
# 
# svm_Linear <- train(Severity~., data = training, method = "svmLinear",
#                     trControl=trctrl,
#                     preProcess = c("center", "scale"),
#                     tuneLength = 4)
# svm_Radial <- train(Severity ~., data = training, method = "svmRadial",
#                     trControl=trctrl,
#                     preProcess = c("center", "scale"),
#                     tuneLength = 10)
# svmlinear.imp <- varImp(svm_Linear,scale=F)
# plot(svmlinear.imp,top=10)
# svmradial.imp <- varImp(svm_Radial,scale=F)
# plot(svmradial.imp,top=10)

# messy variables
uniondata = read.csv("05054000+05082500_new_predict.csv")
uniondata = uniondata[-1][-1]
data3 = uniondata[-19][-8][-4]
set.seed(3033)
data3$Flood.type = ifelse((data3$Flood.type == 1 | data3$Flood.type == 2 | data3$Flood.type == 3), 1, 0)
data3$Flood.type = as.factor(data3$Flood.type)
intrain <- createDataPartition(y = data3$Flood.type, p= 0.75, list = FALSE)
training <- data3[intrain,]
testing <- data3[-intrain,]

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)
# linear
svm_Linear <- train(Flood.type ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svmlinear.pre <- predict(svm_Linear,testing)
confusionMatrix(svmlinear.pre, testing$Flood.type)# tpr = 0.2486

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)
# nonlinear
svm_Radial <- train(Flood.type ~., data = training, method = "svmRadial",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svmradial.pre <- predict(svm_Radial,testing)
confusionMatrix(svmradial.pre, testing$Flood.type)# tpr = 0.7514
