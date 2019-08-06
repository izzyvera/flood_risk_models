library(randomForest)
library(caret)

# Severity
data=read.csv('05054000+05082500_new_predict.csv')
names(data)
data=data[-1][-1]

# Severity Data
names(data)
Severity.data=data[-31][-18][-8][-4]
names(Severity.data)
Severity.data$Month=as.factor(Severity.data$Month)
Severity.data$Severity=as.factor(Severity.data$Severity)


set.seed(3033)
intrain <- createDataPartition(y = data$Severity, p= 0.75, list = FALSE)
test = -intrain
Severity.test=Severity.data[-intrain ,]
test=Severity.test[,'Severity']

#head(Severity.data[train,])

rf.severity=randomForest(Severity~.,data=Severity.data[intrain,], ntree =1000)
#summary(rf.severity)
yhat.rf = predict(rf.severity,newdata=Severity.test)
#mean((yhat.rf-test)^2) #0.008924397
table(yhat.rf,test)
#tpr 
confusionMatrix(yhat.rf,test)
table(yhat.rf,test)[4]/(table(yhat.rf,test)[4]+table(yhat.rf,test)[3]) #0.7868852
#accuracy=0.9871619
(table(yhat.rf,test)[4]+table(yhat.rf,test)[1])/(table(yhat.rf,test)[1]+table(yhat.rf,test)[2]+table(yhat.rf,test)[4]+table(yhat.rf,test)[3])
par(new = TRUE)
importance(rf.severity)
varImpPlot(rf.severity,scale=F)
plot(varImpPlot(rf.severity,scale=F))


# # flood_type
# #05054000+05082500
# data=read.csv('short_05054000+05082500_new_predict_1.csv')
# names(data)
# data=data[-1][-1]
# 
# # Flood_type
# names(data)
# Flood_type.data=data[-31][-19][-8][-4]
# names(Flood_type.data)
# Flood_type.data$Month=as.factor(Flood_type.data$Month)
# Flood_type.data$Flood.type=as.factor(Flood_type.data$Flood.type)
# 
# 
# factor.data=Flood_type.data[17]
# factor.data=cbind(factor.data,Flood_type.data[16])
# names(factor.data)
# corr.data=Flood_type.data[-17][-16]
# correlationmatrix = cor(corr.data)
# highlycorr = findCorrelation(correlationmatrix, cutoff = 0.75)
# print(highlycorr)
# corr.data = corr.data[,-highlycorr]
# names(corr.data)
# Flood_type.data=cbind(corr.data,factor.data)
# 
# 
# set.seed(3033)
# intrain <- createDataPartition(y = data$Severity, p= 0.75, list = FALSE)
# test = -intrain
# Flood_type.test=Flood_type.data[-intrain ,]
# test=Flood_type.test[,'Flood.type']
# head(Flood_type.test)
# 
# #head(Severity.data[train,])
# rf.Flood_type=randomForest(Flood.type~.,data=Flood_type.data[intrain,],importance=TRUE)
# yhat.rf = predict(rf.Flood_type,newdata=is_flood.test)
# #mean((yhat.rf-test)^2) #0.008924397
# table(yhat.rf,test)
# #tpr 
# (48+10+17)/(45+48+8+1+4+6+10+2+1+5+5+17) #0.4934211
# importance(rf.severity)
# varImpPlot(rf.severity)


# is_flood
#05054000+05082500
data=read.csv('05054000+05082500_new_predict.csv')
names(data)
data=data[-1][-1]

#is_flood
names(data)
is_flood.data=data[-19][-18][-8][-4]
names(is_flood.data)
is_flood.data$Month=as.factor(is_flood.data$Month)
is_flood.data$Is.flood=as.factor(is_flood.data$Is.flood)


# factor.data=is_flood.data[27]
# factor.data=cbind(factor.data,is_flood.data[16])
# names(factor.data)
# corr.data=is_flood.data[-27][-16]
# correlationmatrix = cor(corr.data)
# highlycorr = findCorrelation(correlationmatrix, cutoff = 0.75)
# print(highlycorr)
# corr.data = corr.data[,-highlycorr]
# names(corr.data)
# is_flood.data=cbind(corr.data,factor.data)


set.seed(3033)
intrain <- createDataPartition(y = data$Severity, p= 0.75, list = FALSE)
test = -intrain
is_flood.test=is_flood.data[-intrain ,]
test=is_flood.test[,'Is.flood']


#head(Severity.data[train,])
rf.Is.flood=randomForest(Is.flood~.,data=is_flood.data[intrain,],ntree=1000)
yhat.rf = predict(rf.Is.flood,newdata=is_flood.test)
#mean((yhat.rf-test)^2) #0.008924397
table(yhat.rf,test)
confusionMatrix(yhat.rf,test)
#tpr 
table(yhat.rf,test)[4]/(table(yhat.rf,test)[4]+table(yhat.rf,test)[3]) #0.8101266
importance(rf.Is.flood)
varImpPlot(rf.Is.flood)
yhat.rf
cutoff=0.01

l=ifelse(yhat.rf[,2]>cutoff,1,0)
table(l,test)
table(l,test)[4]/(table(l,test)[4]+table(l,test)[3]) 
table[3]/(table[3]+table[1]) 

roc.p

# #install.packages('ROCR')
# plot(performance(prediction(l, iris$setosa), 'tpr', 'fpr'))
# auc.roc.plot(yhat.rf[,2], threshold = 0.6, find.auc = TRUE, which.model = (1:(ncol(DATA) - 2)), na.rm = FALSE, xlab = "1-Specificity (false positives)", ylab = "Sensitivity (true positives)", main = "ROC Plot", model.names = NULL, color = NULL, line.type = NULL, lwd = 1, mark = 0, mark.numbers = TRUE, mark.color = NULL, opt.thresholds = NULL, opt.methods = NULL, req.sens, req.spec, obs.prev = NULL, smoothing = 1, add.legend = TRUE, legend.text = model.names, legend.cex = 0.8, add.opt.legend = TRUE, opt.legend.text = NULL, opt.legend.cex = 0.7, counter.diagonal = FALSE, pch = NULL, FPC, FNC, cost.line = FALSE)


l1=predict(rf.Is.flood,newdata=is_flood.test,type='prob')
library(ROCR)
pred=prediction(l[,2],test)
perf <- performance(pred, "tpr", "fpr")
plot(perf,main="ROC Curve",col='#F8766D') 
auc.perf <- performance(pred, measure = "auc")
print(auc.perf@y.values)
(perf@alpha.values)[335,]
axises = as.data.frame(cbind(as.data.frame(perf@x.values), as.data.frame(perf@y.values)))
head(axises)
axises$cost= (1-axises$c.0..0.0126582278481013..0.0189873417721519..0.0253164556962025..)+axises$c.0..0..0..0..0..0..0..0..0..0..0..0..0..0..0..0..0..0..0..0..
axises$cost= (1-axises$c.0..0.00632911392405063..0.0253164556962025..0.0443037974683544..)+axises$c.0..0..0..0..0..0..0..0..0..0..0..0..0..0..0..0..0..0..0..0..
which.min(axises$cost)
head(axises)
perf@alpha.values




#choose threshold
payoff_cal <- function(data,thres){
#  x <- model.matrix(Is.flood~.,is_flood.data)
  pred <- predict(rf.Is.flood,newdata=data,type='prob')
  pred <- (pred>thres)+0
  confusion <- table(pred[,2],is_flood.test$Is.flood)
#  payoff <- 1*confusion[3]/(confusion[3]+confusion[1]) + 1 * (1-(confusion[4]/(confusion[4]+confusion[3])))
  payoff <- 1*confusion[2] + 25 * confusion[3]
  return(payoff)
}


payoff_wt <- vector("numeric",99)
for (t in 1:99){
  payoff_wt[t] <- payoff_cal(is_flood.test,0.01*t)
}
thres_wt <- which.min(payoff_wt) * 0.01
thres_wt
#payoff_cal(OrangeJuice[testidex,],thres_wt)
#flood best threshold: 0.2

#Threshold
cutoff=thres_wt
l1=predict(rf.Is.flood,newdata=is_flood.test,type='prob')

l=ifelse(l1[,2]>cutoff,1,0)
table(l,test)

table(l,test)[4]/(table(l,test)[4]+table(l,test)[3]) #tpr=0.9620253
table(l,test)[2]/(table(l,test)[2]+table(l,test)[1]) #fpr=(pred1&actual0/all negative) = 0.04794859 after threhold
table(yhat.rf,test)[2]/(table(yhat.rf,test)[2]+table(yhat.rf,test)[1]) #fpr=(pred1&actual0/all negative) = 0.007909046 original



#
#
#




#Severity Threshold
payoff_cal <- function(data,thres){
  #  x <- model.matrix(Is.flood~.,is_flood.data)
  pred <- predict(rf.severity,newdata=data,type='prob')
  pred <- (pred>thres)+0
  confusion <- table(pred[,2],Severity.test$Severity)
  #  payoff <- 1*confusion[3]/(confusion[3]+confusion[1]) + 1 * (1-(confusion[4]/(confusion[4]+confusion[3])))
  payoff <- 1*confusion[2] + 50 * confusion[3]
  return(payoff)
}


payoff_wt <- vector("numeric",99)
for (t in 1:99){
  payoff_wt[t] <- payoff_cal(Severity.test,0.01*t)
}
thres_wt <- which.min(payoff_wt) * 0.01
thres_wt
#Severity best threshold: 0.15

cutoff=0.15
l1=predict(rf.severity,newdata=Severity.test,type='prob')

l=ifelse(l1[,2]>cutoff,1,0)
#confusionMatrix(l,Severity.data$Severity)
table(l,test)[4]/(table(l,test)[4]+table(l,test)[3]) #tpr=1
table(l,test)[2]/(table(l,test)[2]+table(l,test)[1]) #fpr=(pred1&actual0/all negative) = 0.02122642 after threhold
table(yhat.rf,test)[2]/(table(yhat.rf,test)[2]+table(yhat.rf,test)[1]) #fpr=(pred1&actual0/all negative) = 0.002830189 original


#
#
#


#new datapoints: Severity
library(randomForest)
library(caret)

newdata=read.csv('new_data.csv')
names(newdata)
newdata=newdata[-1][-1]

# Severity Data
names(newdata)
newSeverity.data=newdata[-31][-18][-8][-4]
names(newSeverity.data)
newSeverity.data$Month=as.factor(newSeverity.data$Month)
newSeverity.data$Severity=as.factor(newSeverity.data$Severity)
# newSeverity.data$Severity <- factor(newSeverity.data$Severity, levels = levels(Severity.data[intrain,]$Severity))
# newSeverity.data$Month <- factor(newSeverity.data$Month, levels = levels(Severity.data[intrain,]$Month))
newdataset=rbind(Severity.data,newSeverity.data)
names(newdataset)
for (f in 1:length(names(newdataset))) {
  levels(newSeverity.data[, f]) <- levels(as.factor(Severity.data[, f]))
}
rf.severity=randomForest(Severity~.,data=newdataset[intrain,], ntree =1000)
newtest=tail(newdataset,10)
str(newtest)
newtest = na.omit(newtest)
newtest$Turbidity..Maximum.=as.numeric(as.factor(newtest$Turbidity..Maximum.))
newtest
newSeverity.data
rf.severity
yhat.rf = predict(rf.severity,newtest)
#mean((yhat.rf-test)^2) #0.008924397
table(yhat.rf,newtest$Severity) #tpr=0.7
7/10
cutoff=0.15


prob.yhat.rf = predict(rf.severity,newtest,type='prob')
l=ifelse(prob.yhat.rf[,2]>cutoff,1,0)
table(l,newtest$Severity)[4]/(table(l,newtest$Severity)[4]+table(l,newtest$Severity)[3]) #tpr=0.8
yhat.rf
l
newtest$Severity


#new datapoints: Flood_type
library(randomForest)
library(caret)

newdata=read.csv('new_data.csv')
names(newdata)
newdata=newdata[-1][-1]

# Flood type Data
names(flood.newdata)
newFlood.data=newdata[-19][-18][-8][-4]
names(newFlood.data)
newFlood.data$Month=as.factor(newFlood.data$Month)
newFlood.data$Is.flood=as.factor(newFlood.data$Is.flood)
# newSeverity.data$Severity <- factor(newSeverity.data$Severity, levels = levels(Severity.data[intrain,]$Severity))
# newSeverity.data$Month <- factor(newSeverity.data$Month, levels = levels(Severity.data[intrain,]$Month))
newdataset=rbind(is_flood.data,newFlood.data)
names(newdataset)
for (f in 1:length(names(newdataset))) {
  levels(newFlood.data[, f]) <- levels(as.factor(Severity.data[, f]))
}
rf.inFlood=randomForest(Is.flood~.,data=newdataset[intrain,], ntree =1000)
newtest=tail(newdataset,11)
str(newtest)
newtest = na.omit(newtest)
newtest$Turbidity..Maximum.=as.numeric(as.factor(newtest$Turbidity..Maximum.))

yhat.rf = predict(rf.inFlood,newtest)
#mean((yhat.rf-test)^2) #0.008924397
table(yhat.rf,newtest$Is.flood)
#tpr =7/10=0.7
cutoff=0.2


prob.yhat.rf = predict(rf.inFlood,newtest,type='prob')
l=ifelse(prob.yhat.rf[,2]>cutoff,1,0)
table(l,newtest$Is.flood)[4]/(table(l,newtest$Is.flood)[4]+table(l,newtest$Is.flood)[3]) #tpr=0.8

