library(caret)
library(tree)
#Decision Tree
#Severity.data$predict <- ifelse(Severity.data$data>=2, 2, 1)

# Severity
#05054000+05082500
data=read.csv('05054000+05082500_new_predict.csv')
#names(data)
data=data[-1][-1]

# Severity Data
names(data)
Severity.data=data[-31][-18][-8][-4]
names(Severity.data)
Severity.data$Month=as.factor(Severity.data$Month)
Severity.data$Severity=as.factor(Severity.data$Severity)

factor.data=Severity.data[17]
factor.data=cbind(factor.data,Severity.data[16])
corr.data=Severity.data[-17][-16]
correlationmatrix = cor(corr.data)
highlycorr = findCorrelation(correlationmatrix, cutoff = 0.75)
print(highlycorr)
corr.data = corr.data[,-highlycorr]
names(corr.data)
Severity.data=cbind(corr.data,factor.data)

set.seed(3033)
intrain <- createDataPartition(y = data$Severity, p= 0.75, list = FALSE)
test = -intrain
Severity.test=Severity.data[-intrain ,]
test=Severity.test[,'Severity']

tree.Severity =tree(Severity~. ,data=Severity.data[intrain,] )
summary(tree.Severity) #17
#tree.pred=predict (tree.Severity ,Severity.test, type='class')

#table(tree.pred,test)

cv.Severity = cv.tree(tree.Severity, FUN=prune.misclass)
summary(tree.Severity)


plot(cv.Severity$size,cv.Severity$dev,type="b")
plot(cv.Severity$k,cv.Severity$dev,type="b")

which.min(cv.Severity$dev)
cv.Severity$size #based on plot: the best is 13-16
cv.Severity
cv.Severity$size[which.min(cv.Severity$dev)] #11
prune.Severity = prune.misclass(tree.Severity,best =11)
plot(prune.Severity)
text(prune.Severity,pretty=0)

tree.pred=predict (prune.Severity ,Severity.test, type='class')

table(tree.pred,test)
#accuracy=
#(2094+47)/(2094+47+14+26) #0.9816598
table(tree.pred,test)[4]/(table(tree.pred,test)[3]+table(tree.pred,test)[4])
#47/(47+14)
#tpr=0.7704918





#is_flood
data=read.csv('05054000+05082500_new_predict.csv')
names(data)
data=data[-1][-1]

#is_flood
names(data)
is_flood.data=data[-19][-18][-8][-4]
names(is_flood.data)
is_flood.data$Month=as.factor(is_flood.data$Month)
is_flood.data$Is.flood=as.factor(is_flood.data$Is.flood)


factor.data=is_flood.data[27]
factor.data=cbind(factor.data,is_flood.data[16])
names(factor.data)
corr.data=is_flood.data[-27][-16]
correlationmatrix = cor(corr.data)
highlycorr = findCorrelation(correlationmatrix, cutoff = 0.75)
print(highlycorr)
corr.data = corr.data[,-highlycorr]
names(corr.data)
is_flood.data=cbind(corr.data,factor.data)


set.seed(3033)
intrain <- createDataPartition(y = data$Severity, p= 0.75, list = FALSE)
test = -intrain
is_flood.test=is_flood.data[-intrain ,]
test=is_flood.test[,'Is.flood']

tree.Is.flood =tree(Is.flood~. ,data=is_flood.data[intrain,] )
summary(tree.Is.flood) #15

#tree.pred=predict (tree.Severity ,Severity.test, type='class')

#table(tree.pred,test)

cv.Is.flood = cv.tree(tree.Is.flood, FUN=prune.misclass) #15


plot(cv.Is.flood$size,cv.Is.flood$dev,type="b")
plot(cv.Is.flood$k,cv.Is.flood$dev,type="b")


which.min(cv.Is.flood$dev)
cv.Is.flood$size #based on plot: the best is 14-16
cv.Is.flood$size[which.min(cv.Is.flood$dev)]
cv.Is.flood #11
prune.Is.flood = prune.misclass(tree.Is.flood,best =15)
plot(prune.Is.flood)
text(prune.Is.flood,pretty=0)

tree.pred=predict (prune.Is.flood ,is_flood.test, type='class')

table(tree.pred,test)
table(tree.pred,test)[4]/(table(tree.pred,test)[4]+table(tree.pred,test)[3])
#tpr=tp/(tp+fn) =0.5443038

