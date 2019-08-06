#setwd("pathname")

#knn
data=read.csv("05054000+05082500_new_predict.csv")
#using isflood
data1=subset(data,select=c(-1,-2,-6,-20,-10,-21))
data2=data1[,-27]
#using severity
data1=data1=subset(data,select=c(-1,-2,-6,-20,-10,-33))
data2=data1[,-16]

correlationmatrix = cor(data2)
highlycorr = findCorrelation(correlationmatrix, cutoff = 0.75)
print(highlycorr)
data2 = data2[,-highlycorr]
print(names(data2))

#using is.flood
data2.scaled=data.frame("Month"=factor(data1$Month), "Flood"=factor(data1$Is.flood), scale(data2))
#using severity
data2.scaled=data.frame("Month"=factor(data1$Month), "Severity"=factor(data1$Severity), scale(data2))

set.seed(3033)
train=sample(1:nrow(data2.scaled),0.75*nrow(data2.scaled))
test=-train
#using flood
xtrain_KNN=data2.scaled[train,] [-2]
xtest_KNN=data2.scaled[test,][-2]
ytrain_KNN=data2.scaled[train,]$Flood
ytest_KNN=data2.scaled[test,]$Flood
#using severity
xtrain_KNN=data2.scaled[train,] [-2]
xtest_KNN=data2.scaled[test,][-2]
ytrain_KNN=data2.scaled[train,]$Severity
ytest_KNN=data2.scaled[test,]$Severity

library(class)
nfolds=10
folds=sample(1:nfolds,length(train),replace=TRUE)
cv.tpr = vector("numeric",27)

for (nn in 1:27){
  tpr=0
  count=0
  for (j in 1:nfolds){
    xTrain=xtrain_KNN[folds!=j,]
    xTest=xtrain_KNN[folds==j,]
    yTrain=ytrain_KNN[folds!=j]
    yTest=ytrain_KNN[folds==j]
    KNNpred = knn(xTrain,xTest,yTrain, k = nn)
    if (sum(yTest==1)!=0){
      tpr=tpr+sum(KNNpred==1 & yTest==1)/sum(yTest==1)
      count=count+1
    }
  }
  cv.tpr[nn]=tpr/count
}
cv.tpr
best.k = which.max(cv.tpr)

KNN.test.pred=knn(xtrain_KNN,xtest_KNN,ytrain_KNN,k=best.k)
KNN.test.tpr=sum(KNN.test.pred==1 & ytest_KNN==1)/sum(ytest_KNN==1)
KNN.test.tpr


