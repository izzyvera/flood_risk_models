

### Ridge and Lasso Regression

#setwd("pathname")


data=read.csv("05054000+05082500_new_predict.csv")
#using isflood
data1=subset(data,select=c(-1,-2,-6,-20,-10,-21))
data2=data1[,c(-27,-16)]
#using severity
data1=data1=subset(data,select=c(-1,-2,-6,-20,-10,-33))
data2=data1[,c(-16,-17)]

correlationmatrix = cor(data2)
highlycorr = findCorrelation(correlationmatrix, cutoff = 0.75)
print(highlycorr)
data2 = data2[,-highlycorr]
print(names(data2))

data2$Turb.min.Oxg.Max=data2$Turbidity..Minimum.*data2$Dissolved.oxygen..Maximum. 
data2$Turb.min.Cond.Max=data2$Turbidity..Minimum.*data2$Specific.conductance..Maximum.
data2$Turb.min.Water.pH=data2$Turbidity..Minimum.*data2$Water.pH..Minimum.
data2$Turb.min.Humd.Max=data2$Turbidity..Minimum.*data2$Humidity.Max
data2$Turb.min.Temp.min=data2$Turbidity..Minimum.*data2$Temperature.Min
data2$Turb.min.Humd.min=data2$Turbidity..Minimum.*data2$Humidity.Min
data2$Turb.MIN.Wind.max=data2$Turbidity..Minimum.*data2$Wind.Speed.Max
data2$Turb.min.Wind.min=data2$Turbidity..Minimum.*data2$Wind.Speed.Min
data2$Turb.min.Pres.min=data2$Turbidity..Minimum.*data2$Pressure.Min
data2$Turb.min.Preci.avg=data2$Turbidity..Minimum.*data2$Precipitation.Avg

data2$Oxg.max.Cond.max=data2$Dissolved.oxygen..Maximum.*data2$Specific.conductance..Maximum.
data2$Oxg.max.Water.ph=data2$Dissolved.oxygen..Maximum.*data2$Water.pH..Minimum.
data2$Oxg.max.Temp.min=data2$Dissolved.oxygen..Maximum.*data2$Temperature.Min
data2$Oxg.max.Humd.max=data2$Dissolved.oxygen..Maximum.*data2$Humidity.Max
data2$Oxg.max.Humd.min=data2$Dissolved.oxygen..Maximum.*data2$Humidity.Min
data2$Oxg.max.Wind.max=data2$Dissolved.oxygen..Maximum.*data2$Wind.Speed.Max
data2$Oxg.max.Wind.min=data2$Dissolved.oxygen..Maximum.*data2$Wind.Speed.Min
data2$Oxg.max.Pres.Min=data2$Dissolved.oxygen..Maximum.*data2$Pressure.Min
data2$Oxg.max.Preci.avg=data2$Dissolved.oxygen..Maximum.*data2$Precipitation.Avg

data2$cond.max.water.ph=data2$Specific.conductance..Maximum.*data2$Water.pH..Minimum.
data2$cond.max.temp.min=data2$Specific.conductance..Maximum.*data2$Temperature.Min
data2$cond.max.humd.max=data2$Specific.conductance..Maximum.*data2$Humidity.Max
data2$cond.max.humd.min=data2$Specific.conductance..Maximum.*data2$Humidity.Min
data2$cond.max.pres.min=data2$Specific.conductance..Maximum.*data2$Pressure.Min
data2$cond.max.prec.avg=data2$Specific.conductance..Maximum.*data2$Precipitation.Avg


data2$water.ph.temp=data2$Water.pH..Minimum.*data2$Temperature.Min
data2$water.ph.humd.max=data2$Water.pH..Minimum.*data2$Humidity.Max
data2$water.ph.humd.min=data2$Water.pH..Minimum.*data2$Humidity.Min
data2$water.ph.wind.max=data2$Water.pH..Minimum.*data2$Wind.Speed.Max

data2$water.oh.wind.min=data2$Water.pH..Minimum.*data2$Wind.Speed.Min
data2$water.ph.pres.min=data2$Water.pH..Minimum.*data2$Pressure.Min
data2$water.ph.preci.avg=data2$Water.pH..Minimum.*data2$Precipitation.Avg

data2$temp.min.humd.max=data2$Temperature.Min*data2$Humidity.Max
data2$temp.min.humd.min=data2$Temperature.Min*data2$Humidity.Min
data2$temp.min.wind.min=data2$Temperature.Min*data2$Wind.Speed.Min
data2$temp.min.wind.max=data2$Temperature.Min*data2$Wind.Speed.Max
data2$temp.min.pres.min=data2$Temperature.Min*data2$Pressure.Min
data2$temp.min.preci.avg=data2$Temperature.Min*data2$Precipitation.Avg

data2$humd.max.humd.min=data2$Humidity.Max*data2$Humidity.Min
data2$humd.max.wind.max=data2$Humidity.Max*data2$Wind.Speed.Max
data2$humd.max.wind.min=data2$Humidity.Max*data2$Wind.Speed.Min
data2$humd.max.pres.min=data2$Humidity.Max*data2$Pressure.Min
data2$humd.max.preci.avg=data2$Humidity.Max*data2$Precipitation.Avg

data2$humd.min.wind.max=data2$Humidity.Min*data$Wind.Speed.Max
data2$humd.min.wind.min=data2$Humidity.Min*data2$Wind.Speed.Min
data2$humd.min.pres.min=data2$Humidity.Min*data2$Pressure.Min
data2$humd.min.preci.avg=data2$Humidity.Min*data2$Precipitation.Avg

data2$wind.max.wind.min=data2$Wind.Speed.Max*data2$Wind.Speed.Min
data2$wind.max.pres.min=data2$Wind.Speed.Max*data2$Pressure.Min
data2$wind.max.preci.avg=data2$Wind.Speed.Max*data2$Precipitation.Avg

data2$wind.min.pres.min=data2$Wind.Speed.Min*data2$Pressure.Min
data2$wind.min.preci.avg=data2$Wind.Speed.Min*data2$Precipitation.Avg

data2$pres.min.preci.avg=data2$Pressure.Min*data2$Precipitation.Avg

data2$Turb.square=data2$Turbidity..Minimum.^2
data2$oxg.square=data2$Dissolved.oxygen..Maximum.^2
data2$cond.square=data2$Specific.conductance..Maximum.^2
data2$waterph.square=data2$Water.pH..Minimum.^2
data2$humd.max.square=data2$Humidity.Max^2
data2$humd.min.square=data2$Humidity.Min^2
data2$wind.min.square=data2$Wind.Speed.Min^2
data$wind.max.square=data2$Wind.Speed.Max^2
data2$pres.min.square=data2$Pressure.Min^2
data2$prec.square=data2$Precipitation.Avg^2
#using is.flood
data2.scaled=data.frame("Month"=factor(data1$Month), "Flood"=factor(data1$Is.flood), scale(data2))
#using severity
data2.scaled=data.frame("Month"=factor(data1$Month), "Severity"=factor(data1$Severity), scale(data2))
print(names(data2.scaled))
library(glmnet)
library(leaps)
library(caret)

set.seed(3033)
train = sample(1:nrow(data2), 0.75*nrow(data2)) 
test = -train
#using is.flood
x = model.matrix(Flood ~., data2.scaled)[,-2]
y = data2.scaled$Flood
grid=10^(-10:5)
#using severity

x = model.matrix(Severity ~., data2.scaled)[,-2]
y = data2.scaled$Severity
grid=10^(-7:10)


#lasso
cv.out = cv.glmnet(x[train,],y[train], alpha=1, lambda=grid, nfolds=10,family='binomial',
                   type.measure = "auc") 
best.lambda=cv.out$lambda.min
lasso.mod=glmnet(x[train,], y[train], alpha=1, lambda=best.lambda, family="binomial")
pred_lasso = predict(lasso.mod, x[test,],type = "class")
sum(pred_lasso==1 & y[test]==1)/sum(y[test]==1)
#ridge
cv.out = cv.glmnet(x[train,],y[train], alpha=0, lambda=grid, nfolds=10,family='binomial',
                   type.measure = "auc")
best.lambda=cv.out$lambda.min
lasso.mod=glmnet(x[train,], y[train], alpha=1, lambda=best.lambda, family="binomial")
pred_lasso = predict(lasso.mod, x[test,],type = "class")
sum(pred_lasso==1 & y[test]==1)/sum(y[test]==1)
# cv based on tpr
nfolds=10
folds=sample(1:nfolds,length(train),replace=TRUE)
cv.tpr = vector("numeric",length(grid))
for (i in 1:length(grid)){
  lambda = grid[i]
  tpr=0
  count=0
  for (j in 1:nfolds){
    lasso.mod = glmnet(x[train,][folds!=j,], y[train][folds!=j], alpha = 1, lambda = lambda, family = 'binomial')
    pred_lasso = predict(lasso.mod, x[train,][folds==j,],type = "class")
    if (sum(y[train][folds==j]==1)!=0){
      tpr=tpr+sum(pred_lasso==1 & y[train][folds==j]==1)/sum(y[train][folds==j]==1)
      count=count+1
    }
  }
  cv.tpr[i]=tpr/count
}
cv.tpr
best.lambda=grid[which.max(cv.tpr)]
lasso.mod=glmnet(x[train,], y[train], alpha=1, lambda=best.lambda, family="binomial")
pred_lasso = predict(lasso.mod, x[test,],type = "class")
sum(pred_lasso==1 & y[test]==1)/sum(y[test]==1)



#pls
data=read.csv("05054000+05082500_new_predict.csv")
#using isflood
data1=subset(data,select=c(-1,-2,-6,-20,-21))
data2=data1[,c(-28,-17)]
correlationmatrix = cor(data2)
highlycorr = findCorrelation(correlationmatrix, cutoff = 0.75)
print(highlycorr)
data2 = data2[,-highlycorr]
print(names(data2))

data2$Turb.min.Oxg.Max=data2$Turbidity..Minimum.*data2$Dissolved.oxygen..Maximum. 
data2$Turb.min.Cond.Max=data2$Turbidity..Minimum.*data2$Specific.conductance..Maximum.
data2$Turb.min.Water.pH=data2$Turbidity..Minimum.*data2$Water.pH..Minimum.
data2$Turb.min.Humd.Max=data2$Turbidity..Minimum.*data2$Humidity.Max
data2$Turb.min.Temp.min=data2$Turbidity..Minimum.*data2$Temperature.Min
data2$Turb.min.Humd.min=data2$Turbidity..Minimum.*data2$Humidity.Min
data2$Turb.MIN.Wind.max=data2$Turbidity..Minimum.*data2$Wind.Speed.Max
data2$Turb.min.Wind.min=data2$Turbidity..Minimum.*data2$Wind.Speed.Min
data2$Turb.min.Pres.min=data2$Turbidity..Minimum.*data2$Pressure.Min
data2$Turb.min.Preci.avg=data2$Turbidity..Minimum.*data2$Precipitation.Avg

data2$Oxg.max.Cond.max=data2$Dissolved.oxygen..Maximum.*data2$Specific.conductance..Maximum.
data2$Oxg.max.Water.ph=data2$Dissolved.oxygen..Maximum.*data2$Water.pH..Minimum.
data2$Oxg.max.Temp.min=data2$Dissolved.oxygen..Maximum.*data2$Temperature.Min
data2$Oxg.max.Humd.max=data2$Dissolved.oxygen..Maximum.*data2$Humidity.Max
data2$Oxg.max.Humd.min=data2$Dissolved.oxygen..Maximum.*data2$Humidity.Min
data2$Oxg.max.Wind.max=data2$Dissolved.oxygen..Maximum.*data2$Wind.Speed.Max
data2$Oxg.max.Wind.min=data2$Dissolved.oxygen..Maximum.*data2$Wind.Speed.Min
data2$Oxg.max.Pres.Min=data2$Dissolved.oxygen..Maximum.*data2$Pressure.Min
data2$Oxg.max.Preci.avg=data2$Dissolved.oxygen..Maximum.*data2$Precipitation.Avg

data2$cond.max.water.ph=data2$Specific.conductance..Maximum.*data2$Water.pH..Minimum.
data2$cond.max.temp.min=data2$Specific.conductance..Maximum.*data2$Temperature.Min
data2$cond.max.humd.max=data2$Specific.conductance..Maximum.*data2$Humidity.Max
data2$cond.max.humd.min=data2$Specific.conductance..Maximum.*data2$Humidity.Min
data2$cond.max.pres.min=data2$Specific.conductance..Maximum.*data2$Pressure.Min
data2$cond.max.prec.avg=data2$Specific.conductance..Maximum.*data2$Precipitation.Avg


data2$water.ph.temp=data2$Water.pH..Minimum.*data2$Temperature.Min
data2$water.ph.humd.max=data2$Water.pH..Minimum.*data2$Humidity.Max
data2$water.ph.humd.min=data2$Water.pH..Minimum.*data2$Humidity.Min
data2$water.ph.wind.max=data2$Water.pH..Minimum.*data2$Wind.Speed.Max

data2$water.oh.wind.min=data2$Water.pH..Minimum.*data2$Wind.Speed.Min
data2$water.ph.pres.min=data2$Water.pH..Minimum.*data2$Pressure.Min
data2$water.ph.preci.avg=data2$Water.pH..Minimum.*data2$Precipitation.Avg

data2$temp.min.humd.max=data2$Temperature.Min*data2$Humidity.Max
data2$temp.min.humd.min=data2$Temperature.Min*data2$Humidity.Min
data2$temp.min.wind.min=data2$Temperature.Min*data2$Wind.Speed.Min
data2$temp.min.wind.max=data2$Temperature.Min*data2$Wind.Speed.Max
data2$temp.min.pres.min=data2$Temperature.Min*data2$Pressure.Min
data2$temp.min.preci.avg=data2$Temperature.Min*data2$Precipitation.Avg

data2$humd.max.humd.min=data2$Humidity.Max*data2$Humidity.Min
data2$humd.max.wind.max=data2$Humidity.Max*data2$Wind.Speed.Max
data2$humd.max.wind.min=data2$Humidity.Max*data2$Wind.Speed.Min
data2$humd.max.pres.min=data2$Humidity.Max*data2$Pressure.Min
data2$humd.max.preci.avg=data2$Humidity.Max*data2$Precipitation.Avg

data2$humd.min.wind.max=data2$Humidity.Min*data$Wind.Speed.Max
data2$humd.min.wind.min=data2$Humidity.Min*data2$Wind.Speed.Min
data2$humd.min.pres.min=data2$Humidity.Min*data2$Pressure.Min
data2$humd.min.preci.avg=data2$Humidity.Min*data2$Precipitation.Avg

data2$wind.max.wind.min=data2$Wind.Speed.Max*data2$Wind.Speed.Min
data2$wind.max.pres.min=data2$Wind.Speed.Max*data2$Pressure.Min
data2$wind.max.preci.avg=data2$Wind.Speed.Max*data2$Precipitation.Avg

data2$wind.min.pres.min=data2$Wind.Speed.Min*data2$Pressure.Min
data2$wind.min.preci.avg=data2$Wind.Speed.Min*data2$Precipitation.Avg

data2$pres.min.preci.avg=data2$Pressure.Min*data2$Precipitation.Avg

data2$Turb.square=data2$Turbidity..Minimum.^2
data2$oxg.square=data2$Dissolved.oxygen..Maximum.^2
data2$cond.square=data2$Specific.conductance..Maximum.^2
data2$waterph.square=data2$Water.pH..Minimum.^2
data2$humd.max.square=data2$Humidity.Max^2
data2$humd.min.square=data2$Humidity.Min^2
data2$wind.min.square=data2$Wind.Speed.Min^2
data$wind.max.square=data2$Wind.Speed.Max^2
data2$pres.min.square=data2$Pressure.Min^2
data2$prec.square=data2$Precipitation.Avg^2
#using is.flood
data2=data.frame("Month"=factor(data1$Month), data2)
print(names(data2))
library(pls)
library(glmnet)
library(leaps)
library(caret)

set.seed(3033)
train = sample(1:nrow(data2), 0.75*nrow(data2)) 
test = -train
#using is.flood
y = data1$Is.flood

index=1:nrow(data1)
pls.fit = plsr(Gage.height..Mean.~., data= data2, subset=train, scale=T, 
               validation = "CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
pls.pred = predict(pls.fit,data2[test,],ncomp=25)
pred.severity = rep(NA, length(index[test]))
for (i in 1:length(index[test])) {
  ind = index[test][i]
  if (ind <= 4364) {
    if (pls.pred[i] < 18) pred.severity[i] = 0
    else pred.severity[i] = 1
  }
  else {
    if(pls.pred[i] < 28) pred.severity[i] = 0
    else pred.severity[i] = 1
  }
}
tpr=sum(pred.severity==1 & y[test]==1)/sum(y[test]==1)

