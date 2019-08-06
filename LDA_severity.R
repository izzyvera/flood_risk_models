# LDA
uniondata = read.csv("05054000+05082500_new_predict.csv")
uniondata = uniondata[-1][-1]
na.omit(uniondata)

library(caret)
library(MASS)
library(glmnet)

# split the data
correlationmatrix = cor(uniondata)
highlycorr = findCorrelation(correlationmatrix, cutoff = 0.75)
print(highlycorr)
data = uniondata[,-highlycorr]
data = data[-2]
str(data)
data = as.data.frame(scale(data))
data$Month = as.factor(uniondata$Month)
data$Severity = as.factor(uniondata$Severity)
set.seed(3033)
intrain <- createDataPartition(y = data$Severity, p= 0.75, list = FALSE)
training <- data[intrain,]
testing <- data[-intrain,]

# ridge regression
# x=model.matrix(Severity~.,data)
# y=data$Severity
# grid=10^(-2:10)
# fit1 = cv.glmnet(x[train,],y[train],type.measure="mse",alpha=0,lambda=grid)
# plot(fit1, xvar='lambda')
# bestlam=fit1$lambda.min
# ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=bestlam)
# ridge.pred=predict(ridge.mod,newx=x[test,])


# PCA to reduce dimension
# pr.out=prcomp(data,scale=T)
# pr.out$rotation
# reduced.data = pr.out$x
# head(reduced.data)
# biplot(pr.out, scale=0)
# names = names(data)
# colnames(reduced.data) <- names
# reduced.data = data.frame(reduced.data)
# attach(reduced.data)

# lda
invalid <- createDataPartition(y = training$Severity, p= 0.75, list = FALSE)
train <- training[invalid,]
valid <- data[-invalid,]
lda.fit = lda(train$Severity~.,train)
# lda.fit
lda.pred=predict(lda.fit, valid)
names(lda.pred)
lda.class = lda.pred$class
y = valid$Severity
t = table(lda.class, y)
tpr = t[4] / (t[3] + t[4]) # 0.6231884
mean(lda.class== y) # accuracy 0.956761
lda2.fit = lda(training$Severity~.,training)
# lda2.fit
lda2.pred = predict(lda2.fit, testing)
names(lda2.pred)
lda2.class= lda2.pred$class
t2 = table(lda2.class, testing$Severity)
tpr2 = t2[4] / (t2[3] + t2[4]) # 0.5909091

# cross validation and LDA
library(PredPsych)
folds = 3:10
preds = rep(0,10)

ldadata = uniondata[,-highlycorr]
ldadata = ldadata[-2]
data = as.data.frame(scale(ldadata))
data$Severity = as.factor(ldadata$Severity)
data$Month = ldadata$Month
set.seed(3033)
intrain <- createDataPartition(y = data$Severity, p= 0.75, list = FALSE)
training <- data[intrain,]
testing <- data[-intrain,]

for (i in folds){
  LDAmodel <- LinearDA(training, classCol = 5, nTrainFolds = i-1, ntrainTestFolds = i, modelTrainFolds = 1:(i-1), CV = TRUE, extendedResults = TRUE, silent = TRUE)
  preds[i] = LDAmodel$ConfusionMatrixResults$byClass["Specificity"]
}
which.max(preds)
preds
LDAmodel <- LinearDA(training, 5, nTrainFolds = which.max(preds)-1, ntrainTestFolds = which.max(preds), modelTrainFolds = 1:(which.max(preds)-1), CV = TRUE, extendedResults = TRUE)
LDAmodel$ConfusionMatrix # tpr = 0.5934
LDAmodel <- LinearDA(testing, 5, nTrainFolds = which.max(preds)-1, ntrainTestFolds = which.max(preds), modelTrainFolds = 1:(which.max(preds)-1), CV = TRUE, extendedResults = TRUE)
LDAmodel$ConfusionMatrixResults # tpr = 0.5934
LDAmodel$ConfMatrix

preds2 = rep(0,10)
for (i in folds){
  LDAmodel2 <- LinearDA(data, classCol = 5, nTrainFolds = i-1, ntrainTestFolds = i, modelTrainFolds = 1:(i-1), CV = FALSE, extendedResults = TRUE, silent = TRUE)
  preds2[i] = LDAmodel2$ConfusionMatrixResults$byClass["Specificity"]
}
which.max(preds2)
preds2
LDAmodel2 <- LinearDA(training, 5, nTrainFolds = which.max(preds2)-1, ntrainTestFolds = which.max(preds2), modelTrainFolds = 1:(which.max(preds2)-1), CV = FALSE, extendedResults = TRUE)
LDAmodel2$ConfMatrix # 0.3182 

# nonlinear LDA
uniondata = read.csv("05054000+05082500_new_predict.csv")
uniondata = uniondata[-1][-1]
data2 = uniondata
data2$Turb.min.Oxg.Max=data2$Turbidity..Minimum.*data2$Dissolved.oxygen..Maximum. 
data2$Turb.min.Cond.Max=data2$Turbidity..Minimum.*data2$Specific.conductance..Mean.
data2$Turb.min.Water.pH=data2$Turbidity..Minimum.*data2$Water.pH..Minimum.
data2$Turb.min.Humd.Max=data2$Turbidity..Minimum.*data2$Humidity.Max
data2$Turb.min.Temp.min=data2$Turbidity..Minimum.*data2$Temperature.Min
data2$Turb.min.Humd.min=data2$Turbidity..Minimum.*data2$Humidity.Min
data2$Turb.MIN.Wind.max=data2$Turbidity..Minimum.*data2$Wind.Speed.Max
data2$Turb.min.Wind.min=data2$Turbidity..Minimum.*data2$Wind.Speed.Min
data2$Turb.min.Pres.min=data2$Turbidity..Minimum.*data2$Pressure.Max
data2$Turb.min.Preci.avg=data2$Turbidity..Minimum.*data2$Precipitation.Avg

data2$Oxg.max.Cond.max=data2$Dissolved.oxygen..Maximum.*data2$Specific.conductance..Mean.
data2$Oxg.max.Water.ph=data2$Dissolved.oxygen..Maximum.*data2$Water.pH..Minimum.
data2$Oxg.max.Temp.min=data2$Dissolved.oxygen..Maximum.*data2$Temperature.Min
data2$Oxg.max.Humd.max=data2$Dissolved.oxygen..Maximum.*data2$Humidity.Max
data2$Oxg.max.Humd.min=data2$Dissolved.oxygen..Maximum.*data2$Humidity.Min
data2$Oxg.max.Wind.max=data2$Dissolved.oxygen..Maximum.*data2$Wind.Speed.Max
data2$Oxg.max.Wind.min=data2$Dissolved.oxygen..Maximum.*data2$Wind.Speed.Min
data2$Oxg.max.Pres.Min=data2$Dissolved.oxygen..Maximum.*data2$Pressure.Max
data2$Oxg.max.Preci.avg=data2$Dissolved.oxygen..Maximum.*data2$Precipitation.Avg

data2$cond.max.water.ph=data2$Specific.conductance..Mean.*data2$Water.pH..Minimum.
data2$cond.max.temp.min=data2$Specific.conductance..Mean.*data2$Temperature.Min
data2$cond.max.humd.max=data2$Specific.conductance..Mean.*data2$Humidity.Max
data2$cond.max.humd.min=data2$Specific.conductance..Mean.*data2$Humidity.Min
data2$cond.max.pres.min=data2$Specific.conductance..Mean.*data2$Pressure.Max
data2$cond.max.prec.avg=data2$Specific.conductance..Mean.*data2$Precipitation.Avg


data2$water.ph.temp=data2$Water.pH..Minimum.*data2$Temperature.Min
data2$water.ph.humd.max=data2$Water.pH..Minimum.*data2$Humidity.Max
data2$water.ph.humd.min=data2$Water.pH..Minimum.*data2$Humidity.Min
data2$water.ph.wind.max=data2$Water.pH..Minimum.*data2$Wind.Speed.Max
data2$water.oh.wind.min=data2$Water.pH..Minimum.*data2$Wind.Speed.Min
data2$water.ph.pres.min=data2$Water.pH..Minimum.*data2$Pressure.Max
data2$water.ph.preci.avg=data2$Water.pH..Minimum.*data2$Precipitation.Avg

data2$temp.min.humd.max=data2$Temperature.Min*data2$Humidity.Max
data2$temp.min.humd.min=data2$Temperature.Min*data2$Humidity.Min
data2$temp.min.wind.min=data2$Temperature.Min*data2$Wind.Speed.Min
data2$temp.min.wind.max=data2$Temperature.Min*data2$Wind.Speed.Max
data2$temp.min.pres.min=data2$Temperature.Min*data2$Pressure.Max
data2$temp.min.preci.avg=data2$Temperature.Min*data2$Precipitation.Avg

data2$humd.max.humd.min=data2$Humidity.Max*data2$Humidity.Min
data2$humd.max.wind.max=data2$Humidity.Max*data2$Wind.Speed.Max
data2$humd.max.wind.min=data2$Humidity.Max*data2$Wind.Speed.Min
data2$humd.max.pres.min=data2$Humidity.Max*data2$Pressure.Max
data2$humd.max.preci.avg=data2$Humidity.Max*data2$Precipitation.Avg

data2$humd.min.wind.max=data2$Humidity.Min*data$Wind.Speed.Max
data2$humd.min.wind.min=data2$Humidity.Min*data2$Wind.Speed.Min
data2$humd.min.pres.min=data2$Humidity.Min*data2$Pressure.Max
data2$humd.min.preci.avg=data2$Humidity.Min*data2$Precipitation.Avg

data2$wind.max.wind.min=data2$Wind.Speed.Max*data2$Wind.Speed.Min
data2$wind.max.pres.min=data2$Wind.Speed.Max*data2$Pressure.Max
data2$wind.max.preci.avg=data2$Wind.Speed.Max*data2$Precipitation.Avg


data2$wind.min.pres.min=data2$Wind.Speed.Min*data2$Pressure.Max
data2$wind.min.preci.avg=data2$Wind.Speed.Min*data2$Precipitation.Avg

data2$pres.min.preci.avg=data2$Pressure.Max*data2$Precipitation.Avg

data2$Turb.square=data2$Turbidity..Minimum.^2
data2$oxg.square=data2$Dissolved.oxygen..Maximum.^2
data2$cond.square=data2$Specific.conductance..Mean.^2
data2$waterph.square=data2$Water.pH..Minimum.^2

data2$humd.max.square=data2$Humidity.Max^2
data2$humd.min.square=data2$Humidity.Min^2
data2$wind.min.square=data2$Wind.Speed.Min^2
data$wind.max.square=data2$Wind.Speed.Max^2
data2$pres.min.square=data2$Pressure.Max^2
data2$prec.square=data2$Precipitation.Avg^2

correlationmatrix = cor(data2)
highlycorr = findCorrelation(correlationmatrix, cutoff = 0.75)
print(highlycorr)
names(data2[,highlycorr])
data2 = data2[,-highlycorr]
data2 = data2[-3][-1]
str(data2)
data2 = as.data.frame(scale(data2))
data2$Month = as.factor(data2$Month)
data2$Flood.type = as.factor(uniondata$Severity)
set.seed(3033)
intrain <- createDataPartition(y = data2$Severity, p= 0.75, list = FALSE)
training <- data2[intrain,]
testing <- data2[-intrain,]

lda.fit = lda(training$Severity~.,training)
lda.pred = predict(lda.fit, testing)
names(lda.pred)
lda.class= lda.pred$class
t = table(lda.class, testing$Severity)
tpr = t[4] / (t[3] + t[4]) # 0.6212121

