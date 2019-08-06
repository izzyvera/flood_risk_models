# LDA
uniondata = read.csv("05054000+05082500.csv")
uniondata = uniondata[-1]
uniondata = uniondata[-9]
na.omit(uniondata)
cor(uniondata) > 0.8
data = uniondata[-18][-16][-15][-13][-12][-10][-9][-7][-6][-4][-2][-3]

library(MASS)
library(glmnet)
#attach(uniondata)

# split the data
train = sample(1:nrow(data),0.5*nrow(data))
rest = data[-train,]
valid = sample(1:nrow(rest), 0.5*nrow(rest))
test = -valid
tv = c(train, valid)
data = data[-1]

# ridge regression
x=model.matrix(Severity~.,data)
y=data$Severity
grid=10^(-2:10)
fit1 = cv.glmnet(x[train,],y[train],type.measure="mse",alpha=0,lambda=grid)
plot(fit1, xvar='lambda')
bestlam=fit1$lambda.min
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=bestlam)
ridge.pred=predict(ridge.mod,newx=x[test,])


# PCA to reduce dimension
pr.out=prcomp(data,scale=T)
pr.out$rotation
reduced.data = pr.out$x
head(reduced.data)
biplot(pr.out, scale=0)
names = names(data)
colnames(reduced.data) <- names
reduced.data = data.frame(reduced.data)
attach(reduced.data)

# lda
lda.fit = lda(data$Severity~.,data, subset = tv)
lda.fit
lda.pred=predict(lda.fit, data[-6][test,])
names(lda.pred)
lda.class = lda.pred$class
y = data$Severity
table(lda.class, y[test])
mean(lda.class== y[test]) # accuracy = 0.961515

lda.pred=predict(lda.fit, data[-6][test,])
lda.class = lda.pred$class
error.lda = mean(lda.class!= y[test]) 
error.lda # 0.03848503

# SVM
#install.packages("kernlab")
#install.packages("e1071")
#install.packages("RColorBrewer")
#install.packages("pROC")
