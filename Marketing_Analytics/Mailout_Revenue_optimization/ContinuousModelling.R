

##### PREDICTION MODELING ######

######################### Least squares regression

model.ls1 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + genf + wrat*hinc +
                  incm*inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                data.train.std.y)

pred.valid.ls1 <- predict(model.ls1, newdata = data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.ls1)^2) # mean prediction error
sd((y.valid - pred.valid.ls1)^2)/sqrt(n.valid.y) # std error

#############subsets
# All Subsets Regression


leaps.subset <-regsubsets(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + genf + wrat*hinc +
                            incm*inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif
                          ,data=data.train.std.y,nvmax=16)

test.mat=model.matrix(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + genf + wrat*hinc +
                        incm*inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,
                      data=data.valid.std.y )

val.errors=rep(NA,16)
for(i in 1:16){
  coefi=coef(leaps.subset,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((y.valid-pred)^2)
}
val.errors[which.min(val.errors)]

plot(1:17,val.errors[1:17],xlab="#predictors",ylab="MSE", main="Performance of Subset, varying #predictors chosen")
lines(1:17,val.errors[1:17])

#############forward selection
leaps.fwd<-regsubsets(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + genf + wrat*hinc +
                        incm*inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,data=data.train.std.y,method="forward", nvmax=16)

test.mat=model.matrix(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + genf + wrat*hinc +
                        incm*inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,data=data.valid.std.y )
val.errors=rep(NA,16)
for(i in 1:16){
  coefi=coef(leaps.fwd,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((y.valid-pred)^2)
}
val.errors[which.min(val.errors)]

plot(1:17,val.errors[1:17],xlab="#predictors",ylab="MSE", main="Performance of Fwd Selection v #predictors chosen")
lines(1:17,val.errors[1:17])

#############backwards selection
leaps.bkwd<-regsubsets(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + genf + wrat*hinc +
                         incm*inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,data=data.train.std.y,method="backward", nvmax=16)

test.mat=model.matrix(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + genf + wrat*hinc +
                        incm*inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,data=data.valid.std.y )
val.errors=rep(NA,16)
for(i in 1:16){
  coefi=coef(leaps.bkwd,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((y.valid-pred)^2)
}
val.errors[which.min(val.errors)]

plot(1:17,val.errors[1:17],xlab="#predictors",ylab="MSE", main="Performance of Backward Selection v #predictors chosen")
lines(1:17,val.errors[1:17])

# Boosting


set.seed(1)
boost=gbm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + genf + wrat*hinc +
            incm*inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,data=data.train.std.y,distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost)
yhat.boost=predict(boost,newdata=data.valid.std.y,n.trees=5000)
mean((yhat.boost-y.valid)^2)

#boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
#yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
#mean((yhat.boost-boston.test)^2)

#############  Bagging


set.seed(1)
bag=randomForest(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + genf + wrat*hinc +
                   incm*inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,data=data.train.std.y,mtry=20,importance=TRUE)
bag
yhat.bag = predict(bag,newdata=data.valid.std.y)
plot(yhat.bag, data.valid.std.y$damt)
abline(0,1)
mean((yhat.bag-y.valid)^2)



############# RF (5/10/15)

seq=c(1:19)
ResultsVector <- matrix(nrow=length(seq),ncol=2)
count=1

for(value in seq){
  

set.seed(1)
bag=randomForest(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + genf + wrat*hinc +
                   incm*inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,data=data.train.std.y,mtry=value,importance=TRUE)
#bag
yhat.bag = predict(bag,newdata=data.valid.std.y)
#plot(yhat.bag, data.valid.std.y$damt)
#abline(0,1)
mean((yhat.bag-y.valid)^2)

ResultsVector[count,1]  = value
ResultsVector[count,2]  = mean((yhat.bag-y.valid)^2)
count=count+1
}
plot(ResultsVector[,1],ResultsVector[,2],xlab="#predictors",ylab="MSE", main="Performance of Random Forest, varying #predictors chosen")
lines(ResultsVector[,1],ResultsVector[,2])
ResultsVector[which.min(ResultsVector[,2]),]  #the best model


text(ResultsVector[which.min(ResultsVector[,2]),1], ResultsVector[which.min(ResultsVector[,2]),2], 
     labels=paste("Min MSE=",round(ResultsVector[which.min(ResultsVector[,2]),2],5), " X=",ResultsVector[which.min(ResultsVector[,2]),1]), cex= 0.7, pos=3)
points(ResultsVector[which.min(ResultsVector[,2]),1], ResultsVector[which.min(ResultsVector[,2]),2], col="red", pch=19)

#############  SVM Regression
seq=c(1,5,10,50,100)
ResultsVector <- matrix(nrow=length(seq),ncol=2)
count=1

for(value in seq){

svmfit=svm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
             avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
           data.train.std.y, kernel="radial",gamma=1,cost=value)

summary(svmfit)
pred <- predict(svmfit,data.valid.std.y)
mean((pred-y.valid)^2)

ResultsVector[count,1]  = value
ResultsVector[count,2]  = mean((pred-y.valid)^2)
count=count+1
}
plot(ResultsVector[,1],ResultsVector[,2],xlab="Cost",ylab="MSE", main="Performance of Raidal SVM Regression, varying Cost")
lines(ResultsVector[,1],ResultsVector[,2])
ResultsVector[which.min(ResultsVector[,2]),]  #the best model

text(ResultsVector[which.min(ResultsVector[,2]),1], ResultsVector[which.min(ResultsVector[,2]),2], 
     labels=paste("Min MSE=",round(ResultsVector[which.min(ResultsVector[,2]),2],5), " X=",ResultsVector[which.min(ResultsVector[,2]),1]), cex= 0.7, pos=3)
points(ResultsVector[which.min(ResultsVector[,2]),1], ResultsVector[which.min(ResultsVector[,2]),2], col="red", pch=19)

#polynomial degree=2  (3.46837)
seq=c(2,3,4,5)
ResultsVector <- matrix(nrow=length(seq),ncol=2)
count=1

for(value in seq){

svmfit=svm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + genf + wrat*hinc +
             incm*inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
           data.train.std.y, kernel="polynomial",degree=value)

summary(svmfit)
pred <- predict(svmfit,data.valid.std.y)
mean((pred-y.valid)^2)

ResultsVector[count,1]  = value
ResultsVector[count,2]  = mean((pred-y.valid)^2)
count=count+1
}
plot(ResultsVector[,1],ResultsVector[,2],xlab="degree",ylab="MSE", main="Performance of Polymomial SVM Regression v degree")
lines(ResultsVector[,1],ResultsVector[,2])
ResultsVector[which.min(ResultsVector[,2]),]  #the best model

text(ResultsVector[which.min(ResultsVector[,2]),1], ResultsVector[which.min(ResultsVector[,2]),2], 
     labels=paste("Min MSE=",round(ResultsVector[which.min(ResultsVector[,2]),2],5), " X=",ResultsVector[which.min(ResultsVector[,2]),1]), cex= 0.7, pos=3)
points(ResultsVector[which.min(ResultsVector[,2]),1], ResultsVector[which.min(ResultsVector[,2]),2], col="red", pch=19)

############PCA Regression

pcr.fit=pcr(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + genf + wrat*hinc +
              incm*inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,
            data=data.train.std.y,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")


seq=c(1:20)
ResultsVector <- matrix(nrow=length(seq),ncol=2)
count=1

for(value in seq){
  
pcr.pred=predict(pcr.fit, data.valid.std.y,ncomp=value)
mean((pcr.pred-y.valid)^2)

ResultsVector[count,1]  = value
ResultsVector[count,2]  = mean((pcr.pred-y.valid)^2)
count=count+1
}
plot(ResultsVector[,1],ResultsVector[,2],xlab="#ncomp",ylab="MSE", main="Performance of PCA Regression, varying nComp")
lines(ResultsVector[,1],ResultsVector[,2])
ResultsVector[which.min(ResultsVector[,2]),]  #the best model

text(ResultsVector[which.min(ResultsVector[,2]),1], ResultsVector[which.min(ResultsVector[,2]),2], 
     labels=paste("Min MSE=",round(ResultsVector[which.min(ResultsVector[,2]),2],5), " X=",ResultsVector[which.min(ResultsVector[,2]),1]), cex= 0.7, pos=3)
points(ResultsVector[which.min(ResultsVector[,2]),1], ResultsVector[which.min(ResultsVector[,2]),2], col="red", pch=19)

#############Ridge Regression

x = model.matrix(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + genf + wrat + hinc +avhv+
                   incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, data.train.std.y)[,-1]
y = data.train.std.y$damt
testdata <-  data.valid.std.y
testdata$damt <- NULL
t = data.matrix(testdata)

grid=10^seq(10,-2,length=100)
model.ridge <- glmnet(x,y,alpha=0, lambda=grid)

cv.out=cv.glmnet(x,y,alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam=model.ridge$lambda.min

predict.ridge <-  predict(model.ridge,s=0.1253756,newx=t)
mean((predict.ridge -y.valid)^2)

seq=c(0.01,0.05,0.1,0.2,0.5,1)
ResultsVector <- matrix(nrow=length(seq),ncol=2)
count=1

for(value in seq){
  
  predict.ridge <-  predict(model.ridge,s=value,newx=t)
  mean((predict.ridge -y.valid)^2)
  
  ResultsVector[count,1]  = value
  ResultsVector[count,2]  =   mean((predict.ridge -y.valid)^2)
  count=count+1
}
plot(ResultsVector[,1],ResultsVector[,2],xlab="#s",ylab="MSE", main="Performance of Ridge Regression, varying s")
lines(ResultsVector[,1],ResultsVector[,2])
ResultsVector[which.min(ResultsVector[,2]),]  #the best model

############ LASSO

x = model.matrix(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + genf + wrat + hinc +avhv+
                   incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, data.train.std.y)[,-1]
y = data.train.std.y$damt
testdata <-  data.valid.std.y
testdata$damt <- NULL
t = data.matrix(testdata)

grid=10^seq(10,-2,length=100)
model.lasso <- glmnet(x,y,alpha=0, lambda=grid)
cv.out=cv.glmnet(x,y,alpha=1)
bestlam=cv.out$lambda.min
lasso.pred=predict(model.lasso,s=bestlam,newx=t)
mean((lasso.pred-y.valid)^2)

seq=c(0.01,0.05,0.1,0.2,0.5,1)
ResultsVector <- matrix(nrow=length(seq),ncol=2)
count=1

for(value in seq){
  
  lasso.pred=predict(model.lasso,s=value,newx=t)
  mean((lasso.pred-y.valid)^2)
  
  ResultsVector[count,1]  = value
  ResultsVector[count,2]  =    mean((lasso.pred-y.valid)^2)
  count=count+1
}
plot(ResultsVector[,1],ResultsVector[,2],xlab="s",ylab="MSE", main="Performance of Lasso Regression, varying s")
lines(ResultsVector[,1],ResultsVector[,2])
ResultsVector[which.min(ResultsVector[,2]),]  #the best model

########## partial least squares

pls.fit=plsr(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + genf + wrat*hinc +
               incm*inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,
             data=data.train.std.y,scale=TRUE,validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")

seq=c(1:20)
ResultsVector <- matrix(nrow=length(seq),ncol=2)
count=1

for(value in seq){
  
  pls.pred=predict(pls.fit,data.valid.std.y,ncomp=value)
  mean((pls.pred-y.valid)^2)
  
  ResultsVector[count,1]  = value
  ResultsVector[count,2]  =  mean((pls.pred-y.valid)^2)
  count=count+1
}
plot(ResultsVector[,1],ResultsVector[,2],xlab="nComp",ylab="MSE", main="Performance of Partial LS Regression, varying nComp")
lines(ResultsVector[,1],ResultsVector[,2])
ResultsVector[which.min(ResultsVector[,2]),]  #the best model

text(ResultsVector[which.min(ResultsVector[,2]),1], ResultsVector[which.min(ResultsVector[,2]),2], 
     labels=paste("Min MSE=",round(ResultsVector[which.min(ResultsVector[,2]),2],5), " X=",ResultsVector[which.min(ResultsVector[,2]),1]), cex= 0.7, pos=3)
points(ResultsVector[which.min(ResultsVector[,2]),1], ResultsVector[which.min(ResultsVector[,2]),2], col="red", pch=19)

