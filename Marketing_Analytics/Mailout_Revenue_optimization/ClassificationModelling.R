

##### CLASSIFICATION MODELING ######

####################### linear discriminant analysis

library(MASS)

model.lda1 <- lda(donr  ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv+   I(wrat^2) + incm +   I(incm^2)  + inca  + plow + plow + npro + npro + tgif +I(tdon^2)
                  + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c) # include additional terms on the fly using I()

# Note: strictly speaking, LDA should not be used with qualitative predictors,
# but in practice it often is if the goal is simply to find a good predictive model

post.valid.lda1 <- predict(model.lda1, data.valid.std.c)$posterior[,2] # n.valid.c post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.lda1 <- cumsum(14.5*c.valid[order(post.valid.lda1, decreasing=T)]-2)
plot(profit.lda1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.lda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.lda1)) # report number of mailings and maximum profit

cutoff.lda1 <- sort(post.valid.lda1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.lda1 <- ifelse(post.valid.lda1>cutoff.lda1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.lda1, c.valid) # classification table
#               c.valid
#chat.valid.lda1   0   1
#              0 675  14
#              1 344 985
# check n.mail.valid = 344+985 = 1329
# check profit = 14.5*985-2*1329 = 11624.5

####################  logistic regression

model.log1 <- glm(donr  ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv+   I(wrat^2) + incm +   I(incm^2)  + inca  + plow + plow + npro + npro + tgif +I(tdon^2)
                  + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c, family=binomial("logit"))

post.valid.log1 <- predict(model.log1, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.log1 <- cumsum(14.5*c.valid[order(post.valid.log1, decreasing=T)]-2)
plot(profit.log1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.log1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.log1)) # report number of mailings and maximum profit

cutoff.log1 <- sort(post.valid.log1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.log1 <- ifelse(post.valid.log1>cutoff.log1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.log1, c.valid) # classification table
#               c.valid
#chat.valid.log1   0   1
#              0 709  18
#              1 310 981
# check n.mail.valid = 310+981 = 1291
# check profit = 14.5*981-2*1291 = 11642.5

# Results

# n.mail Profit  Model
# 1329   11624.5 LDA1
# 1291   11642.5 Log1


####################  PROBIT regression

model.log1 <- glm(donr  ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv+   I(wrat^2) + incm +   I(incm^2)  + inca  + plow + plow + npro + npro + tgif +I(tdon^2)
                  + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c, family=binomial("probit"))

post.valid.log1 <- predict(model.log1, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.log1 <- cumsum(14.5*c.valid[order(post.valid.log1, decreasing=T)]-2)
plot(profit.log1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.log1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.log1)) # report number of mailings and maximum profit

cutoff.log1 <- sort(post.valid.log1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.log1 <- ifelse(post.valid.log1>cutoff.log1, 1, 0) # mail to everyone above the cutoff

################# GAM Model  (1341 11473)

model.gam <- gam(donr  ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                   avhv+   I(wrat^2) + incm +   I(incm^2)  + inca  + plow + plow + npro + npro + tgif +I(tdon^2)
                 + lgif + rgif + tdon + tlag + agif, 
                  data=data.train.std.c, family=binomial)

summary(model.gam)
pred.gam <- predict(model.gam, data.valid.std.c, type="response") # n.valid post probs
profit.gam <- cumsum(14.5*c.valid[order(pred.gam, decreasing=T)]-2)
plot(profit.gam) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.gam) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.gam )) # report number of mailings and maximum profit

################  QDA (1298 11208)

model.qda <- qda(donr  ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                   avhv+   I(wrat^2) + incm +   I(incm^2)  + inca  + plow + plow + npro + npro + tgif +I(tdon^2)
                 + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c) # include additional terms on the fly using I()
pred.qda <- predict(model.qda , data.valid.std.c)$posterior[,2] # n.valid.c post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.qda <- cumsum(14.5*c.valid[order(pred.qda, decreasing=T)]-2)
plot(profit.qda) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.qda) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.qda)) # report number of mailings and maximum profit

################  KNN  (1298 11208)

#k=all

seq=c(5,10,15,19,20,21,23,24,25,26,30,40,50,100)
ResultsVector <- matrix(nrow=length(seq),ncol=3)
count=1

for(value in seq){
  
model.knn  <- kknn(donr  ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                     avhv+   I(wrat^2) + incm +   I(incm^2)  + inca  + plow + plow + npro + npro + tgif +I(tdon^2)
                   + lgif + rgif + tdon + tlag + agif
                   , train = data.train.std.c, test =  data.valid.std.c, k=value)
predict.knn <- model.knn$fitted.values
profit.knn <- cumsum(14.5*c.valid[order(predict.knn , decreasing=T)]-2)
#plot(profit.knn) # see how profits change as more mailings are made\
n.mail.valid <- which.max(profit.knn) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.knn)) # report number of mailings and maximum profit

ResultsVector[count,1]  = value
ResultsVector[count,2]  = n.mail.valid
ResultsVector[count,3]  = max(profit.knn)

count=count+1
}

plot(ResultsVector[,1],ResultsVector[,3],xlab="k",ylab="profit" , main="Performance of KNN Classifier, varying K")
lines(ResultsVector[,1],ResultsVector[,3])
ResultsVector[which.max(ResultsVector[,3]),]  #the best model

text(ResultsVector[which.max(ResultsVector[,3]),1], ResultsVector[which.max(ResultsVector[,1]),3], 
     labels=paste("Max Profit=",round(ResultsVector[which.max(ResultsVector[,3]),3],5), " X=",ResultsVector[which.max(ResultsVector[,3]),1]), cex= 0.7, pos=3)
points(ResultsVector[which.max(ResultsVector[,3]),1], ResultsVector[which.max(ResultsVector[,3]),3], col="red", pch=19)

# Support Vector Machine
#linear

seq=c(0.005,0.01, 0.1,1)
ResultsVector <- matrix(nrow=length(seq),ncol=3)
count=1

for(value in seq){

svmfit=svm(donr  ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
             avhv+   I(wrat^2) + incm +   I(incm^2)  + inca  + plow + plow + npro + npro + tgif +I(tdon^2)
           + lgif + rgif + tdon + tlag + agif, 
           data.train.std.c, kernel="linear",cost=value)

summary(svmfit)
pred.svm <- predict(svmfit,data.valid.std.c)
profit.svm <- cumsum(14.5*c.valid[order(pred.svm , decreasing=T)]-2)
plot(profit.svm) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.svm) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.svm)) # report number of mailings and maximum profit


ResultsVector[count,1]  = value
ResultsVector[count,2]  = n.mail.valid
ResultsVector[count,3]  = max(profit.svm)

count=count+1
}

plot(ResultsVector[,1],ResultsVector[,3],xlab="cost",ylab="profit", main="Performance of Linear SVM Classifier, varying Cost")
lines(ResultsVector[,1],ResultsVector[,3])
ResultsVector[which.max(ResultsVector[,3]),]  #the best model

text(ResultsVector[which.max(ResultsVector[,3]),1], ResultsVector[which.max(ResultsVector[,1]),3], 
     labels=paste("Max Profit=",round(ResultsVector[which.max(ResultsVector[,3]),3],5), " X=",ResultsVector[which.max(ResultsVector[,3]),1]), cex= 0.7, pos=3)
points(ResultsVector[which.max(ResultsVector[,3]),1], ResultsVector[which.max(ResultsVector[,3]),3], col="red", pch=19)

