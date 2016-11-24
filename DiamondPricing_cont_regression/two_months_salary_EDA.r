
library("lattice")

setwd("~/Downloads/PA454/3_week")
df <- read.csv("two_months_salary.csv")
#row count
nrow(df)

#get cont variables
df_cont <-data.frame(df$carat, df$color, df$clarity, df$price) 
#get cat variables 
df_cat <- data.frame(df$cut, df$channel, df$store )
#get target
df_target <- data.frame(df$price)
  
#1 basic stats
library("fBasics")
BasicStatsDF <- my.Summary(df_cont)
write.csv(BasicStatsDF,"basicStats.csv")

#2 distributions (cont)
dev.off()
par(mfrow=c(2,2))
PlotHistograms(df_cont)

#transformed predictors
dev.off()
par(mfrow=c(2,3))
PlotTransformedHistograms(df_cont)

#plot pairs, big enought ot be visible 
PlotPairsAgainstTarget(df_cont$df.price, df_cont[,-4])
PlotPairsAgainstTarget(log(df_cont$df.price), df_cont[,-4])
PlotTransformedPairsAgainstTarget(df_cont$df.price, df_cont[,1])
PlotTransformedPairsAgainstTarget(df_cont$df.price, df_cont[,2])
PlotTransformedPairsAgainstTarget(df_cont$df.price, df_cont[,3])

#target
dev.off()
PlotHistograms(df_target)

#conditional boxplots 
densityplot(~ df[,7] , group=channel, data = df )
densityplot(~ df[,7] , group=store, data =  df , ylim=c(0,0.0005))
densityplot(~ df[,7] , group=cut, data = df )

boxplot(df$price~df$channel , cex.axis=1, las=3)
boxplot(df$price~df$store , cex.axis=1, las=3)
boxplot(df$price~df$cut , cex.axis=1, las=3)

#transformed target
dev.off()
par(mfrow=c(2,3))
PlotTransformedHistograms(df_target)

#3 distributions (cat)
#this is for when the binaries are split already
#barchart(colSums(df[,11:14]))

#not split
PlotCategoricalBarCharts(df_cat)

#4 correlations
cor <- cor(df_cont, method="pearson")
library(ggplot2)
library(reshape2)
qplot(x=Var1, y=Var2, data=melt(cor), fill=value, geom="tile")

library(corrplot)
col3 <- colorRampPalette(c("white", "blue")) 
corrplot(cor, order="AOE",  method="color",  col=col3(10),addCoef.col="black", tl.cex=1, tl.col="black",type = c("lower"),diag = FALSE)
corrplot(cor)
pairs(df_cont)

#5 outliers
dev.off()
boxplot(scale(df_cont), cex.axis=0.60, las=3)
x <- GetTotalSDsfromMean(df_cont)
write.csv(x,"sdsfromMean.csv")

#6 fit a tree for EDA (methood="class" or "anova" )
library(rpart)
dev.off()
EDA_tree <- rpart(price~., data=df, method="anova",control = rpart.control(minsplit = 20))
plot(EDA_tree, uniform = TRUE,margin = 0.2)
text(EDA_tree, use.n=TRUE, all=TRUE, cex=0.8)
summary(EDA_tree)
round(EDA_tree$variable.importance/100000000)

#7 Model based EDA (RF)
library("randomForest")
rf = randomForest(price ~ ., data = df, mtry = 5, ntree = 500, importance = T)
rf.pred = predict(rf, df)
mean((df$price - rf.pred)^2)
importance(rf,type=1)
varImpPlot(rf)
partialPlot(rf, df, "carat" )
partialPlot(rf, df, "color" )
partialPlot(rf, df, "clarity" )

#trying to find interaction
library(plotmo)
plotmo(rf)

#8 Clustering for EDA (not used)
pairs(scale(df[,2:8])
      pairs(df[,9:14])
      pairs(df[,c(2,3,4,5)])

#9 PCA for EDA (not ued)
library(ggbiplot)
df.pca <- prcomp(df_cont,center = TRUE,scale. = TRUE) 
g <- ggbiplot(df.pca, obs.scale = 1, var.scale = 1, circle = TRUE)
plot(g)
points(g, cex = .5, col = "dark red")

#### MODELLING
#recoding the categoricals
df2 <- df
cats <- data.frame((cbind((cbind(with(df2, model.matrix(~ cut + 0)), with(df2, model.matrix(~ channel + 0)) , with(df2, model.matrix(~ store + 0)))))))
df2 <- data.frame(cats, df)
df2 <- df2[,-c(21,22,23)]

#set up training/test
temp_index <- sample(nrow(df2), round(nrow(df2)*0.3))  
df_training <- df2[-temp_index,]
df_test <- df2[temp_index,]

#training/test for log variables
df_training_2 <- df_training
df_test_2 <- df_test
df_training_2$carat <- log(df_training$carat)
df_test_2$carat <- log(df_test$carat)

library(leaps)
#10 simple linear  model
model1 <- lm(price~. , data=df_training)
pred1 <- predict(model1, df_test)
MSE <- mean((pred1 - df_test$price)^2) 
MSE

model1 <- lm(log(price)~. , data=df_training)
pred1 <- predict(model1, df_test)
MSE <- mean((exp(pred1) - df_test$price)^2)
MSE

#looking at the log-log correlation
model1 <- lm(log(price)~. , data=df_training_2)
pred1 <- predict(model1, df_test_2)
MSE <- mean((exp(pred1) - df_test$price)^2)
MSE

#best subset, tforms
model2<-regsubsets(price~.,data=df_training_2,nbest=1, method=c("exhaustive"))
summary(model2)
plot(model2,scale="adjr2")
summary.out2 <- summary(model2) 
which.max(summary.out2$adjr2)
summary.out4$which[9,]

model_2 <- lm(log(price)~cutIdeal + channelMall +storeAshford + storeFred.Meyer + storeGoodmans + storeRiddles+ carat +color +clarity, data=df_training_2)
pred2 <- predict(model_2, df_test_2)
MSE <- sum((exp(pred2) - df_test$price)^2) /nrow(df_test)
RMSE <- sqrt(MSE)
MSE
RMSE
AIC(model_2 )
BIC(model_2 )

#backward
model3<-regsubsets(price~.,data=df_training,nbest=1, method=c("backward"))
summary(model3)
plot(model3,scale="adjr2")
summary.out3 <- summary(model3) 
which.max(summary.out3$adjr2)
summary.out4$which[9,]

model_3 <- lm(price~cutIdeal + channelMall +storeAshford + storeFred.Meyer + storeGoodmans + storeRiddles+ carat +color +clarity, data=df_training)
pred3 <- predict(model_3, df_test)
MSE <- sum((pred3 - df_test$price)^2) /nrow(df_test)
RMSE <- sqrt(MSE)
MSE
RMSE
AIC(model_3 )
BIC(model_3 )

#backward, transforms
model32<-regsubsets(price~.,data=df_training_2,nbest=1, method=c("backward"))
summary(model32)
summary.out32 <- summary(model32) 
which.max(summary.out32$adjr2)
summary.out4$which[9,]

model_32 <- lm(log(price)~cutIdeal + channelMall +storeAshford + storeFred.Meyer + storeGoodmans + storeRiddles+ carat +color +clarity, data=df_training_2)
pred32 <- predict(model_32, df_test_2)
MSE <- sum((exp(pred32) - df_test_2$price)^2) /nrow(df_test_2)
RMSE <- sqrt(MSE)
MSE
RMSE
AIC(model_32 )
BIC(model_32 )

#forward, transforms
model4<-regsubsets(price~.,data=df_training_2,nvmax = 20, method=c("forward"))
summary(model4)
model4$rss
dev.off
par(mfrow=c(1,1))
plot(model4,scale="r2")
plot(model4, scale="adjr2")
plot(model4, scale="bic")
summary.out4 <- summary(model4) 
which.max(summary.out4$adjr2)
summary.out4$which[10,]

model_4 <- lm(log(price)~cutIdeal +	 channelIndependent + channelMall +storeKay + storeChalmers +storeFred.Meyer + storeGoodmans +  storeRiddles+ carat +color +clarity, data=df_training_2)
pred4 <- predict(model_4, df_test_2)
MSE <- sum((exp(pred4) - df_test$price)^2) /nrow(df_test)
RMSE <- sqrt(MSE)
MSE
RMSE
AIC(model_4 )
BIC(model_4 )
summary(model_4)

#forward, transforms, interactions
df_training_3 <- df_training_2
df_test_3 <- df_test_2
df_training_3$CaretColor <- df_training_3$carat*df_training_3$color
df_test_3$CaretColor <- df_test_3$carat*df_test_3$color
df_training_3$CaretCla <- df_training_3$carat*df_training_3$clarity
df_test_3$CaretCla <- df_test_3$carat*df_test_3$clarity

model9<-regsubsets(log(price)~.,data=df_training_3,nvmax = 21, method=c("forward"))
summary(model9)
summary.out9 <- summary(model9) 
which.max(summary.out9$adjr2)
summary.out9$which[12,]

model_9 <- lm(log(price)~cutIdeal +	 channelIndependent + channelMall +storeKay + storeChalmers +storeFred.Meyer + storeGoodmans +  storeRiddles+ carat +color +clarity+CaretCla+CaretColor, data=df_training_3)
pred9 <- predict(model_9, df_test_3)
MSE <- sum((exp(pred9) - df_test$price)^2) /nrow(df_test)
RMSE <- sqrt(MSE)
MSE
RMSE
AIC(model_9 )
BIC(model_9 )
summary(model_9)

#stepwise
library(MASS)
model5 <- lm(log(price)~.,data=df_training_2 )
slm.stepwise <- step(model5,direction="both")
step <- stepAIC(model5, direction="both")
step$anova # display results 
summary(step)
pred5 <- predict(step, df_test_2)
MSE <- sum((exp(pred5) - df_test$price)^2) /nrow(df_test)
RMSE <- sqrt(MSE)
MSE
RMSE
AIC(model5  )
BIC(model5  )
summary(model5)

#RF
library("randomForest")
rf = randomForest(price ~ ., data = df_training, mtry = 20, ntree = 500, importance = T)
rf.pred = predict(rf, df_test)
MSE = mean((df_test$price - rf.pred)^2)
RMSE <- sqrt(MSE)
MSE
RMSE

#RF
library("randomForest")
rf = randomForest(log(price) ~ ., data = df_training_2, mtry = 20, ntree = 500, importance = T)
rf.pred = predict(rf, df_test_2)
MSE = mean((df_test_2$price - exp(rf.pred))^2)
RMSE <- sqrt(MSE)
MSE
RMSE

#RF
library("randomForest")
rf = randomForest(log(price) ~ ., data = df_training_3, mtry = 10, ntree = 500, importance = T)
rf.pred = predict(rf, df_test_3)
MSE = mean((df_test_2$price - exp(rf.pred))^2)
RMSE <- sqrt(MSE)
MSE
RMSE

#Lasso
library(glmnet)
x<-model.matrix(log(price)~., df_training_2)
y<-log(df_training$price)
grid=10^seq(10,-2, length=100)
lasso <- glmnet(x,y,alpha=1,lambda=grid)
plot(lasso)
#cv
lasso.cv <- cv.glmnet(x,y,alpha=1,lambda=grid)
bestlam <- lasso.cv$lambda.min
#prediction
x_test <- model.matrix(log(price)~., df_test_2)
lasso.pred <- predict(lasso, s=bestlam, newx=x_test)
MSE <- mean((exp(lasso.pred) - df_test$price)^2)
RMSE <- sqrt(MSE)
MSE
RMSE
AIC(lasso   )
BIC(lasso   )
summary(model5)

#trees! (methood="class" or "anova" )
library(rpart)
dev.off()
EDA_tree <- rpart(price~., data=df_training, method="anova",control = rpart.control(minsplit = 5))
pred_TREE <- predict(EDA_tree, df_test)
MSE <- mean((pred_TREE - df_test$price)^2)
RMSE <- sqrt(MSE)
MSE
RMSE

EDA_tree <- rpart(log(price)~., data=df_training_2, method="anova",control = rpart.control(minsplit = 5))
pred_TREE <- predict(EDA_tree, df_test_2)
MSE <- mean((exp(pred_TREE) - df_test$price)^2)
RMSE <- sqrt(MSE)
MSE
RMSE

EDA_tree <- rpart(log(price)~., data=df_training_3, method="anova",control = rpart.control(minsplit = 5))
pred_TREE <- predict(EDA_tree, df_test_3)
MSE <- mean((exp(pred_TREE) - df_test$price)^2)
RMSE <- sqrt(MSE)
MSE
RMSE


