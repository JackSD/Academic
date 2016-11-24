#setting up the data set
setwd("~/Downloads/PA454/6_week")

df <- read.csv("spambase.data", header = FALSE)
header <- read.csv("spambase.names", header = FALSE)
t <- c("spam")
r <- as.character(unlist(header))
headervec <-  c(r,t)
colnames(df) <- headervec

#fixing the spam as a a factor
df$spam <- as.factor(df$spam)

# Basic Stats)
stats <- my.Summary(df_cont)
write.csv(stats, "stats.csv")

# Histograms
PlotHistograms(df_cont)
PlotNonZeroHistograms(df_cont)

#boxplots for outliers 
boxplot(scale(df_cont[,1:10]), cex.axis=0.60, las=3)

#4 correlations
df_cont <- df[,-58]
cor <- cor(df_cont, method="pearson")
library(ggplot2)
library(reshape2)
qplot(x=Var1, y=Var2, data=melt(cor), fill=value, geom="tile")

library(corrplot)
col3 <- colorRampPalette(c("white", "blue")) 
corrplot(cor, order="AOE",  method="color",  col=col3(10),addCoef.col="black", tl.cex=1, tl.col="black",type = c("lower"),diag = FALSE)
corrplot(cor)

#get the top correlations
library(dplyr)
library(reshape2)
d_cor <- as.matrix(cor)
d_cor_melt <- arrange(melt(d_cor), -abs(value))
write.csv(d_cor_melt, "correlations.csv")

#Advanced EDA: conditional density plots
PlotConditionalDesityPlotsGrouped(df)

#Model based EDA (tree)
EDA_tree <- rpart(spam~., data=df, method="anova",control = rpart.control(minsplit = 20))
plot(EDA_tree, margin = 0.05)
text(EDA_tree, use.n=TRUE, all=TRUE, cex=0.7)
summary(EDA_tree)
write.csv(EDA_tree$variable.importance,"varimp.csv")

#Model based EDA (RF)
library("randomForest")
rf = randomForest(spam ~ ., data = df, mtry = 5, ntree = 500, importance = T)
importance(rf,type=1)
varImpPlot(rf)
partialPlot(rf, df, "word_freq_project" )

#trying to find interaction
library(plotmo)
plotmo(rf)

#actual modelling
#set up training/test
temp_index <- sample(nrow(df), round(nrow(df)*0.3))  
df_training <- df[-temp_index,]
df_test <- df[temp_index,]

rownames(df_test) <- seq(length=nrow(df_test))
rownames(df_training) <- seq(length=nrow(df_training))
validation_index <- sample(nrow(df_training), round(nrow(df_training)*0.2))  

#for the performance meausre functions:
df_test$class <- df_test$spam

#model1: baseline linear regression
model1 <- glm(spam ~ ., data = df_training, family = "binomial")
summary(model1)
pred1 <- predict(model1, df_test, type="response")
df_test$scored.class <- round(pred1,0)
PrintPerformanceMetrics(df_test)
write.csv(model1$coefficients, "coef.csv")

#model2: stepwise glm selection
model2 <- step(model1, direction="both")
summary(model2)
pred2 <- predict(model2, df_test, type="response")
df_test$scored.class <- round(pred2,0)
PrintPerformanceMetrics(df_test)

#model3: forward glm selection
model3 <- step(model1, direction="forward")
summary(model3)
pred3 <- predict(model3, df_test, type="response")
df_test$scored.class <- round(pred3,0)
PrintPerformanceMetrics(df_test)

#model4: backward glm selection
model4 <- step(model1, direction="backward")
summary(model4)
pred4 <- predict(model4, df_test, type="response")
df_test$scored.class <- round(pred4,0)
PrintPerformanceMetrics(df_test)

#trees (methood="class" or "anova" )
library(rpart)
tree <- rpart(spam~., data=df_training, control = rpart.control(minsplit = 5))
pred_TREE <- predict(tree, df_test, type = c("class"))
df_test$scored.class <- pred_TREE
PrintPerformanceMetrics(df_test)

#tree, plotting
plot(tree, uniform = TRUE,margin = 0.2)
text(tree, use.n=TRUE, all=TRUE, cex=0.8)

#model: support vector machine
library(e1071)
c <- 10
svmmodel <- svm(spam ~ ., data = df_training, kernel="linear", cost = c, scale=FALSE)
svmpred <- predict(svmmodel, df_test)
df_test$scored.class <- svmpred
PrintPerformanceMetrics(df_test)

svmmodel2 <- svm(spam ~ ., data = df_training, kernel="linear", cost = c, scale=TRUE)
svmpred2 <- predict(svmmodel2, df_test)
df_test$scored.class <- svmpred2
PrintPerformanceMetrics(df_test)

svmmodel3 <- svm(spam ~ ., data = df_training, kernel="polynomial", cost = c, scale=TRUE)
svmpred3 <- predict(svmmodel3, df_test)
df_test$scored.class <- svmpred3
PrintPerformanceMetrics(df_test)

svmmodel4 <- svm(spam ~ ., data = df_training, kernel="radial", cost = c, scale=TRUE)
svmpred4 <- predict(svmmodel4, df_test)
df_test$scored.class <- svmpred4
PrintPerformanceMetrics(df_test)

#using the internal cross validation of SVM
svmtune <- tune(svm,as.numeric(spam) ~ ., data = df_training, kernel="radial", ranges = list(cost=c(1,10,50, 100),gamma=c(0.01, 0.1, 0.5, 1)))

graphSVM <- summary(svmtune)$performances
plot(graphSVM[,2],graphSVM[,3] , xlab="Cost", ylab="CV Error", main="SVM, CV Error verus Gamma Parameter, for 4 costs (1,10,50, 100)")
lines(graphSVM[graphSVM[,1]==1e+02,2],graphSVM[graphSVM[,1]==1e+02,3] )
lines(graphSVM[graphSVM[,1]==5e+01,2],graphSVM[graphSVM[,1]==5e+01,3] )
lines(graphSVM[graphSVM[,1]==1e+01,2],graphSVM[graphSVM[,1]==1e+01,3] )
lines(graphSVM[graphSVM[,1]==1e+00,2],graphSVM[graphSVM[,1]==1e+00,3] )

svmmodel5 <- svm(spam ~ ., data = df_training, kernel="sigmoid", cost = c, scale=TRUE)
svmpred5 <- predict(svmmodel5, df_test)
df_test$scored.class <- svmpred5
PrintPerformanceMetrics(df_test)

#best model
df_training$spam <- as.factor(df_training$spam)
svmmodel6 <- svm(spam ~ ., data = df_training, kernel="radial", cost = 10, gamma=0.01, scale=TRUE)
svmpred6 <- predict(svmmodel6, df_test)
df_test$scored.class <- svmpred6
PrintPerformanceMetrics(df_test)

#best model, reduced variable set
svmmodel7 <- svm(formula1, data = df_training, kernel="radial", cost = 10, gamma=0.01, scale=TRUE)
svmpred7 <- predict(svmmodel7, df_test)
df_test$scored.class <- svmpred7
PrintPerformanceMetrics(df_test)

#model: knn, note for report: scaling improves perofrmance significantly, presumably because it distorts distance less
library(class)
results = matrix(nrow=50,ncol=4) 
for(kn in 1:50)
{
predknn <- knn(scale(df_training[,1:57]), scale(df_test[,1:57]), df_training[,58], k = kn)
df_test$scored.class <- predknn
results[kn,1] <- kn
results[kn,2] <- FAccuracy(df_test)
results[kn,3] <- FPrecision(df_test)
results[kn,4] <- FSensitivity(df_test)
}
plot(results, xlab="k", ylab="Performance", main="K versus accuracy, KNN (scaled, euclidian distance)", ylim=c(0.77,0.92))
lines(results, col="blue")
lines(results[,1],results[,3], col="brown")
lines(results[,1],results[,4])
points(results[,1],results[,3])
points(results[,1],results[,4])
#lines for the best k
maxAccuracy <- results[which.max(2*results[,3]+results[,4]),1]
lines(c(maxAccuracy,maxAccuracy),c(0,1), col="red",lty=2)
text(x=maxAccuracy+0.5, y=0.78, paste("Best performance when k = ",maxAccuracy),pos=4, col="red")

text(44, 0.795,"Sensitivity")
text(44, 0.905,"Precision")
text(44, 0.865,"Accuracy")

#model: RF (tuning tree number)
results = matrix(nrow=11,ncol=4) 
n<-1
for(tr in seq(from=1, to=501, by=50))
{
rf = randomForest(spam ~ ., data = df_training, mtry = 5, ntree = tr, importance = T)
pred7 <- predict(rf,df_test)
df_test$scored.class <- pred7
df_test$class <- df_test$spam
results[n,1] <- tr
results[n,2] <- FAccuracy(df_test)
results[n,3] <- FPrecision(df_test)
results[n,4] <- FSensitivity(df_test)
n<-n+1
}

plot(results, xlab="trees", ylab="", main="#trees versus accuracy/precision/specificity, Random Forest", ylim=c(0.89,0.97))
lines(results)
lines(results[,1],results[,3])
lines(results[,1],results[,4])
points(results[,1],results[,3])
points(results[,1],results[,4])

#model: RF (tuning mtry number)
results = matrix(nrow=14,ncol=4) 
n<-1
for(mt in c(2,3,4,5,6,7,8,9,10,15,20,30,40,50))
{
  rf = randomForest(spam ~ ., data = df_training, mtry = mt, ntree = 100, importance = T)
  pred7 <- predict(rf,df_test)
  df_test$scored.class <- pred7
  df_test$class <- df_test$spam
  results[n,1] <- mt
  results[n,2] <- FAccuracy(df_test)
  results[n,3] <- FPrecision(df_test)
  results[n,4] <- FSensitivity(df_test)
  n<-n+1
}

plot(results, xlab="mtry", ylab="", main="mtry versus accuracy/precision/specificity, Random Forest", ylim=c(0.89,0.97))
lines(results)
lines(results[,1],results[,3])
lines(results[,1],results[,4])
points(results[,1],results[,3])
points(results[,1],results[,4])

#plot a vertical line with the max accuracy:
maxAccuracy <- results[which.max(2*results[,3]+results[,4]),1]
lines(c(maxAccuracy,maxAccuracy),c(0,1), col="red",lty=2)
text(x=maxAccuracy+0.5, y=0.89, paste("Best performance when mtry = ",maxAccuracy),pos=4, col="red")

text(44, 0.9,"Sensitivity")
text(44, 0.925,"Precision")
text(44, 0.95,"Accuracy")

#RF, reduced variables
df_training$spam <- as.factor(df_training$spam)
formula1 <- as.formula("spam ~ word_freq_address + word_freq_3d + word_freq_our + word_freq_over + word_freq_remove + word_freq_internet + word_freq_order + word_freq_mail + word_freq_receive + word_freq_will + word_freq_addresses + word_freq_free + word_freq_business + word_freq_you + word_freq_credit + word_freq_your + word_freq_000 + word_freq_money + word_freq_hp + word_freq_hpl + word_freq_george + word_freq_650 + word_freq_lab + word_freq_telnet + word_freq_data + word_freq_85 + word_freq_technology + word_freq_parts + word_freq_pm + word_freq_cs + word_freq_meeting + word_freq_original + word_freq_project + word_freq_re + word_freq_edu + word_freq_table + word_freq_conference + char_freq_semic + char_freq_exclamationMark + char_freq_dollarSign + char_freq_Hash + capital_run_length_average + capital_run_length_longest + capital_run_length_total")
rf2 = randomForest(formula1, data = df_training, mtry = 5, ntree = 100, importance = T)
pred8 <- predict(rf2,df_test)
df_test$scored.class <- pred8
df_test$class <- df_test$spam
PrintPerformanceMetrics(df_test)

#boosting
library(gbm)
df_training$spam <- as.character(df_training$spam)

results = matrix(nrow=4,ncol=4) 
n<-1
for(idde in c(1,2,4,8))
{
modelboost <- gbm(spam ~ ., data = df_training, distribution="bernoulli", n.trees=1000, interaction.depth=idde, verbose = TRUE)
boostpred <- predict(modelboost, df_test, n.trees=1000, type="response")
boostpred <- round(boostpred)
df_test$scored.class <- boostpred
results[n,1] <- idde
results[n,2] <- FAccuracy(df_test)
results[n,3] <- FPrecision(df_test)
results[n,4] <- FSensitivity(df_test)
n<-n+1
}

plot(results, xlab="interaction depth", ylab="", main="Boosting: interaction depth versus accuracy/precision/specificity", ylim=c(0.6,0.96))
lines(results)
lines(results[,1],results[,3])
lines(results[,1],results[,4])
points(results[,1],results[,3])
points(results[,1],results[,4])

text(7, 0.83,"Sensitivity")
text(7, 0.95,"Precision")
text(7, 0.89,"Accuracy")

#plot a vertical line with the max accuracy:
maxAccuracy <- results[which.max(2*results[,3]+results[,4]),1]
lines(c(maxAccuracy,maxAccuracy),c(0,1), col="red",lty=2)
text(x=maxAccuracy-0.5, y=0.7, paste("Best performance when interaction depth = ",maxAccuracy),pos=2, col="red")

