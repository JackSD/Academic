library("lattice")
library("outliers")

setwd("~/Downloads/PA454/1_Week/data")

#load the data
df <- read.csv("wine.data")

#add the column names to the data
colnames(df) <- c("classID","Alcohol","MalicAcid","Ash","AlcalinityOfAsh","Magnesium","TotalPhenols","Flavanoids","NonflavanoidPhenols","Proanthocyanins","ColorIntensity","Hue","OD280OD315OfDilutedWines","Proline")   

#0 real world interpretation of data
#done.

#1 basic descriptive statistics
library("fBasics")

BasicStatsDF <- my.Summary(df)
write.csv(BasicStatsDF, "basicstats.csv")  

#2 distributions
dev.off()
par(mfrow=c(4,4))

#1.5 Understanding the distribution of the target variable
png(filename="ClassIDHist.png")
histogram(classID)
dev.off()

PlotConditionalHistograms(df)
PlotConditionalDesityPlots(df)
PlotConditionalDesityPlotsGrouped(df)
PlotConditionalXYPlotsGrouped(df)
PLotQQPlots(df)

#log transformed histos?

#QQ plots TO CHECK FOR THE NORMALITY OF variables
qqmath(~df$TotalPhenols, f.value =  ppoints(100))
qqmath(~df$TotalPhenols, f.value =  ppoints(100), groups=df$classID ,data=df)
qqmath(~df$ColorIntensity, f.value =  ppoints(100))
qqmath(~df$ColorIntensity, f.value =  ppoints(100), groups=df$classID ,data=df)

#3 missing data

#The EDA indicates there is no missing values in this data set, so noimputation of missing data will be required

#4 correlations (heat map) (grid)
cor(df, method="pearson")

library(ggplot2)
library(reshape2)
qplot(x=Var1, y=Var2, data=melt(cor(df[,2:14])), fill=value, geom="tile")

#studing the strong correlations in more depth
#SCATTER PLOTS OF CORRELATED VARIABLES?

attach(df)
png(filename="AlcProline.png")
xyplot(Alcohol ~ Proline, Data=df , groups = df$classID , cex=2, pch = 21)
dev.off()
png(filename="TotalP_Flava.png")
xyplot(TotalPhenols ~ Flavanoids, Data=df , groups = df$classID,cex=2, pch = 21 )
dev.off()
png(filename="OfDiluted_Flava.png")
xyplot(Flavanoids ~ OD280OD315OfDilutedWines, Data=df, groups = df$classID, cex=2, pch = 21  )
dev.off()
png(filename="Proanthocyanins_Flava.png")
xyplot(Flavanoids ~ Proanthocyanins, Data=df, groups = df$classID ,cex=2,  pch = 21 )
dev.off()


#5 outliers
#boxplots: ash magnesium, flavonoids, hue
boxplot(scale(df[,2:14]), cex.axis=0.60, las=3)

chisq.out.test(df$Ash)
chisq.out.test(df$Magnesium)

which.max(scores(df$Ash, type = c("z")))
which.min(scores(df$Ash, type = c("z")))

which.max(scores(df$Magnesium, type = c("z")))
which.min(scores(df$Magnesium, type = c("z")))

grubbs.test(df$Alcohol, type = 10)
grubbs.test(df$Ash, type = 11)
grubbs.test(df$Magnesium, type = 11)

#5.5 do I need to look at transform correltations?

#conditional dists?
#conditional correlations?

#6 fit a tree for EDA
library(rpart)
EDA_tree <- rpart(classID~., data=df, method="class", minsplit=5)
plot(EDA_tree)
text(EDA_tree, use.n=TRUE, all=TRUE, cex=0.8)

summary(EDA_tree)

#7 Model based EDA (RFs?)

#8 Clustering for EDA
pairs(scale(df[,2:8])
pairs(df[,9:14])
pairs(df[,c(2,3,4,5)])

tempdf <- data.frame(df[,2:8])
print(pairs(scale(tempdf)))

tempdf <- data.frame(df[,8:14])
print(pairs(scale(tempdf)))

tempdf <- data.frame(df[,2:5],df[,8:11])
print(pairs(scale(tempdf)))
      
tempdf <- data.frame(df[,2:5],df[,11:14])
print(pairs(scale(tempdf)))
            
tempdf <- data.frame(df[,6:8],df[,8:11])
print(pairs(scale(tempdf)))
            
tempdf <- data.frame(df[,6:8],df[,11:14])
print(pairs(scale(tempdf)))

xyplot(df$Flavanoids ~ df$ColorIntensity, Data=df, groups = df$classID ,cex=2,  pch = 21 )
                  
#this doesnt really work!
PlotPairsInsequence(df)

#9 PCA for EDA
library(ggbiplot)
df.pca <- prcomp(df[,-1],
                 center = TRUE,
                 scale. = TRUE) 

g <- ggbiplot(df.pca, obs.scale = 1, var.scale = 1, 
              +               groups = df$classID, ellipse = TRUE, 
              +               circle = TRUE)
plot(g)

#MODELING

library("randomForest")
str(df)
df$classID <- as.factor(df$classID)
rf = randomForest(classID ~ ., data = df, mtry = 7, ntree = 500, importance = T)
importance(rf,type=1)
varImpPlot(rf)
partialPlot(rf, df, "Alcohol" )
partialPlot(rf, df, "MalicAcid" )
partialPlot(rf, df, "OD280OD315OfDilutedWines" )

#trying to find interaction
library(plotmo)
plotmo(rf)

#actual models 1, RF
library("randomForest")
str(df)
df$classID <- as.factor(df$classID)
df$scored.class <- NULL
df$class <- NULL
df2<-df

#model: RF (tuning tree number)
results = matrix(nrow=100,ncol=2) 
n<-1
for(tr in seq(from=2, to=200, by=2))
{
  rf = randomForest(classID ~ ., data = df, mtry = 7, ntree = tr, importance = T)
  
  df2$scored.class <- as.character(rf$predicted)
  df2$class <- as.character(df$classID)
  results[n,1] <- tr
  results[n,2] <- FAccuracy_multiclass(df2)
  n<-n+1
}

plot(results, xlab="trees", ylab="", main="#trees versus in-sample  (OOB) Accuracy, Random Forest (mtry=7)", ylim=c(0.9,1.01))
lines(results)

#model: RF (tuning mtry number)
results = matrix(nrow=13,ncol=3) 
n<-1
df$scored.class <- NULL
df$class <- NULL
df2<-df

for(mt in c(1,2,3,4,5,6,7,8,9,10,11,12,13))
{
  rf = randomForest(classID ~ ., data = df, mtry = mt, ntree = 100, importance = T)
  df2$scored.class <- as.character(rf$predicted)
  df2$class <- as.character(df$classID)
  results[n,1] <- mt
  results[n,2] <- FAccuracy_multiclass(df2)
  n<-n+1
}

plot(results, xlab="mtry", ylab="", main="mtry versus in-sample (OOB) accuracy, Random Forest (trees=100)", ylim=c(0.96,1))
lines(results)

#best RF here
rf = randomForest(classID ~ ., data = df, mtry = 1, ntree = 100, importance = T)
1-rf$err.rate[50]

#2 model: knn, note for report: scaling improves perofrmance significantly, presumably because it distorts distance less
library(class)
results = matrix(nrow=50,ncol=2) 
for(kn in 2:50)
{
  predknn <- knn(scale(df[training_index,2:14]), scale(df[-training_index,2:14]), df[training_index,1], k = kn)
  df3$scored.class <- predknn
  results[kn,1] <- kn
  results[kn,2] <- FAccuracy_multiclass(df3)
}

plot(results, xlab="k", ylab="Performance", main="K versus accuracy, KNN (scaled, euclidian distance)", ylim=c(0.90,1))
lines(results)

#3 MODEL -------- SVM

#set up training/test
training_index <- sample(nrow(df), round(nrow(df)*0.7))  
df3 <- df[-training_index,]
df3$class <- df3$classID

#model: support vector machine
library(e1071)
c <- 10
svmmodel <- svm(classID ~ ., data = df[training_index,], kernel="linear", cost = c, scale=FALSE)
svmpred <- predict(svmmodel, df[-training_index,])
df3$scored.class <- svmpred
q <- FAccuracy_multiclass(df3)
q

svmmodel2 <- svm(classID ~ ., data = df[training_index,], kernel="linear", cost = c, scale=TRUE)
svmpred2 <- predict(svmmodel2,  df[-training_index,])
df3$scored.class <- svmpred2
q <- FAccuracy_multiclass(df3)
q

svmmodel3 <- svm(classID ~ ., data = df[training_index,], kernel="polynomial", cost = c, scale=TRUE)
svmpred3 <- predict(svmmodel3,  df[-training_index,])
df3$scored.class <- svmpred3
q <- FAccuracy_multiclass(df3)
q

svmmodel4 <- svm(classID ~ ., data = df[training_index,], kernel="radial", cost = c, scale=TRUE)
svmpred4 <- predict(svmmodel4,  df[-training_index,])
df3$scored.class <- svmpred4
q <- FAccuracy_multiclass(df3)
q

svmmodel5 <- svm(classID ~ ., data = df[training_index,], kernel="sigmoid", cost = c, scale=TRUE)
svmpred5 <- predict(svmmodel5,  df[-training_index,])
df3$scored.class <- svmpred5
q <- FAccuracy_multiclass(df3)
q

#using the internal cross validation of SVM
svmtune <- tune(svm,as.numeric(classID) ~ ., data = df, kernel="linear", ranges = list(cost=c(0.01, 0.025, 0.05, 0.075, 0.1,0.2, 0.5)))

graphSVM <- summary(svmtune)$performances
plot(graphSVM[,1],1-graphSVM[,2] , xlab="Cost", ylab="CV Performance", main="SVM, linear - CV Performance verus Cost Parameter")
lines(graphSVM[,1],1-graphSVM[,2])
graphSVM

#best model
results = matrix(nrow=15,ncol=2) 
n<-1
for(co in c(0.01, 0.025, 0.05, 0.075, 0.1,0.2, 0.5,1))
{
  df$classID <- as.factor(df$classID)
  svmmodel6 <- svm(classID ~ ., data = df[training_index,], kernel="linear", cost = co, scale=TRUE)
  svmpred6 <- predict(svmmodel6,  df[-training_index,])
  df3$scored.class <- svmpred6
  results[n,1] <- co
  results[n,2] <- FAccuracy_multiclass(df3)
  n<-n+1
}

plot(results[,1],results[,2] , xlab="Cost", ylab="CV Performance", main="SVM, linear - CV Performance verus Cost Parameter")
lines(results[,1],results[,2])
results

df$classID <- as.factor(df$classID)
svmmodel6 <- svm(classID ~ ., data = df[training_index,], kernel="linear", cost = 10, scale=TRUE)
svmpred6 <- predict(svmmodel6,  df[-training_index,])
df3$scored.class <- svmpred6
q <- FAccuracy_multiclass(df3)
q

#baseline model 
baseline <- multinom(classID ~ ., data = df[training_index,])
write.csv(baseline$C)
pred10 <- predict(baseline,df[-training_index,])
df3$scored.class <- pred10
q <- FAccuracy_multiclass(df3)
q
baseline

#finally, Neural Network

library(neuralnet)  
library(nnet)
library(RSNNS)

target <- class.ind(df$classID)
#single layer perceptron
#train the neural net 

results <- matrix(nrow=27, ncol=2)
n<-1
for(s in seq(from=2, to=54, by=2))
{
NN <- nnet(df[,-1], target, size=s, softmax=TRUE)
#predict 
pred <- predict(NN, df[,-55], type="class")
df2$scored.class <- pred
results[n,1]<-s
results[n,2]<- FAccuracy_multiclass(df2)
n<-n+1
}

plot(results[,1],results[,2], ylim=c(0,1), ylab = "In Sample Performance", xlab="NN Nodes", main="Single Layer Perceptron performance, varying size")
lines(results[,1],results[,2])

#now using a multi-layer perceptron in order to see if we get better results  
#this takes a long time!
#MPL
library(monmlp)
df4<-df
#out of sample
df5 <- df4[training_index,]
df3 <- df4[-training_index,]

matrix4 <- data.matrix(df4[training_index,-1], rownames.force = NA)
matrix5 <- data.matrix(df4[training_index,1])
matrix6 <- data.matrix(df4[-training_index,-1])
class(matrix5)<-"numeric"

r <- monmlp.fit(matrix4,matrix5, hidden1=3,hidden2 = 8, n.ensemble=15, monotone=1, bag=TRUE)
z <- monmlp.predict(x = matrix6, weights = r)
z2 <- round(z)

df3$class <- df3$classID
df3$scored.class <- z2
q <- FAccuracy_multiclass(df3)
q





