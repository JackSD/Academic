
library(sqldf)
library(fBasics)
library(corrplot) 
library(ggplot2)
library(randomForest)
library(gbm)
library(leaps)
library(e1071)
library(gam)
library(kknn) #knn
library(RSNNS) #pnn
library(glmnet) 
library(pls)
library(neuralnet) 
library(nnet) 

# PREDICT 422 Practical Machine Learning

# Course Project - Example R Script File

# OBJECTIVE: A charitable organization wishes to develop a machine learning
# model to improve the cost-effectiveness of their direct marketing campaigns
# to previous donors.

# 1) Develop a classification model using data from the most recent campaign that
# can effectively capture likely donors so that the expected net profit is maximized.

# 2) Develop a prediction model to predict donation amounts for donors - the data
# for this will consist of the records for donors only.

# load the data
charity <- read.csv("charity.csv") # load the "charity.csv" file

#recompose the region variable into a single predictor so it can be analyzed
charity$region <- "0"
charity[charity$reg1 == 1, ][, "region"] <- "1"
charity[charity$reg2 == 1, ][, "region"] <- "2"
charity[charity$reg3 == 1, ][, "region"] <- "3"
charity[charity$reg4 == 1, ][, "region"] <- "4"

charityTrain <- charity[charity$part == "train" ,]

# general method for continuous
par(mfrow=c(3,3))

#initialize basicStats object
basicStatsLists <- basicStats(charity$damt[charity$part=="train"])
colnames(basicStatsLists)[ncol(basicStatsLists)] <- "damt"

EDA_continuous <- function(dataframe, variable, vecVariable){
  
  #compiling basic stats
  x <- basicStats(vecVariable)
  basicStatsLists[,ncol(basicStatsLists)+1] <<- x[,1]
  colnames(basicStatsLists)[ncol(basicStatsLists)] <<- variable
  basicStatsLists <<- format(basicStatsLists,scientific=FALSE) 
  
  #histograms
  hist(vecVariable, main=variable)
}


EDA_categorical <- function(dataframe, variable, vecVariable){
  x<-sqldf(paste("select ", variable , ", count(id) from " , dataframe, "GROUP BY", variable, " ORDER BY count(id)"));  
  print(paste("------",variable,"------"))
  print(x)
  counts <- table(vecVariable)
  a = counts[order(counts)]
  barplot(a, main=variable)
  return(x)
}



EDA_categorical("charity", "region", charity$region)

Catvarnames <- names(charity)[names(charity) %in% c("donr","reg1","reg2","reg3","reg4","home","genf","region")]
Contvarnames <- names(charity)[!names(charity) %in% c("ID","donr","part","reg1","reg2","reg3","reg4","home","genf","region")]

for (i in 1:length(Contvarnames)) {
  y <- EDA_continuous("charity",Contvarnames[i], charity[,Contvarnames[i]][charity$part=="train"])
}

#full dataset
for (i in 1:length(Contvarnames)) {
  y <- EDA_continuous("charity",Contvarnames[i], charity[,Contvarnames[i]])
}

for (i in 1:length(Contvarnames)) {
  y <- EDA_continuous("charity",Contvarnames[i], log(charity[,Contvarnames[i]][charity$part=="train"]))
}

for (i in 1:length(Catvarnames)) {
  y <- EDA_categorical("charity",Catvarnames[i], charity[,Catvarnames[i]][charity$part=="train"])
}

#scanning for missing variables

for (i in 1:length(Contvarnames)) {
  y <- charity[is.na(charity[,Contvarnames[i]]),]
  if (nrow(y)>0){
    print(paste("missing variables found in ",Contvarnames[i]))
  } 
}

for (i in 1:length(Catvarnames)) {
  y <- charity[is.na(charity[,Catvarnames[i]]),]
  if (nrow(y)>0){
    print(paste("missing variables found in ",Catvarnames[i]))
  } 
}

for (i in 1:length(Contvarnames)) {
  y <- charity[charity[,Contvarnames[i]] < 0,]
  if (nrow(y)>0){
    print(paste("< 0 variables found in ",Contvarnames[i]))
  } 
}




#scanning for outliers (>3 SDs from the mean)

outfun <- function(column, dataframe, name, nostdDeviations) {
  mean.x <-  mean(column,na.rm=TRUE)
  sd.x <- sd(column,na.rm=TRUE)
  outliers <<- dataframe[abs(column-mean.x) > nostdDeviations*sd.x,]
  if (nrow(outliers)>0){
    print(paste(nrow(outliers), " outliers found for predictor ",name, " by std deviations:",nostdDeviations ))
  } 
}

for (i in 1:length(Contvarnames)) {
    outfun(charityTrain[,Contvarnames[i]], charityTrain ,Contvarnames[i], 3)
}


#scanning ofr non-sensical data amongst the gift data

# largest gift < most recent gift
charityrows <- nrow(charity)
Nonsense <- charity[charity$lgif <  charity$rgif ,]
100* nrow(Nonsense) /charityrows

# largest gift = total gifts != most recent gift or != average
Nonsense <- charity[charity$lgif == charity$rgif & charity$agif != charity$rgif,]
100* nrow(Nonsense) /charityrows

# largest gift < average gift
Nonsense <- charity[charity$lgif <  charity$agif ,]
100* nrow(Nonsense) /charityrows



#cross-predictor EDA
#corelations (continuous) 
par(mfrow=c(1,1))
cor(charityTrain[Contvarnames])
corrplot::corrplot(cor(charityTrain[Contvarnames]),method="number", type="lower",bg="lightblue", tl.srt=45)

#corelations squares 
charity_squares <- charityTrain
Contvarnames.squares <- names(charity)[!names(charity) %in% c("ID","donr","part","reg1","reg2","reg3","reg4","home","genf","region","chld","plow","wrat")]
for (i in 1:length(Contvarnames.squares )) {
  charity_squares[,Contvarnames.squares[i]] <-  (charity_squares[,Contvarnames.squares[i]])^2
}
cor(charity_squares[Contvarnames.squares])
corrplot::corrplot(cor(charity_squares[Contvarnames.squares]),method="number", type="lower",bg="lightblue", tl.srt=45)

#corelations logs
charity_logs <- charityTrain
Contvarnames.logs <- names(charity)[!names(charity) %in% c("ID","donr","part","reg1","reg2","reg3","reg4","home","genf","region","chld","plow","wrat")]
for (i in 1:length(Contvarnames.logs )) {
  charity_logs[,Contvarnames.logs[i]] <-  (charity_logs[,Contvarnames.logs[i]])^2
}
cor(charity_logs[Contvarnames.logs])
corrplot::corrplot(cor(charity_logs[Contvarnames.logs]),method="number", type="lower",bg="lightblue", tl.srt=45)

#corelations sqrt
charity_sqrt <- charityTrain
Contvarnames.sqrt <- names(charity)[!names(charity) %in% c("ID","donr","part","reg1","reg2","reg3","reg4","home","genf","region","chld","plow","wrat")]
for (i in 1:length(Contvarnames.sqrt )) {
  charity_sqrt[,Contvarnames.sqrt[i]] <-  (charity_sqrt[,Contvarnames.sqrt[i]])^0.5
}
cor(charity_sqrt[Contvarnames.sqrt])
corrplot::corrplot(cor(charity_sqrt[Contvarnames.sqrt]),method="number", type="lower",bg="lightblue", tl.srt=45)





qplot(x =  charity$tgif, y = charity$npro) 
qplot(x =  charity$tgif, y = charity$incm,  color = charity$genf, size = charity$chld) 
qplot(x =  charity$damt, y = charity$incm,  color = charity$genf, size = charity$chld) 

par(mfrow=c(3,3))
for (i in 1:length(Contvarnames)) {
  for (k in 1:length(Catvarnames)) {
      boxplot(charityTrain[,Contvarnames[i]]~charityTrain[,Catvarnames[k]],data=charityTrain, main=paste(Catvarnames[k]," ", Contvarnames[i]) )
    }
}

#looking at the sctter plot matrics 
#pairs(charity[,Contvarnames])
#pairs(charity[,Contvarnames])
pairs(charity[,c("npro","lgif","wrat","tgif")])
pairs(charity[,c("damt","lgif","incm","tgif")])
pairs(charity[,c("damt","tdon","tlag","incm")])
pairs(charity[,c("lgif","tdon","tgif","agif")])

counts <- table(charity$chld)
a = counts[order(counts)]

#outliers 2 (sitting in own cluster) - requires scatter plot exploration







