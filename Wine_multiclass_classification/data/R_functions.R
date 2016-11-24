
library(fBasics)
#1 basic descriptive statistics

splitTrainingTestValidation <- function(dfr, trainingFraction, testFraction, validationFraction) {

  if (trainingFraction+testFraction+validationFraction !=1) {
    return(NULL)
  }    
  if (trainingFraction>0) {
    temp_index <- sample(nrow(df), round(nrow(df)*trainingFraction))  
    df_training <<- df[temp_index,]
    df_test <<- df[-temp_index,]
    rownames(df_training) <<- seq(length=nrow(df_training))
    rownames(df_test) <<- seq(length=nrow(df_test))
  }
  #split test
  if (validationFraction>0){
    temp_index <- sample(nrow(df_test), round(nrow(df_test)*(validationFraction/(testFraction+validationFraction))))  
    df_validation <<- df_test[temp_index,]
    df_test <<- df_test[-temp_index,]                 
    rownames(df_validation) <<- seq(length=nrow(df_validation))
    rownames(df_test) <<- seq(length=nrow(df_test))
  }
}

my.Summary <- function(dfr)
{
  # get the basic stats 
  NonZeroCount <- matrix(, nrow = 1, ncol = ncol(dfr))
  rownames(NonZeroCount) <- c("NonZeroCount")
  
  statsDF <- data.frame(basicStats(dfr[,1]))
  NonZeroCount[1,1] <- nrow(dfr[dfr[,1] != 0,])
  
  for(i in 2:ncol(dfr))
  {
    statsDF[,i] <- basicStats(dfr[,i])
    NonZeroRows <- nrow(dfr[dfr[,i] != 0,])
    NonZeroCount[1,i] <- NonZeroRows
  }
  colnames(statsDF) <- colnames(dfr)
  
  #add in the extra stats
  ExtraStatsDF <- get.extraPercentiles(dfr)
  colnames(ExtraStatsDF) <- colnames(statsDF)
  
  NonZeros <- as.data.frame(NonZeroCount)
  colnames(NonZeros) <- colnames(statsDF)
  
  statsDF <- rbind(statsDF, ExtraStatsDF)
  statsDF <- rbind(statsDF,NonZeros )
  
  return(statsDF)
}

#returns a percentile value from a DF, given column Index and fraction (eg 0.25)
get.Fraction <- function(df, colindex, percentFraction)
{
  count <- nrow(df)
  RowIndex <- ceiling(count*percentFraction)
  df <- df[order(df[,colindex]),]
  return(df[RowIndex,colindex])
}

#add in the additional Stats not provided by get basic stats by building an addition DF and then appending
get.extraPercentiles <- function(dfr){
  resultMatrix <- matrix(, nrow = 4, ncol = ncol(dfr))
  rownames(resultMatrix) <- c("0.01Percentile","0.05Percentile","0.95Percentile","0.99Percentile")
  for(i in 1:ncol(dfr))
  {
    z=1
    for(k in c(0.01,0.05,0.95,0.99))
    {
      resultMatrix   
      resultMatrix[z,i] <- get.Fraction(dfr, i, k)
      z = z+1
    }
  }
  return(as.data.frame(resultMatrix))
}

#returns basic stats of all variables in a DF
GetBasicStatsMatrix <- function(df)
{
  statsDF <- data.frame(basicStats(df[,1]))
  for(i in 2:ncol(df))
  {
    statsDF[,i] <- basicStats(df[,i])
  }
  colnames(statsDF) <- colnames(df)
  return(statsDF)
}

#plot QQ plots 
PLotQQPlots <- function(dfr)
{
  for(i in 1:ncol(dfr))
  {
    print(qqmath(~dfr[,i], f.value =  ppoints(100)))
    Sys.sleep(1)
  }
}

#get number of SDs from the mean
GetSDfromMean <- function(vector, threshold){
  sd <- sd(vector)
  mean <- mean(vector)
  sdsFromMean <- abs((vector - mean) / sd)
  sdsFromMean <- subset(sdsFromMean, sdsFromMean>threshold) 
  return(length(sdsFromMean))
}

GetALLSDfromMean <- function(dfr, threshold)
{
  returndf <- data.frame(names(dfr))
  returndf$SDCount <- "x"
  for(i in 1:ncol(dfr))
  {
    returndf[i,]$SDCount <- GetSDfromMean(dfr[,i],threshold)
  }
  return(returndf)
}


GetTotalSDsfromMean <- function(dfr)
{
  returndf <- data.frame(names(dfr))
  tempdf <- GetALLSDfromMean(dfr,3)
  returndf$sds3 <- tempdf$SDCount
  tempdf <- GetALLSDfromMean(dfr,3.5)
  returndf$sds35 <- tempdf$SDCount
  tempdf <- GetALLSDfromMean(dfr,4)
  returndf$sds4 <- tempdf$SDCount
  tempdf <- GetALLSDfromMean(dfr,5)
  returndf$sds5 <- tempdf$SDCount
  tempdf <- GetALLSDfromMean(dfr,6)
  returndf$sds6 <- tempdf$SDCount
  tempdf <- GetALLSDfromMean(dfr,7)
  returndf$sds7 <- tempdf$SDCount
  return(returndf)
}


#plotsa a basic vidoe of histograms

PlotHistograms <- function(dfr)
{
  for(i in 1:ncol(dfr))
  {
    print(i)
    print(hist(dfr[,i], main=colnames(dfr)[i]))
    Sys.sleep(1)
  }
}

PlotNonZeroHistograms <- function(dfr)
{
  for(i in 1:ncol(dfr))
  {
    print(i)
    print(hist(as.numeric(df[df[,i]!=0,i]), main=colnames(df)[i]))
    Sys.sleep(1)
  }
}

#Plot modified Histograms (unchanged + 3) 
PlotTransformedHistograms <- function(dfr)
{
  for(i in 1:ncol(dfr))
  {
    print(i)
    print(hist(dfr[,i], main=colnames(dfr)[i]))
    print(hist(log(dfr[,i]), main="Log Transform"))
    print(hist(dfr[,i]*dfr[,i], main="Square Transform"))
    print(hist(dfr[,i]*dfr[,i]*dfr[,i], main="Cube Transform"))
    print(hist(sqrt(dfr[,i]), main="Square Root Transform"))
    print(hist(log10(dfr[,i]), main="Log(10) Transform"))
    Sys.sleep(1)
  }
}


#plot conditional hisograms
PlotConditionalHistograms <- function(dfr)
{
  for(i in 1:ncol(dfr))
  {
    print(i)
    print(histogram(~ dfr[,i] | factor(classID), data = dfr ))
    Sys.sleep(1)
  }
}

library("lattice")

#plot conditional density plots
PlotConditionalDesityPlots <- function(dfr)
{
  for(i in 1:ncol(dfr))
  {
    print(i)
    print(densityplot(~ dfr[,i] | factor(spam), data = dfr ))
    Sys.sleep(1)
  }
}

#plot categorical counts
PlotCategoricalBarCharts <- function(dfr)
{
      dfr$count <- 1
      counter <- ncol(dfr)-1
      for(i in 1:counter)
        {
        df_sum <- aggregate(dfr$count,by=list(dfr[,i]),FUN=sum)
        df_sum <- df_sum[order(df_sum[,2]),]
        df_sum$Group.1 <- reorder(df_sum$Group.1, df_sum[,2])
        print(barchart(df_sum$Group.1 ~ df_sum$x, xlab="Count", ylab=names(dfr)[i]))
        Sys.sleep(1)
      }
}

#plot conditional density plots
PlotConditionalDesityPlotsGrouped <- function(dfr)
{
  for(i in 1:ncol(dfr))
  {
    print(i)
    print(densityplot(~ dfr[,i], group=spam, data = dfr ))
    Sys.sleep(1)
  }
}

#plot conditional density plots
PlotConditionalDesityPlotsGrouped <- function(dfr)
{
  for(i in 1:ncol(dfr))
  {
    png(filename=paste(colnames(dfr)[i], "ConditionalDplot.png"))
    print(densityplot(~ dfr[,i], group=spam, data = dfr,  auto.key=TRUE ))
    Sys.sleep(1)
    dev.off()
  }
}

#plot conditional density plots
PlotDesityPlots <- function(dfr)
{
  for(i in 1:ncol(dfr))
  {
    #png(filename=paste(colnames(dfr)[i], "ConditionalDplot.png"))
    print(densityplot(~ dfr[,i], data = dfr ))
    Sys.sleep(1)
    #dev.off()
  }
}

#plot pairs, big enought ot be visible 
PlotPairsInsequence <- function(dfr)
{
  for(i in 1:ncol(dfr)-4)
  {
    for(k in 1:ncol(dfr)-4){
      tempdf <- data.frame(dfr[,i],dfr[,i+1],dfr[,i+2],dfr[,i+3],dfr[,k],dfr[,k+1],dfr[,k+2],dfr[,k+3])
      print(pairs(scale(tempdf)))
      k=k+4;
      Sys.sleep(1)
    }
    i=i+4
  }
}

PlotPairsAgainstTarget <- function(targetVector,dfr)
{
  for(i in 1:ncol(dfr))
  {
    tempdf <- data.frame(targetVector,dfr[,i])
    print(plot(scale(tempdf)))
    Sys.sleep(1)
    i=i+1
  }
}

PlotTransformedPairsAgainstTarget <- function(targetVector,dependentVector)
{
    tempdf <- data.frame(targetVector,dependentVector)
    print(plot(scale(tempdf)), main="no change")
    print(cor(tempdf))
    #log-linear
    tempdf <- data.frame(log(targetVector),dependentVector)
    print(plot(scale(tempdf)), main="log linear")
    print(cor(tempdf))
    #linear-log
    tempdf <- data.frame(targetVector,log(dependentVector))
    print(plot(scale(tempdf)), main="linear log")
    print(cor(tempdf))
    #log-log
    tempdf <- data.frame(log(targetVector),log(dependentVector))
    print(plot(scale(tempdf)), main="log log")
    print(cor(tempdf))
    #log10-log10
    tempdf <- data.frame(log10(targetVector),log10(dependentVector))
    print(plot(scale(tempdf)), main="log10 log10")
    print(cor(tempdf))
    #linear-exp
    tempdf <- data.frame(targetVector,exp(dependentVector))
    print(plot(scale(tempdf)), main="linear exp")
    print(cor(tempdf))
    #linear-exp
    tempdf <- data.frame(exp(targetVector),dependentVector)
    print(plot(scale(tempdf)), main="exp linear")
    print(cor(tempdf))
    #linear-exp
    tempdf <- data.frame(exp(targetVector),exp(dependentVector))
    print(plot(scale(tempdf)), main="exp exp")
    print(cor(tempdf))
}



#plot conditional density plots
PlotConditionalXYPlotsGrouped <- function(dfr)
{
  for(i in 1:ncol(dfr))
  {
    print(i)
    #this not sure if it is right
    print(xyplot(~ dfr[,i] | factor(classID), data = dfr ))
    Sys.sleep(1)
  }
}


GetStats <- function(data1)
{
  TotalTruePositive <<- nrow(data1[data1$class== "1" & data1$scored.class == "1",])
  TotalFalsePositive <<- nrow(data1[data1$class== "0" & data1$scored.class == "1",])
  TotalFalseNegative <<- nrow(data1[data1$class== "1" & data1$scored.class == "0",])
  TotalTrueNegative <<- nrow(data1[data1$class== "0" & data1$scored.class == "0",])
}

FAccuracy_multiclass <- function(data1){
  TotalTrue <<- nrow(data1[data1$class== data1$scored.class,])
  FAccuracy_multiclass <- (TotalTrue) / (nrow(data1))
}


#question 3
FAccuracy <- function(data1){
  GetStats(data1)
  Result_Accuracy <- (TotalTruePositive + TotalTrueNegative) / (TotalTruePositive + TotalTrueNegative +TotalFalseNegative + TotalFalsePositive)
}

#question 4
FClassErrorRate <- function(data1){
  GetStats(data1)
  Result_ClassErrorRate <- (TotalFalseNegative + TotalFalsePositive) / (TotalTruePositive + TotalTrueNegative +TotalFalseNegative + TotalFalsePositive)
}

#question5 : Precision
FPrecision <- function(data1){
  GetStats(data1)
  Result_Precision <- TotalTruePositive / (TotalTruePositive + TotalFalsePositive)
}

#question6 : Sensitivity
FSensitivity <- function(data1){
  GetStats(data1)
  Result_Sensitivity <- TotalTruePositive / (TotalTruePositive + TotalFalseNegative)
}

#Question 7 : specificity
Fspecificity <- function(data1){
  GetStats(data1)
  Result_specificity <- TotalTrueNegative / (TotalTrueNegative + TotalFalsePositive)
}

#Question 8 : F1Score
FF1Score <- function(data1){
  GetStats(data1)
  Precision <- FPrecision(data1)
  Sensitivity <- FSensitivity(data1)
  Result_F1Score <- (2* Sensitivity*Precision) / (Sensitivity + Precision)
}

PrintPerformanceMetrics <- function(data1){
  print(paste("Accuracy:",FAccuracy(df_test)))
  print(paste("FClassErrorRate:",FClassErrorRate(df_test)))
  print(paste("Precision:",FPrecision(df_test)))
  print(paste("Sensitivity:",FSensitivity(df_test)))
}

#Question 9 : Bounds of the F1 Score
# since precision and sensitivity are both If 0<x<1, then precision*sensitivity < precision
# and precision*sensitivity < sensitivity, also
# it follows from this that 2*precision*sensitivity<precision+sensitivity, or this result is
# also 0<x<1

# Question 10 : generates an ROC curve
FRocCurve <- function(data1){
  Results <<- matrix(ncol = 3 , nrow = 100)
  count =0
  for(i in seq(from=1, to=0.01, by=-0.01)){
    count <- count+1
    for(k in 1:dim(data1)[1])  # for each row
    {
      if(data1$scored.probability[k]>=i){
        data1$scored.class[k] <- "1"
      }
      else  {data1$scored.class[k] <- "0"}
    }
    Results[count,1] <<- FSensitivity(data1)
    Results[count,2] <<- Fspecificity(data1)
    Results[count,3] <<- 0.01* Results[count,1]
  }
  plot(1- Results[,2], Results[,1], xlab="1-Specificity", ylab="Sensitivity" )
  lines(1- Results[,2], Results[,1] )
}

