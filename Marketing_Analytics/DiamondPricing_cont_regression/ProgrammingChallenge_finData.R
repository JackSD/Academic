library("fBasics")
setwd("~/Downloads/PA454/3_week")
df <- read.csv("jpm.csv", header = TRUE)
#1. date appears to be of type integer, when casted, double?! 
# I guess this is as dates are stored as number of days since a base (1/1/1970?)
typeof(df$Date)
str(df)
#2. casting the date 
df$Date <- as.Date(df$Date, "%d-%b-%Y")
#3. create year-mnth variable
df$YearMonth <- paste(format(df$Date,"%Y"), format(df$Date,"%m"), sep ="")
#4 trading volume per month
df_volume <- aggregate(df$Volume,by=list(df$YearMonth),FUN=sum)
#5 average closing
df_closing <- aggregate(df$Close,by=list(df$YearMonth),FUN=mean)
#6 average spread
df_spread <- aggregate(abs(df$Close-df$Open),by=list(df$YearMonth),FUN=mean)
#7 box and wisker chart
boxplot(abs(df$Close-df$Open) ~ df$YearMonth, data = df, las=3)