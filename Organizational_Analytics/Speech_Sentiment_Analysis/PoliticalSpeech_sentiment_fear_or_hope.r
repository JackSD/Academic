
# install these packages before bringing them in by library()
library(tm)  # text mining and document management
library(stringr)  # character manipulation with regular expressions
library(grid)  # grid graphics utilities
library(ggplot2)  # graphics
library(latticeExtra) # package used for text horizon plot
library(caret)  # for confusion matrix function
library(rpart)  # tree-structured modeling
library(e1071)  # support vector machines
library(randomForest)  # random forests
library(rpart.plot)  # plot tree-structured model information


# read in positive and negative word lists 
positive.data.frame <- read.csv(file = "hope_words.csv",
                                  header = TRUE)
negative.data.frame <- read.csv(file = "fear_words.csv",
                                  header = TRUE)  

#add the speech to be analyzed here
speech = read.table(file = "HILLARY_1.txt")

#word counts for each of the vocab associated with hope/fear



#count how many questions asked in each speech
tofind <- '?'



#looking at topics associted with fear: immigration, china
topics <- data.matrix("China","Immigration")



#finally, score the speech, and compare the the benchmarks of the TRUMP and OBAMA speechs which I used
# to train the model




