library(GA)

setwd("~/Downloads/PA454/3_week")
df <- read.csv("two_months_salary.csv")
#row count
nrow(df)

temp_index <- sample(nrow(df), round(nrow(df)*0.3))  
df_training <- df[-temp_index,]
df_test <- df[temp_index,]
rownames(df_test) <- seq(length=nrow(df_test))
rownames(df_training) <- seq(length=nrow(df_training))

#GA function for variable selection
mod <- lm(price ~ ., data = df_training)
summary(mod)

x <- model.matrix(mod)[, -1]
y <- model.response(model.frame(mod))

fitness <- function(string) {
  inc <- which(string == 1)
  X <- cbind(1, x[,inc])
  mod <- lm(price~X, data=df_training)
  pred <- predict(mod, df_training)  #this is IN SAMPLE RMSE, but should be using df_test, but it doesnt seem to work
  df_training$scored <-pred
  MSE <- sqrt(sum((df_training$scored-df_training$price)^2)/nrow(df_training))
  class(mod) <- "lm"
  return(-MSE)
}

GA <- ga("binary", fitness = fitness, nBits = ncol(x),names = colnames(x), monitor = plot)

plot(GA)
summary(GA)
#initial population

#convergence check

#selection

#crossover

#mutation

