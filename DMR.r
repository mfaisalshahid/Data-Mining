library(tidyverse)
library(PerformanceAnalytics)
library(caret)
housing_price <- read.csv("/Users/muhammadshahid/Desktop/ASS05_Data.csv")
df <- data.frame(housing_price)
# Part1
part1 <- function(df, p){
  n <- nrow(df)
  shuffled_df <- df[sample(n), ]
  train_indices <- 1:round(p * n)
  train <- shuffled_df[train_indices, ]
  test_indices <- (round(p * n) + 1):n
  test <- shuffled_df[test_indices, ]
  lin_model_1 <- lm(SalePrice ~ ., data = train)
  mse <- mean(lin_model_1$residuals^2)
  rsq <- summary(lin_model_1)$r.squared
  arsq <- summary(lin_model_1)$adj.r.squared
  pred <- predict(lin_model_1, test)
  ase <- mean((pred-test$SalePrice)**2)
  return(c(mse, rsq, arsq, ase))
}
for (i in 1:5){
  print(part1(df, 0.7))
}
# Part2
sets <- split(df, sample(1:1460, 5, replace=F))
folds_test <- c()
folds_train <- c()
for (i in 1:length(sets)){
  folds_test[i] <- list(as.data.frame(sets[i]))
  folds_train[i] <- list(as.data.frame(do.call(rbind, sets[-i])))
}
for (i in 1:5){
  train <- as.data.frame(folds_train[i])
  test <- as.data.frame(folds_test[i])
  columnnames <- c("LotArea","TotalBsmtSF","GarageCars","SalePrice","AGE","TotalArea")
  colnames(test) <- columnnames
  colnames(train) <- columnnames
  lin_model_1 <- lm(SalePrice ~ ., data = train)
  mse <- mean(lin_model_1$residuals^2)
  rsq <- summary(lin_model_1)$r.squared
  arsq <- summary(lin_model_1)$adj.r.squared
  pred <- predict(lin_model_1, test)
  ase <- mean((pred-test$SalePrice)**2)
  print(c(mse, rsq, arsq, ase))
  print (c(average(mse, rsq, arsq, ase)))
}