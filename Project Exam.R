setwd("~/Documents/UCF/Study/Semester 2/STA 5703 - Data Mining/Final.tmp")

library(tidyverse)
library(optmatch)
library(moments)
library(caTools)
library(InformationValue)
library(randomForest)
library(gbm)

set.seed(100)

data <- read_csv("./PHY_TRAIN.csv")

## Check bias in target
table(data$target)
# 0     1 
# 25139 24861 

# data$target <- as.factor(data$target)

# Find missing percentages
pMiss <- function(x){sum(is.na(x))/length(x)*100}
# apply(df,2,pMiss)
# apply(df,1,pMiss)

# Add missing indicator vars
# indicators <- fill.NAs(target ~ . - exampleid, data=df) %>%
#   select(ends_with(".NA")) %>%
#   mutate_all(list(as.numeric))

# Append indicator cols
# df <- cbind(df, indicators)

# Handles missing imputation
df <- fill.NAs(target ~ . - exampleid, data=data)

# Drop certain cols
drops <- c("feat29", "feat47", "feat48", "feat49", "feat50", "feat51", "feat55")
df <-df[, !names(df) %in% drops]

transformSkew <- function(x) {
  #print(names(df)[[x]])
  #x <- df[, x]
  skew <- skewness(x)
  if (skew > 1) {
     # if (min(x) >= 0) {
     #   return(log10(x))
     # }
    return(tanh(x))
  }
  if (skew < 0) {
    return(x^2)
  }
  return(x)
}

transformed <- df %>%
  select(!ends_with(".NA"), -target) %>%
  mutate_all(transformSkew) %>%
  cbind(
    select(df, ends_with(".NA"), target) %>% # Columns to add to
    mutate_if(is.logical, as.numeric) # Convert NA bool to numeric
  )

#transformed <- cbind(transformed, select(df, ends_with(".NA"), target))



#################################################################
##                          Splitting                          ##

# Random sample indexes
train_index <- sample(1:nrow(transformed), 0.8 * nrow(transformed))
test_index <- setdiff(1:nrow(transformed), train_index)

# Build X_train, y_train, X_test, y_test
train <- transformed[train_index,]
test <- transformed[test_index,]


#################################################################
##                     Feature Selection                       ##

base.mod <- glm(target ~ 1 , data= train, family = binomial(link="logit"))  # base intercept only model
all.mod <- glm(target ~ . , data= train, family = binomial(link="logit")) # full model with all predictors
stepMod <- step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = "both", trace = 1, steps = 100)  # perform step-wise algorithm
shortlistedVars <- names(unlist(stepMod[[1]])) # get the shortlisted variable.
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"]  # remove intercept 
print(shortlistedVars)


#################################################################
##                     Logistic Regression                     ##


logitMod <- glm(target ~ ., data=train, family = binomial(link="logit"))
predicted <- plogis(predict(logitMod, test))

optCutOff <- optimalCutoff(test$target, predicted)[1]
accuracy_logit <- 1 - misClassError(test$target, predicted, threshold = optCutOff)
#summary(logitMod)


#################################################################
##             Logistic Regression w/ Interactions             ##

logitMod <- glm(target ~ . + feat5*feat10 + feat4*feat20 + feat9*feat30 , data=train, family = binomial(link="logit"))
predicted <- plogis(predict(logitMod, test))

optCutOff <- optimalCutoff(test$target, predicted)[1]
accuracy_w_int <- 1 - misClassError(test$target, predicted, threshold = optCutOff)
#summary(logitMod)


##################################################################
##                        Random Forrest                        ##

extract_x <- function(dataset) { dataset[, colnames(dataset) != "target"] }
extract_y <- function(dataset) { train[, "target"] }

rf.train_x <- extract_x(train)
rf.train_y <- as.factor(extract_y(train))
rf.test_x <- extract_x(test)
rf.test_y <- as.factor(extract_y(test))
ranForMod <- randomForest(
  rf.train_x,
  rf.train_y,
  #xtest=rf.test_x,
  #ytest=rf.test_y,
  ntree=501,
  do.trace = TRUE
  
)
predicted_rf <- predict(ranForMod, test)
mean(predicted_rf == test$target)
plotROC(test$target, as.numeric(predicted_rf))


##################################################################
##                        Gradient Boosting                     ##

gbModel <- gbm(target ~ ., data=train, verbose=TRUE)
predicted <- predict(gbModel, test, n.trees=100)
#plotROC(test$target, predicted)









