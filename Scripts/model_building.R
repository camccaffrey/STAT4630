
#
# File: model_building.R
# Description: model building for Milestone 3
#

# Load Dependencies ------------------------------------------------------------

# get global variables
source(file.path("Scripts", "config.R"))

# load libraries
library(tidyverse)
library(ROCR)
library(MASS)
library(boot)
library(ipred)
library(ICS)

# load data
data.dict <- read.csv(DICT_PATH)

train <- readRDS(TRAIN_PATH)
train.long <- readRDS(TRAINLONG_PATH)

test <- readRDS(TEST_PATH)
test.long <- readRDS(TESTLONG_PATH)

# create display names
display <- setNames(data.dict$Display, data.dict$Alias)
rev.display <- setNames(data.dict$Alias, data.dict$Display)


# Variable Selection -----------------------------------------------------------

COLS1 <- c("expensive", "meal1", "bread", "eggs", "wine", "gas", "basic", "cinema", "salary")

train.sub <- train %>%
  dplyr::select(all_of(COLS1)) %>%
  na.omit()

test.sub <- test %>%
  dplyr::select(all_of(COLS1)) %>%
  na.omit()


# Logistic Classification ------------------------------------------------------

logistic <-glm(expensive ~., family=binomial, data=train.sub)
summary(logistic)

logistic.preds <- predict(logistic, newdata = test.sub, type = "response")

logistic.rates <- ROCR::prediction(logistic.preds, test.sub$expensive)

logistic.roc <- ROCR::performance(logistic.rates, measure="tpr", x.measure="fpr")
plot(logistic.roc, main="ROC Curve for Logistic Regression")


# LDA Classification -----------------------------------------------------------


# check assumptions
cheap <- train.sub[which(train.sub$expensive == 0),]
expensive <- train.sub[which(train.sub$expensive == 1),]
ICS::mvnorm.kur.test(cheap[, 2:8]) # error when 2:9?
ICS::mvnorm.kur.test(expensive[, 2:9])
ICS::mvnorm.skew.test(cheap[, 2:9])
ICS::mvnorm.skew.test(expensive[, 2:9])

LDA <- MASS::lda(expensive ~., data=train.sub)
LDA

LDA.preds <- predict(LDA, test.sub)

LDA.rates <- ROCR::prediction(LDA.preds$posterior[,2], test.sub$expensive)

LDA.roc <- ROCR::performance(LDA.rates, measure="tpr", x.measure="fpr")
plot(LDA.roc, main="ROC Curve for LDA")


# Area Under the ROC -----------------------------------------------------------

logistic.perf.auc <- ROCR::performance(logistic.rates, measure = "auc")
logistic.auc <- logistic.perf.auc@y.values

cat("The AUC of logistic regression is", logistic.auc[[1]], "\n")

LDA.perf.auc <- ROCR::performance(LDA.rates, measure = "auc")
LDA.auc <- LDA.perf.auc@y.values
cat("The AUC of LDA is", LDA.auc[[1]], "\n")


# Cross Validation -------------------------------------------------------------

set.seed(SEED)
logistic.5CV <- boot::cv.glm(train.sub, logistic, K=5) 
logistic.10CV <- boot::cv.glm(train.sub, logistic, K=10) 

cat("The test error of 5-fold CV of Logistic Regression is", logistic.5CV$delta[1], "\n")
cat("The test error of 10-fold CV of Logistic Regression is", logistic.10CV$delta[1], "\n")

cv <- function(object, newdata){
  return(predict(object, newdata = newdata)$class)
}

set.seed(4630)
LDA.5CV <- ipred::errorest(expensive ~. , data=train.sub, model=MASS::lda,
                           estimator="cv",
                           est.para=control.errorest(k=5),
                           predict=cv)$err

LDA.10CV <- ipred::errorest(expensive ~ ., data=train.sub, model=MASS::lda,
                            estimator="cv",
                            est.para=control.errorest(k=10),
                            predict=cv)$err


cat("The test error of 5-fold CV of LDA is", LDA.5CV, "\n")
cat("The test error of 10-fold CV of LDA is", LDA.10CV, "\n")


# Error Rate on Test Set -------------------------------------------------------

label <- as.numeric(logistic.preds > 0.8)
logistic.acc <- mean(test.sub$expensive == label)
cat("The test error of logistic regression is", 1 - logistic.acc, "\n")

LDA.acc <- mean(test.sub$expensive == LDA.preds$class)
cat("The test error of LDA is", 1 - LDA.acc, "\n")


# Improvement (Logistic) -------------------------------------------------------

logistic.reduced <- glm(expensive ~ meal1 + bread + cinema + salary, family = binomial, data = train.sub)
summary(logistic.reduced)

TS <- logistic.reduced$dev - logistic$dev

p.val <- 1-pchisq(TS,4)
p.val

# ROC Curve
logistic.reduced.preds <-predict(logistic.reduced, newdata=test.sub, type="response")
logistic.reduced.rates <-ROCR::prediction(logistic.reduced.preds, test.sub$expensive)
logistic.reduced.roc <- ROCR::performance(logistic.reduced.rates, measure="tpr", x.measure="fpr")
plot(logistic.reduced.roc, main="ROC Curve for Logistic Regression (Reduced)")


logistic.reduced.perf.auc <- ROCR::performance(logistic.reduced.rates, measure = "auc")
logistic.reduced.auc <- logistic.reduced.perf.auc@y.values
logistic.reduced.auc


set.seed(SEED)
logistic.reduced.5CV <- boot::cv.glm(train.sub, logistic.reduced, K=5) 
logistic.reduced.10CV <- boot::cv.glm(train.sub, logistic.reduced, K=10) 

cat("The test error of 5-fold CV of Logistic Regression is", logistic.reduced.5CV$delta[1], "\n")
cat("The test error of 10-fold CV of Logistic Regression is", logistic.reduced.10CV$delta[1], "\n")


label.reduced <- as.numeric(logistic.reduced.preds > 0.70)
logistic.reduced.acc <- mean(test.sub$expensive == label.reduced)
cat("The test error of logistic regression is", 1 - logistic.reduced.acc, "\n")


# Discussion of summary() Output -----------------------------------------------

summary(logistic)
summary(logistic.reduced)


# Discussion of lda() Output

LDA





