
#
# File: classification_tree.R
# Description: classification tree for Milestone 4
#

# Load Dependencies ------------------------------------------------------------

# get global variables
source(file.path("Scripts", "config.R"))

# load libraries
library(tidyverse)
library(MASS)
library(tree)
library(randomForest)
library(gbm)

# load data
train <- readRDS(TRAIN_PATH)
test <- readRDS(TEST_PATH)
data.dict <- read.csv(DICT_PATH)


# Variable Selection -----------------------------------------------------------

# remove unwanted variables
exclude <- c("city", "country", "beer.rest.domestic", "beer.rest.imported",
             "coffee", "soda", "water.rest", "taxi.km", "taxi.hr",
             "rent1.center", "rent1.outer", "rent3.center", "rent3.outer",
             "sqm.center", "sqm.outer", "quality")

train.processed <- train %>% dplyr::select(!all_of(exclude))
test.processed <- train %>% dplyr::select(!all_of(exclude))


# Recursive Binary Splitting ---------------------------------------------------

# create tree
tree.cls <- tree::tree(expensive~., data=train.processed)
summary(tree.cls)

# plot tree
plot(tree.cls)
text(tree.cls, cex=0.6, pretty=0)

# predictions
tree.cls.pred <- predict(tree.cls, newdata=test, type="class")
tree.cls.probs <- predict(tree.cls, newdata=test)

# confusion matrix for test data
tree.cls.conf.5 <- table(test$expensive, tree.cls.pred)
tree.cls.conf.5

tree.cls.conf.7 <- table(test$expensive, tree.cls.probs[,2] > 0.7) # threshold = 0.7
tree.cls.conf.7

# calculate test accuracy
tree.cls.acc.5 <- mean(tree.cls.pred == test$expensive)
tree.cls.acc.5

tree.cls.acc.7 <- mean(as.numeric(tree.cls.probs[,2] > 0.7) == test$expensive)
tree.cls.acc.7




# Pruning ----------------------------------------------------------------------

set.seed(4630)
cv.cls.10 <- tree::cv.tree(tree.cls, K=10, FUN=prune.misclass) 
cv.cls.10

# plot of dev against size
plot(cv.cls.10$size, cv.cls.10$dev,type='b')

# size of tree chosen by pruning
prune.cls.num <- cv.cls.10$size[which.min(cv.cls.10$dev)]
prune.cls.num #9 and 5 have same dev??

# fit tree with size chosen by pruning
prune.cls <-tree::prune.misclass(tree.cls, best=5)
prune.cls

# plot pruned tree
plot(prune.cls)
text(prune.cls, cex=0.75, pretty=0)

# predictions
prune.cls.pred <- predict(prune.cls, newdata=test, type="class")
prune.cls.probs <- predict(prune.cls, newdata=test)

# confusion matrix for test data
prune.cls.conf.5 <- table(test$expensive, prune.cls.pred)
prune.cls.conf.5

prune.cls.conf.7 <- table(test$expensive, prune.cls.probs[,2] > 0.7) # threshold = 0.7
prune.cls.conf.7

# calculate test accuracy
prune.cls.acc.5 <- mean(prune.cls.pred == test$expensive)
prune.cls.acc.5

prune.cls.acc.7 <- mean(as.numeric(prune.cls.probs[,2] > 0.7) == test$expensive)
prune.cls.acc.7


# Random Forest ----------------------------------------------------------------

set.seed(4630)
rf.cls <- randomForest::randomForest(expensive~., data=train.processed, mtry=6, importance=TRUE) ## unsure about mtry number
rf.cls

# see variable importance
round(importance(rf.cls),2)
varImpPlot(rf.cls)

# predictions
rf.cls.pred <- predict(rf.cls, newdata=test, type="class")
rf.cls.probs <- predict(rf.cls, newdata=test, type="prob")

# confusion matrix for test data
rf.cls.conf.5 <- table(test$expensive, rf.cls.pred)
rf.cls.conf.5

rf.cls.conf.7 <- table(test$expensive, rf.cls.probs[,2] > 0.7) # threshold = 0.7
rf.cls.conf.7

# calculate test accuracy
rf.cls.acc.5 <- mean(rf.cls.pred == test$expensive)
rf.cls.acc.5

rf.cls.acc.7 <- mean(as.numeric(rf.cls.probs[,2] > 0.7) == test$expensive)
rf.cls.acc.7


# Comparing Performance --------------------------------------------------------

cat("Recursive Binary Test Accuracy:", tree.cls.acc.5, tree.cls.acc.7, "\n",
    "Pruned Test Accuracy:", prune.cls.acc.5, prune.cls.acc.7, "\n",
    "Random Forest Test Accuracy:", rf.cls.acc.5, rf.cls.acc.7)


