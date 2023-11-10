
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
exclude.cls <- c("city", "country", "beer.rest.domestic", "beer.rest.imported",
             "coffee", "soda", "water.rest", "taxi.km", "taxi.hr",
             "rent1.center", "rent1.outer", "rent3.center", "rent3.outer",
             "sqm.center", "sqm.outer", "quality")

exclude.test <- c("city","country", "quality", "rent1.center", "rent1.outer",
                  "rent3.center", "rent3.outer", "sqm.center", "sqm.outer")

train.processed <- train %>% dplyr::select(!all_of(exclude.test))
test.processed <- train %>% dplyr::select(!all_of(exclude.test))


# Recursive Binary Splitting ---------------------------------------------------

# create tree
tree.cls <- tree::tree(expensive~., data=train.processed)
summary(tree.cls)

# plot tree
plot(tree.cls)
text(tree.cls, cex=0.6, pretty=0)

# predictions
tree.cls.pred <- predict(tree.cls, newdata=test.processed, type="class")
tree.cls.probs <- predict(tree.cls, newdata=test.processed)

# confusion matrix for test data
tree.cls.conf.d <- table(test.processed$expensive, tree.cls.pred)
tree.cls.conf.d

tree.thresh <- 0.55
tree.cls.conf.c <- table(test.processed$expensive, tree.cls.probs[,2] > tree.thresh)
tree.cls.conf.c

# calculate test accuracy
tree.cls.acc.d <- mean(tree.cls.pred == test.processed$expensive)
tree.cls.acc.d

tree.cls.acc.c <- mean(as.numeric(tree.cls.probs[,2] > tree.thresh) == test.processed$expensive)
tree.cls.acc.c




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
prune.cls.pred <- predict(prune.cls, newdata=test.processed, type="class")
prune.cls.probs <- predict(prune.cls, newdata=test.processed)

# confusion matrix for test data
prune.cls.conf.d <- table(test.processed$expensive, prune.cls.pred)
prune.cls.conf.d

prune.thresh <- 0.60
prune.cls.conf.c <- table(test.processed$expensive, prune.cls.probs[,2] > prune.thresh)
prune.cls.conf.c

# calculate test accuracy
prune.cls.acc.d <- mean(prune.cls.pred == test.processed$expensive)
prune.cls.acc.d

prune.cls.acc.c <- mean(as.numeric(prune.cls.probs[,2] > prune.thresh) == test.processed$expensive)
prune.cls.acc.c


# Random Forest ----------------------------------------------------------------

set.seed(4630)
rf.cls <- randomForest::randomForest(expensive~., data=train.processed, mtry=7, importance=TRUE)
rf.cls

# see variable importance
round(importance(rf.cls),2)
varImpPlot(rf.cls)

# predictions
rf.cls.pred <- predict(rf.cls, newdata=test.processed, type="class")
rf.cls.probs <- predict(rf.cls, newdata=test.processed, type="prob")

# confusion matrix for test data
rf.cls.conf.d <- table(test.processed$expensive, rf.cls.pred)
rf.cls.conf.d

rf.thresh <- 0.60
rf.cls.conf.c <- table(test.processed$expensive, rf.cls.probs[,2] > rf.thresh)
rf.cls.conf.c

# calculate test accuracy
rf.cls.acc.d <- mean(rf.cls.pred == test.processed$expensive)
rf.cls.acc.d

rf.cls.acc.c <- mean(as.numeric(rf.cls.probs[,2] > rf.thresh) == test.processed$expensive)
rf.cls.acc.c


# Comparing Performance --------------------------------------------------------

cat("Recursive Binary Test Accuracy:", tree.cls.acc.d, tree.cls.acc.c, "(", tree.thresh, ")\n",
    "Pruned Test Accuracy:", prune.cls.acc.d, prune.cls.acc.c, "(", prune.thresh, ")\n",
    "Random Forest Test Accuracy:", rf.cls.acc.d, rf.cls.acc.c, "(", rf.thresh, ")")


