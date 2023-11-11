
#
# File: regression_tree.R
# Description: regression tree for Milestone 4
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
exclude.reg <- c("city", "country", "beer.rest.domestic", "beer.rest.imported",
             "coffee", "soda", "water.rest", "taxi.km", "taxi.hr",
             "rent1.center", "rent1.outer", "rent3.center", "rent3.outer",
             "sqm.center", "sqm.outer", "quality")

exclude.test <- c("city","country", "quality", "expensive")

train.processed <- train %>% dplyr::select(!all_of(exclude.test))
test.processed <- test %>% dplyr::select(!all_of(exclude.test))


# Recursive Binary Splitting ---------------------------------------------------

# create tree
tree.reg <- tree::tree(salary ~ ., data = train.processed)
summary(tree.reg)

# plot tree
plot(tree.reg)
text(tree.reg, cex=0.75, pretty=0)

# calculate test MSE
tree.reg.pred <- predict(tree.reg, newdata=test.processed) 
tree.reg.mse <- mean((test.processed$salary - tree.reg.pred)^2)
cat("Recursive Binary Test MSE:", tree.reg.mse)


# Pruning ----------------------------------------------------------------------

# use 10-fold CV to prune tree
set.seed(SEED)
cv.reg.10 <- tree::cv.tree(tree.reg, K=10)
cv.reg.10

# plot of residual mean deviance vs size of tree with pruning
plot(cv.reg.10$size, cv.reg.10$dev, type="b",
     xlab="Size of Tree",
     ylab="Res Mean Deviance")

# see size of tree which gives best tree based on pruning and 10-fold CV
prune.reg.num <- cv.reg.10$size[which.min(cv.reg.10$dev)]
prune.reg.num

# create pruned tree
prune.reg <- tree::prune.tree(tree.reg, best=prune.reg.num)
prune.reg
summary(prune.reg)

# decision tree with pruning, with training data
plot(prune.reg)
text(prune.reg, cex=0.75, pretty=0)

# calculate test MSE
prune.reg.pred <- predict(prune.reg, newdata=test.processed) 
prune.reg.mse <- mean((test.processed$salary - prune.reg.pred)^2)
cat("Pruned Tree Test MSE:", prune.reg.mse)


# Random Forest ----------------------------------------------------------------

# create random forest, mtry = p/3 for regression tress (see Lab 8b)
set.seed(4630)
rf.reg <- randomForest::randomForest(salary ~ ., data=train.processed,
                                     mtry=18,importance=TRUE)
rf.reg

# variable importance
importance(rf.reg)
varImpPlot(rf.reg)

# calculate test MSE
rf.reg.pred <- predict(rf.reg, newdata=test.processed)
rf.reg.mse <- mean((test.processed$salary - rf.reg.pred)^2)
cat("Random Forest Test MSE:", rf.reg.mse)


# Comparing Performance --------------------------------------------------------

cat("Recursive Binary Test MSE:", tree.reg.mse, "\n",
    "Pruned Test MSE:", prune.reg.mse, "\n",
    "Random Forest Test MSE:", rf.reg.mse)
