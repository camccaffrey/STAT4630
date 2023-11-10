
#
# File: shrinkage.R
# Description: shrinkage methods for Milestone 4
#

# Load Dependencies ------------------------------------------------------------

# get global variables
source(file.path("Scripts", "config.R"))

# load libraries
library(tidyverse)
library(glmnet)
library(kableExtra)

# load data
train <- readRDS(TRAIN_PATH)
test <- readRDS(TEST_PATH)
data.dict <- read.csv(DICT_PATH)

# create display names
display <- setNames(data.dict$Display, data.dict$Alias)
names(display)[60] <- "usa1"

# Variable Selection -----------------------------------------------------------

# remove unwanted variables
exclude.shr <- c("city","country", "beer.rest.domestic", "beer.rest.imported",
                 "coffee", "soda", "water.rest", "taxi.km", "taxi.hr",
                 "rent1.outer", "rent3.center", "rent3.outer", "sqm.center",
                 "sqm.outer", "quality", "expensive")

exclude.test <- c("city","country", "quality", "expensive")

train.processed <- train %>% dplyr::select(!all_of(exclude.test))
test.processed <- train %>% dplyr::select(!all_of(exclude.test))


# Reformat Dataframes ----------------------------------------------------------

# combined
total <- rbind(train.processed, test.processed)
y.total <-total$salary
x.total <- model.matrix(salary~., data=total)[,-1]

# train and test
x.train <- model.matrix(salary~., data=train.processed)[,-1]
x.test <- model.matrix(salary~., data=test.processed)[,-1]
y.train <- train.processed$salary
y.test <- test.processed$salary


# Choosing Lambda --------------------------------------------------------------

ols.total <- lm(salary~.,data=total)
ridge.total <- glmnet::glmnet(x.total, y.total, alpha=0, lambda=0, thresh=1e-32)
cbind(coefficients(ols.total), coefficients(ridge.total))


# Ridge Regression -------------------------------------------------------------

# get optimal lambda
set.seed(SEED)
cv.ridge <- glmnet::cv.glmnet(x.train, y.train, nfolds=10, alpha=0, thresh=1e-32)
best.lam.ridge <- cv.ridge$lambda.min
best.lam.ridge

# plot lambda values
plot(cv.ridge)
xticks <- seq(0, 10, by=0.2)
axis(1, at=xticks)

# create model
model.ridge <- glmnet::glmnet(x.train, y.train, alpha=0, lambda=best.lam.ridge,
                              thresh=1e-32)
options(scipen = 999)
coef.ridge <- coefficients(model.ridge)
df.ridge <- data.frame(
  var=rownames(coef.ridge),
  coef=round(as.numeric(coef.ridge), 3)
)

# Order coefficients by absolute value in descending order
df.ridge <- df.ridge[order(-abs(df.ridge$coef)), ]

df.ridge.high <- df.ridge[1:28, ]
df.ridge.low <- df.ridge[29:56, ]

rownames(df.ridge.high) <- NULL
rownames(df.ridge.low) <- NULL

df.ridge.high %>%
  kbl(caption = "Ridge Coefficients") %>%
  kable_classic(full_width = FALSE, html_font = "Times New Roman")

df.ridge.low %>%
  kbl(caption = "Ridge Coefficients") %>%
  kable_classic(full_width = FALSE, html_font = "Times New Roman")

# calculate test MSE
ridge.pred <- predict(model.ridge, newx=x.test)
ridge.mse <- mean((ridge.pred - y.test)^2)
ridge.mse


# Lasso Regression -------------------------------------------------------------

# get optimal lambda
set.seed(4630)
cv.lasso <- glmnet::cv.glmnet(x.train, y.train, nfolds=10, alpha=1, thresh=1e-23)
best.lam.lasso <- cv.lasso$lambda.min
best.lam.lasso

# plot lambda values
plot(cv.lasso)
xticks <- seq(0, 10, by=0.2)
axis(1, at=xticks)

# create model
model.lasso <-glmnet::glmnet(x.train, y.train, alpha=1, lambda=best.lam.lasso,
                             thresh=1e-32)
options(scipen = 999)
coef.lasso <-coef(model.lasso)
df.lasso <- data.frame(
  var=rownames(coef.lasso),
  coef=round(as.numeric(coef.lasso),3)
)

# Order coefficients by absolute value in descending order
df.lasso <- df.lasso[order(-abs(df.lasso$coef)), ]

df.lasso.high <- df.ridge[1:28, ]
df.lasso.low <- df.ridge[29:56, ]

rownames(df.lasso.high) <- NULL
rownames(df.lasso.low) <- NULL

df.lasso.high %>%
  kbl(caption = "Lasso Coefficients") %>%
  kable_classic(full_width = FALSE, html_font = "Times New Roman")

df.lasso.low %>%
  kbl(caption = "Lasso Coefficients") %>%
  kable_classic(full_width = FALSE, html_font = "Times New Roman")

# calculate test MSE
lasso.pred <- predict(model.lasso, newx=x.test)
lasso.mse <- mean((lasso.pred - y.test)^2)
lasso.mse


# Comparison with OLS ----------------------------------------------------------

model.ols <- glmnet::glmnet(x.train, y.train, alpha=0, lambda=0, thresh=1e-32)
ols.pred <- predict(model.ols, newx=x.test)
ols.mse <- mean((ols.pred - y.test)^2)
ols.mse

