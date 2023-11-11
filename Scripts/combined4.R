
#
# File: combined4.R
# Authors: Group 13 for STAT 4630
# Description: combination of config.R, themes.R, data_cleaning.R, eda4.R,
#              shrinkage.R, classification_tree.R, and regression_tree.R.
#              Redundancies have been removed.
#

############################### config.R #######################################

# File Paths (OS Independent) --------------------------------------------------
  
# set paths to data
DATA_PATH <- "cost-of-living_v2.csv"
  
  
# Packages (RENV Alternative) --------------------------------------------------
  
  # required packages
  REQUIREMENTS <- c(
    "tidyverse",  # collection of data science packages
    "GGally",     # ggplot extension that helps combine objects
    "rlang",      # provides various interfaces for working with R and R objects
    "ggtext",     # provides simple Markdown and HTML rendering for ggplot2
    "viridis",    # provides the base functions for generating the color maps
    "glue",       # interpret string literals
    "rprojroot",   # finding files in project subdirectories
    "faraway",
    "kableExtra",
    "glmnet",
    "MASS",
    "tree",
    "randomForest",
    "gbm"
  )
  
  # load required packages (alternative to renv) 
  load_requirements <- function() {
    # check and install packages
    new.packages <- REQUIREMENTS[!(REQUIREMENTS %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    # load packages
    lapply(REQUIREMENTS, require, character.only = TRUE)
  }
  
  # run load_requirements()
  load_requirements()
  
# Test / Train Split -----------------------------------------------------------
  
  # random seed for reproducibility
  SEED <- 4630
  
  # data split ratios
  TRAIN_RATIO <- 0.7
  TEST_RATIO <- 0.3

############################### data_cleaning.R ################################

# Load Dependencies ------------------------------------------------------------

# load libraries
library(tidyverse)

# get data from data dict
Index <- 1:60
Name <- c("city", "country", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28", "x29", "x30", "x31", "x32", "x33", "x34", "x35", "x36", "x37", "x38", "x39", "x40", "x41", "x42", "x43", "x44", "x45", "x46", "x47", "x48", "x49", "x50", "x51", "x52", "x53", "x54", "x55", "data_quality", "expensive", "usa")
Alias <- c("city", "country", "meal1", "meal2", "mcmeal", "beer.rest.domestic", "beer.rest.imported", "coffee", "soda", "water.rest", "milk", "bread", "rice", "eggs", "cheese", "chicken", "beef", "apples", "bananas", "oranges", "tomatoes", "potatoes", "onions", "lettuce", "water.market", "wine", "beer.market.domestic", "beer.market.imported", "cigarettes", "ticket", "pass", "taxi.start", "taxi.km", "taxi.hr", "gas", "volkswagen", "toyota", "basic", "mobile", "internet", "gym", "tennis", "cinema", "preschool", "school", "jeans", "dresses", "nikes", "shoes", "rent1.center", "rent1.outer", "rent3.center", "rent3.outer", "sqm.center", "sqm.outer", "salary", "mortgage", "quality", "expensive", "usa")
Display <- c("City", "Country", "Cheap Meal", "Mid-Meal for 2", "McMeal", "Domestic Beer at Restaurants", "Imported Beer at Restraunts", "Cappucinno", "Coke/Pepsi", "Water at Restaurants", "Milk", "Bread Loaf", "Rice", "Eggs", "Cheese", "Chicken", "Beef", "Apples", "Bananas", "Oranges", "Tomatoes", "Potatoes", "Onions", "Lettuce", "Water at Market", "Wine", "Domestic Beer at Market", "Imported Beer at Market", "Cigarettes", "Local Transport Ticket", "Monthly Pass", "Taxi Start", "Taxi 1km", "Taxi 1hr", "Gasoline", "Volkswagen Golf", "Toyota Corolla", "Utilities", "Mobile", "Internet", "Gym", "Tennis", "Cinema", "Preschool", "Primary School", "Jeans", "Dresses", "Nikes", "Business Shoes", "1 Bed Central Apartment", "1 Bed Outer Apartment", "3 Bed Central Apartment", "3 Bed Outer Apartment", "Central Price Per Sqm", "Outer Price Per Sqm", "Monthly Salary", "Mortgage Interest Rate", "Quality", "Relative Rent Cost", "United States")


# load data
data <- read.csv(DATA_PATH) %>%
  filter(!is.na(x48) & !is.na(x54)) %>%
  mutate(country = as.factor(country),
         data_quality = as.factor(data_quality))

data.dict <- data.frame(Index=Index, Name=Name, Alias=Alias, Display=Display)


# Train/Test Split -------------------------------------------------------------

# set seed
set.seed(SEED)

# split
sample.data <- sample.int(nrow(data), floor(TRAIN_RATIO * nrow(data)), replace = F)
train.data <- data[sample.data, ]
test.data <- data[-sample.data, ]


# Data Transformation Functions ------------------------------------------------

# impute function
impute <- function(df, medians) {
  for (i in seq_along(medians)) {
    df[, i][is.na(df[, i])] <- medians[i]
  }
  return(df)
}

# scale function
normalize <- function(df, means, vars) {
  for (i in seq_along(means)) {
    df[, i] <- (df[, i]  - means[i]) / sqrt(vars[i])
  }
  return(df)
}

# get fit from training data
fit <- function(df) {
  
  # get numeric columns
  c <- ncol(df)
  num.cols <- (1:c)[sapply(df, is.numeric)]
  
  
  # mean, median, var
  means <- sapply(df[num.cols], mean, na.rm=TRUE)
  medians <- sapply(df[num.cols], median, na.rm=TRUE)
  vars <- sapply(df[num.cols], var, na.rm=TRUE)
  
  # return list
  list(num.cols=num.cols, means=means, medians=medians, vars=vars)
}

# transform training or test data using fit
transform <- function(df, fit_list, impute=FALSE, scale=FALSE) {
  # impute NAs
  if (impute) {df[fit_list$num.cols] <- impute(df[fit_list$num.cols], fit_list$medians)}
  
  # create expensive variable from x48
  set.seed(SEED)
  df$expensive = ifelse(df$x48 == fit_list$medians[48] | is.na(df$x48), rbinom(1, 1, 0.5), 0)
  df$expensive = as.factor(ifelse(df$x48 < fit_list$medians[48] | is.na(df$x48), df$expensive, 1))
  
  # create USA variable
  df$usa = as.factor(ifelse(df$country == "United States", 1, 0))
  
  # scale numeric values
  if (scale) {df[fit_list$num.cols] <- scale(df[fit_list$num.cols], fit_list$means, fit_list$vars)}
  
  # rename columns
  df %>% rename(all_of(setNames(colnames(df), data.dict$Alias)))
}

# pivot numeric columns to longer 
wide_to_long <- function(df) {
  df.longer <- df %>%
    pivot_longer(cols = where(is.numeric),
                 names_to = "type",
                 values_to = "cost") %>%
    filter(!is.na(cost))
  
  df.longer
}


# Apply Transformations --------------------------------------------------------

# fit datasets
my.fit <- fit(train.data)

# transform datasets
train <- transform(train.data, my.fit, impute=TRUE, scale=FALSE)
test <- transform(test.data, my.fit, impute=TRUE, scale=FALSE)
eda <- transform(train.data, my.fit, impute=FALSE, scale=FALSE)

# pivot longer (for EDA only)
eda.long <- wide_to_long(eda)


########################## themes.R ############################################


# GGText Functions -------------------------------------------------------------

# these are used for creating multicolored titles and subtitles (examples in eda.R)
fColor <- function(text, color) {glue("<span style='color:{color};'>{text}</span>")}
fSize <- function(subtitle, size) {glue("<span style='font-size:{size}pt'>{subtitle}</span>")}
createTitle <- function(title, subtitle) {paste(title, "  \n", fSize(subtitle, 11), sep="")}


# Color Hexcodes ---------------------------------------------------------------

RED <- "#F8766D"
BLUE <- "#00BFC4"


# Custom GGplot Thenes ---------------------------------------------------------

markdown.title <- theme(plot.title = element_markdown(lineheight = 1.1, hjust=0.5))

blank.x <- theme(
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.title.x = element_blank(),
  panel.grid.major.x = element_blank()
)

blank.y <- theme(
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.title.y = element_blank(),
  panel.grid.major.x = element_blank()
)

# scatter plot matrix
scatter.theme <- theme_bw() + markdown.title

# facet-wrapped boxplots 
box.theme <- theme_bw() + markdown.title + blank.x + 
  theme(axis.title.y = element_blank())

# country boxplots
country.theme <- theme_bw() + markdown.title + 
  theme(axis.text.x = element_text(angle = -45, hjust=0),
        panel.grid.major.x = element_blank())

# salary histogram
hist.theme <- theme_bw() + markdown.title

############################## eda.4 ###########################################

# Load Dependencies ------------------------------------------------------------

# load libraries
library(tidyverse)
library(glue)
library(ggtext)
library(rlang)
library(GGally)
library(viridis)
library(grid)
library(corrplot)
library(faraway)
library(kableExtra)

# create display names
display <- setNames(data.dict$Display, data.dict$Alias)
rev.display <- setNames(data.dict$Alias, data.dict$Display)

# get numeric columns
num.cols <- which(sapply(eda, is.numeric))


# Correlation Matrix -----------------------------------------------------------

corr.train <- eda %>%
  rename(all_of(rev.display)) %>%
  select(where(is.numeric)) %>%
  na.omit() %>%
  cor()

path.corr <- file.path(ROOT, "Output", "corr.png")

png(path.corr, width=7, height=7, units="in", res=200)
print(corrplot(corr.train,
               #title = "Correlation Matrix of Commodity Prices",
               method = "circle",
               type = "lower",
               diag = TRUE,
               order = 'FPC',
               outline = FALSE,
               col = COL2("PuOr", 10),
               tl.col = "black",
               tl.srt = 45,
               tl.cex = 0.4))
title("Correlation Matrix of Commodity Prices", cex.main=0.8)
dev.off()


# Response Histogram -----------------------------------------------------------

# custom title
hist.title <- createTitle(
  title = "**Distribution of Average Monthly Salary of Global Cities**",
  subtitle = paste(
    "Relative to ",
    fColor("cities abroad", RED),
    ", ",
    fColor("American cities", BLUE),
    " showcase significantly higher incomes",
    sep=""
  )
)

# get medians
med.domestic <- median(eda$salary[train$usa == 1], na.rm=TRUE)
med.foreign <- median(eda$salary[train$usa == 0], na.rm=TRUE)

# create plot
plot.hist <- eda %>%
  # data cleaning
  dplyr::select(salary, usa) %>%
  na.omit() %>%
  mutate(domestic = ifelse(usa==1, salary, NA),
         foreign = ifelse(usa==0, salary, NA)) %>%
  # create plot
  ggplot() +
  geom_histogram(aes(x=foreign), alpha=0.7, fill=RED, boundary = 0) +
  geom_histogram(aes(x=domestic), alpha=0.5, fill=BLUE, boundary = 0) +
  geom_segment(x=med.foreign, xend=med.foreign, y=0, yend=Inf,linetype="dashed", color=RED) +
  geom_segment(x=med.domestic, xend=med.domestic, y=0, yend=Inf, linetype="dashed", color=BLUE) +
  scale_x_continuous(limits=c(0, 10000), breaks = sort(c(seq(0, 10000, 2500), round(segments$x, 0)))) +
  labs(title = hist.title, x="Monthly Salary (USD)", y="Count") +
  hist.theme + theme(axis.text.x = element_text(colour = c("black", RED, "black", BLUE, rep(c("black"), 3))))

plot.hist


# Scatterplots -------------------------------------------------------------------

cormat <- eda %>%
  dplyr::select(all_of(num.cols)) %>%
  cor(use="complete.obs")

cors <- sort(cormat[54, -54], TRUE)
df.cors <- data.frame(predictor=names(cors), corr=cors)


# custom title
salary.scatter.title <- createTitle(
  title = "**Monthly Salary vs. Common Commodity Prices**"
)

# create plot
plot.scatter <- eda %>%
  # data cleaning
  dplyr::select(salary, all_of(df.cors$predictor[1:54])) %>%
  gather(-salary, key="predictor", value="value") %>%
  left_join(df.cors, by="predictor") %>%
  mutate(predictor = reorder(as.factor(predictor), -corr, mean)) %>%
  na.omit() %>%
  # create scatterplot matrix
  ggplot(aes(x=value, y=salary, color=corr)) +
  geom_point(alpha = 0.3, show_guide = FALSE) +
  facet_wrap(.~predictor, scales="free", ncol=6, labeller = as_labeller(display)) +
  scale_color_gradientn(limits=c(-1, 1), colors=c("maroon", "gray90", "navy")) +
  labs(title=salary.scatter.title) + 
  salary.scatter.theme

plot.scatter


# VIF Table --------------------------------------------------------------------

# create model
model.full <- lm(salary ~., data=eda[num.cols])
vifs <- faraway::vif(model.full)
vifs <- vifs[vifs >= 5]
vif.table <- cbind("VIF"=round(sort(vifs, TRUE), 2))
rownames(vif.table) <- display[rownames(vif.table)]

# output table
vif.table %>%
  kbl(caption = "Variance Inflation Factors") %>%
  kable_classic(full_width = FALSE, html_font = "Times New Roman")


########################### shrinkage.R #########################################


# Load Dependencies ------------------------------------------------------------

# load libraries
library(tidyverse)
library(glmnet)
library(kableExtra)

# Variable Selection -----------------------------------------------------------

# remove unwanted variables
exclude.test <- c("city","country", "quality", "expensive")

train.processed <- train %>% dplyr::select(!all_of(exclude.test))
test.processed <- test %>% dplyr::select(!all_of(exclude.test))

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

df.lasso.high <- df.lasso[1:28, ]
df.lasso.low <- df.lasso[29:56, ]

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

# create model
model.ols <- glmnet::glmnet(x.train, y.train, alpha=0, lambda=0, thresh=1e-32)

options(scipen = 999)
coef.ols <-coef(model.ols)
df.ols <- data.frame(
  var=rownames(coef.ols),
  coef=round(as.numeric(coef.ols),3)
)

# Order coefficients by absolute value in descending order
df.ols <- df.ols[order(-abs(df.ols$coef)), ]

df.ols.high <- df.ols[1:28, ]
df.ols.low <- df.ols[29:56, ]

rownames(df.ols.high) <- NULL
rownames(df.ols.low) <- NULL

df.ols.high %>%
  kbl(caption = "OLS Coefficients") %>%
  kable_classic(full_width = FALSE, html_font = "Times New Roman")

df.ols.low %>%
  kbl(caption = "OLS Coefficients") %>%
  kable_classic(full_width = FALSE, html_font = "Times New Roman")


# calculate test MSE
ols.pred <- predict(model.ols, newx=x.test)
ols.mse <- mean((ols.pred - y.test)^2)
ols.mse


ridge.mse
lasso.mse
ols.mse


########################### regression_tree.R ##################################


# Load Dependencies ------------------------------------------------------------

# load libraries
library(tidyverse)
library(MASS)
library(tree)
library(randomForest)
library(gbm)

# Variable Selection -----------------------------------------------------------

# remove unwanted variables
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


####################### classification_tree.R ##################################



# Load Dependencies ------------------------------------------------------------

# load libraries
library(tidyverse)
library(MASS)
library(tree)
library(randomForest)
library(gbm)


# Variable Selection -----------------------------------------------------------

# remove unwanted variables
exclude.test <- c("city","country", "quality", "rent1.center", "rent1.outer",
                  "rent3.center", "rent3.outer", "sqm.center", "sqm.outer")

train.processed <- train %>% dplyr::select(!all_of(exclude.test))
test.processed <- test %>% dplyr::select(!all_of(exclude.test))


# Recursive Binary Splitting ---------------------------------------------------

# create tree
tree.cls <- tree::tree(expensive~., data=train.processed)
summary(tree.cls)

# plot tree
plot(tree.cls)
text(tree.cls, cex=0.75, pretty=0)

# predictions
tree.cls.pred <- predict(tree.cls, newdata=test.processed, type="class")
tree.cls.probs <- predict(tree.cls, newdata=test.processed)

# confusion matrix for test data
tree.cls.conf.d <- table(test.processed$expensive, tree.cls.pred)
tree.cls.conf.d

tree.thresh <- 0.60
tree.cls.conf.c <- table(test.processed$expensive, tree.cls.probs[,2] > tree.thresh)
tree.cls.conf.c

# calculate test accuracy
tree.cls.acc.d <- mean(tree.cls.pred == test.processed$expensive)
tree.cls.acc.d

tree.cls.acc.c <- mean(as.numeric(tree.cls.probs[,2] > tree.thresh) == test.processed$expensive)
tree.cls.acc.c




# Pruning ----------------------------------------------------------------------

set.seed(SEED)
cv.cls.10 <- tree::cv.tree(tree.cls, K=10, FUN=prune.misclass) 
cv.cls.10

# plot of dev against size
plot(cv.cls.10$size, cv.cls.10$dev,type='b')

# size of tree chosen by pruning
prune.cls.num <- cv.cls.10$size[which.min(cv.cls.10$dev)]
prune.cls.num # 7 and 4 have same

# fit tree with size chosen by pruning
prune.cls <-tree::prune.misclass(tree.cls, best=4)
summary(prune.cls)

# plot pruned tree
plot(prune.cls)
text(prune.cls, cex=0.75, pretty=0)

# predictions
prune.cls.pred <- predict(prune.cls, newdata=test.processed, type="class")
prune.cls.probs <- predict(prune.cls, newdata=test.processed)

# confusion matrix for test data
prune.cls.conf.d <- table(test.processed$expensive, prune.cls.pred)
prune.cls.conf.d

prune.thresh <- 0.5
prune.cls.conf.c <- table(test.processed$expensive, prune.cls.probs[,2] > prune.thresh)
prune.cls.conf.c

# calculate test accuracy
prune.cls.acc.d <- mean(prune.cls.pred == test.processed$expensive)
prune.cls.acc.d

prune.cls.acc.c <- mean(as.numeric(prune.cls.probs[,2] > prune.thresh) == test.processed$expensive)
prune.cls.acc.c


# Random Forest ----------------------------------------------------------------

set.seed(SEED)
rf.cls <- randomForest::randomForest(expensive~., data=train.processed, mtry=7, importance=TRUE)
rf.cls

# see variable importance
round(importance(rf.cls),2)
varImpPlot(rf.cls)

# predictions
rf.cls.pred <- predict(rf.cls, newdata=test.processed)
rf.cls.probs <- predict(rf.cls, newdata=test.processed, type="prob")

# confusion matrix for test data
rf.cls.conf.d <- table(test.processed$expensive, rf.cls.pred)
rf.cls.conf.d

rf.thresh <- 0.55
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























  