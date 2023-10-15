
#
# File: complete.R
# Authors: Group 13 for STAT 4630
# Description: combination of config.R, themes.R, data_cleaning.R, eda.R, and
#              model_building.R. Redundancies have been removed.
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
  "grid",       # used for testing, no longer needed 
  "corrplot",   # correlation matrix displays
  "ROCR",       # ROC curves and AOC
  "MASS",       # used for LDA
  "boot",       # cross validation
  "ipred",      # error estimates for LDA cross validation
  "ICS"         # test kurtosis and skewness (MVN assumption)
  
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

############################# data_cleaning.R ##################################

# Load Dependencies ------------------------------------------------------------

# load libraries
library(tidyverse)

# get data from data dict
Index <- 1:60
Name <- c("city", "country", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28", "x29", "x30", "x31", "x32", "x33", "x34", "x35", "x36", "x37", "x38", "x39", "x40", "x41", "x42", "x43", "x44", "x45", "x46", "x47", "x48", "x49", "x50", "x51", "x52", "x53", "x54", "x55", "data_quality", "expensive", "usa")
Alias <- c("city", "country", "meal1", "meal2", "mcmeal", "beer.rest.domestic", "beer.rest.imported", "coffee", "soda", "water.rest", "milk", "bread", "rice", "eggs", "cheese", "chicken", "beef", "apples", "bananas", "oranges", "tomatoes", "potatoes", "onions", "lettuce", "water.market", "wine", "beer.market.domestic", "beer.market.imported", "cigarettes", "ticket", "pass", "taxi.start", "taxi.km", "taxi.hr", "gas", "volkswagen", "toyota", "basic", "mobile", "internet", "gym", "tennis", "cinema", "preschool", "school", "jeans", "dresses", "nikes", "shoes", "rent1.center", "rent1.outer", "rent3.center", "rent3.outer", "sqm.center", "sqm.outer", "salary", "mortgage", "quality", "expensive", "usa")
Display <- c("City", "Country", "Cheap Meal", "Mid-Meal for 2", "McMeal", "Domestic Beer at Restaurants", "Imported Beer at Restraunts", "Cappucinno", "Coke/Pepsi", "Water at Restaurants", "Milk", "Bread Loaf", "Rice", "Eggs", "Cheese", "Chicken", "Beef", "Apples", "Bananas", "Oranges", "Tomatoes", "Potatoes", "Onions", "Lettuce", "Water at Market", "Wine", "Domestic Beer at Market", "Imported Beer at Market", "Cigarettes", "Local Transport Ticket", "Monthly Pass", "Taxi Start", "Taxi 1km", "Taxi 1hr", "Gasoline", "Volkswagen Golf", "Toyota Corolla", "Utilities", "Mobile", "Internet", "Gym", "Tennis", "Cinema", "Preschool", "Primary School", "Jeans", "Dresses", "Nikes", "Business Shoes", "1 Bed Central Apartment", "1 Bed Outer Apartment", "3 Bed Central Apartment", "3 Bed Outer Apartment", "Central Price Per Sqm", "Outer Price Per Sqm", "Monthly Salary", "Mortgage Interest Rate", "Quality", "Relative Rent Cost", "United States")

data <- read.csv(DATA_PATH)
data.dict <- data.frame(Index=Index, Name=Name, Alias=Alias, Display=Display)


# Train/Test Split -------------------------------------------------------------

# set seed
set.seed(SEED)

# split
sample.data <- sample.int(nrow(data), floor(TRAIN_RATIO * nrow(data)), replace = F)
train.data <- data[sample.data, ]
test.data <- data[-sample.data, ]


# Data Transformation Functions ------------------------------------------------

# get fit from training data
fit <- function(df) {
  # median x48
  median.x48 <- median(df$x48, na.rm=TRUE)
  
  # countries
  country.table <- sort(table(df$country), decreasing=TRUE)
  countries <- names(country.table[country.table > 50])[1]
  country.missing <- "Other"
  
  # return list
  list(median.x48=median.x48,
       countries=countries,
       country.missing=country.missing)
}

# transform training or test data using fit
transform <- function(df, fit_list) {
  df.transform <- df %>%
    # impute x48 value
    mutate(x48 = ifelse(is.na(x48), fit_list$median.x48 + rnorm(1), x48)) %>%
    
    # mutate categorical variables
    mutate(expensive = as.factor(ifelse(x48 > fit_list$median.x48, 1, 0)),
           usa = as.factor(ifelse(country %in% fit_list$countries, 1, 0)),
           data_quality = as.factor(data_quality))
  
  
  df.transform <- df.transform %>%
    # rename columns with aliases
    rename(all_of(setNames(colnames(df.transform), data.dict$Alias)))
  
  
  # return dataframe
  df.transform
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
train <- transform(train.data, my.fit)
test <- transform(test.data, my.fit)

# pivot longer (for EDA only)
train.long <- wide_to_long(train)
test.long <- wide_to_long(test)


########################### themes.R + eda.R ###################################

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

# create display names
display <- setNames(data.dict$Display, data.dict$Alias)
rev.display <- setNames(data.dict$Alias, data.dict$Display)


# GGText Functions -------------------------------------------------------------

# these are used for creating multicolored titles and subtitles (examples in eda.R)
fColor <- function(text, color) {glue("<span style='color:{color};'>{text}</span>")}
fSize <- function(subtitle, size) {glue("<span style='font-size:{size}pt'>{subtitle}</span>")}
createTitle <- function(title, subtitle) {paste(title, "  \n", fSize(subtitle, 11), sep="")}


# Color Hexcodes ---------------------------------------------------------------

RED <- "#F8766D"
BLUE <- "#00BFC4"


# Custom GGplot Themes ---------------------------------------------------------

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


# Correlation Matrix -----------------------------------------------------------

# create correlation matrix with training data
corr.train <- train %>%
  rename(all_of(rev.display)) %>%
  dplyr::select(where(is.numeric)) %>%
  na.omit() %>%
  cor()

# WARNING: this will update the image stored in Output/corr.png
#png(path.corr, width=7, height=7, units="in", res=200)
#print(
  corrplot(corr.train,
               #title = "Correlation Matrix of Commodity Prices",
               method = "circle",
               type = "lower",
               diag = TRUE,
               order = 'FPC',
               outline = FALSE,
               col = COL2("PuOr", 10),
               tl.col = "black",
               tl.srt = 45,
               tl.cex = 0.4)
#)
title("Correlation Matrix of Commodity Prices", cex.main=0.8)
#dev.off()


# Variables of Interest --------------------------------------------------------

# original columns (33 -> "x33")
x <- c(33, 36, 41, 1, 9, 10, 12, 17, 24, 54)

# names of columns
COLS1 <- colnames(train)[x+2]
COLS2 <- COLS1[1:5]


# Scatterplot Matrix -----------------------------------------------------------

# custom title
scatter.title <- createTitle(
  title = "**Scatterplot Matrix of Commodity Prices**",
  
  subtitle = paste(
    "For cities that fall",
    fColor("*above*", RED),
    "and",
    fColor("*below*", BLUE),
    "the the median average rent for a 1-bedroom apartment"
  )
)


# define content of each scatterplot
upperfun <- function(data, mapping) {
  # get variables
  x_var <- as_name(mapping$x)
  y_var <- as_name(mapping$y)
  c <- cor(data[[x_var]], data[[y_var]])
  
  # calculate the x and y positions for the text
  x_range <- range(data[[x_var]], na.rm = TRUE)
  y_range <- range(data[[y_var]], na.rm = TRUE)
  x_pos <- x_range[2] - 0.30 * diff(x_range)
  y_pos <- y_range[2] - 0.05 * diff(y_range)
  
  # return ggplot object
  ggplot(data = data, mapping = mapping) +
    geom_point(alpha = 0.4) +
    geom_text(aes(label = paste("r =",round(c,3)), x=x_pos, y=y_pos),
              color = "darkgray", size = 4) +
    scale_color_manual(values = c("1" = RED, "0" = BLUE)) 
}

# create plot
plot.scatter <- train %>%
  # data cleaning
  dplyr::select(all_of(COLS2), expensive) %>%
  na.omit() %>%
  # create scatterplot matrix
  ggpairs(columns = 1:5, aes(colour = expensive, alpha = 0.4),
          upper = list(continuous = wrap(upperfun)),
          lower = "blank",
          labeller = as_labeller(display)) +
  scatter.theme + labs(title = scatter.title) +
  scale_color_manual(values = c("1" = RED, "0" = BLUE)) +  
  scale_fill_manual(values = c("1" = RED, "0" = BLUE))

plot.scatter



# Rent Box Plots ---------------------------------------------------------------

# custom title
box.title <- createTitle(
  title = "**Boxplot of Key Commodity Prices**",
  
  subtitle = paste(
    "For cities that fall",
    fColor("*above*", RED),
    "and",
    fColor("*below*", BLUE),
    "the the median average rent for a 1-bedroom apartment"
  )
)


# create plot
plot.box <- train.long %>%
  # data cleaning
  filter(type %in% COLS1) %>%
  # create plot
  ggplot(aes(x=expensive, y=cost, fill = expensive)) +
  facet_wrap(vars(type), ncol = 5, scales = "free_y", labeller = as_labeller(display)) +
  stat_boxplot(geom = "errorbar", width = 0.3) +
  geom_boxplot(width = 0.5, show.legend = FALSE,  outlier.shape = 1) +
  scale_fill_manual(values = c("1" = RED, "0" = BLUE)) +
  box.theme + labs(title = box.title)

plot.box



# Country Box Plots ------------------------------------------------------------

# create custom title
country.title = "**Distribution of Average Monthly Salary by Country**"

# top countries
country.table <- sort(table(train$country), decreasing=TRUE)
top.countries <- names(country.table[country.table > 50])

# create plot
plot.country <- train %>%
  # data cleaning
  group_by(country) %>%
  filter(country %in% top.countries) %>%
  na.omit() %>%
  # create plot
  ggplot(aes(x=reorder(country, -salary, median),
             y=salary,
             fill = reorder(country, salary, median))) +
  stat_boxplot(geom = "errorbar", width = 0.3) +
  geom_boxplot(width = 0.5, show.legend = FALSE,  outlier.shape = 1) +
  scale_fill_viridis(discrete=TRUE) +
  country.theme +
  labs(title = country.title, x = "Country", y = "Average Monthly Salary ($)")

plot.country


########################### model_building.R ###################################

# Load Dependencies ------------------------------------------------------------

# load libraries
library(tidyverse)
library(ROCR)
library(MASS)
library(boot)
library(ipred)
library(ICS)


# Variable Selection -----------------------------------------------------------

COLS <- c("expensive", "meal1", "bread", "eggs", "wine", "gas", "basic", "cinema", "salary")

train.sub <- train %>%
  dplyr::select(all_of(COLS)) %>%
  na.omit()

test.sub <- test %>%
  dplyr::select(all_of(COLS)) %>%
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


