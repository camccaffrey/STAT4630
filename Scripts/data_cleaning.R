
#
# File: data_cleaning.R
# Description: data cleaning for "cost-of-living-v2.csv" dataset
#

# Load Dependencies ------------------------------------------------------------

# get global variables
source(file.path("Scripts", "config.R"))

# load libraries
library(tidyverse)

# load data
data <- read.csv(DATA_PATH) %>%
  filter(!is.na(x48) & !is.na(x54)) %>%
  mutate(country = as.factor(country),
         data_quality = as.factor(data_quality))

data.dict <- read.csv(DICT_PATH)


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
train.trans <- transform(train.data, my.fit, impute=TRUE, scale=FALSE)
test.trans <- transform(test.data, my.fit, impute=TRUE, scale=FALSE)
eda.trans <- transform(train.data, my.fit, impute=FALSE, scale=FALSE)

# pivot longer (for EDA only)
eda.long <- wide_to_long(eda.trans)


# Save Transformed Data --------------------------------------------------------

saveRDS(train.trans, file=TRAIN_PATH)
saveRDS(test.trans, file=TEST_PATH)

saveRDS(eda.trans, file=EDA_PATH)
saveRDS(eda.long, file=EDALONG_PATH)

