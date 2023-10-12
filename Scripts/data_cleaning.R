
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
data <- read.csv(DATA_PATH)
data.dict <- read.csv(DICT_PATH)


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
  countries <- names(country.table[country.table > 50])
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
           country = as.factor(ifelse(country %in% fit_list$countries,
                                      country, fit_list$country.missing)),
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
train.trans <- transform(train.data, my.fit)
test.trans <- transform(test.data, my.fit)

# pivot longer (for EDA only)
train.trans.long <- wide_to_long(train.trans)
test.trans.long <- wide_to_long(train.trans)


# Save Transformed Data --------------------------------------------------------

saveRDS(train.trans, file=TRAIN_PATH)
saveRDS(test.trans, file=TEST_PATH)

saveRDS(train.trans.long, file=TRAINLONG_PATH)
saveRDS(test.trans.long, file=TESTLONG_PATH)

