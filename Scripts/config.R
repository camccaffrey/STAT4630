
#
# File: config.R
# Description: configuration file
#

# File Paths (OS Independent) --------------------------------------------------

# set root (should be STAT4630 folder)
ROOT <- getwd()

# set paths to data
DATA_PATH <- file.path(ROOT, "Data", "cost-of-living_v2.csv")
DICT_PATH <- file.path(ROOT, "Data", "data-dict.csv")

TRAIN_PATH <- file.path(ROOT, "Data", "train.rds")
TEST_PATH <- file.path(ROOT, "Data", "test.rds")

TRAINLONG_PATH <- file.path(ROOT, "Data", "train-long.rds")
TESTLONG_PATH <- file.path(ROOT, "Data", "test-long.rds")


# Packages (RENV Alternative) --------------------------------------------------

# required packages
REQUIREMENTS <- c(
  "tidyverse",  # collection of data science packages
  "GGally",     # ggplot extension that helps combine objects
  "rlang",      # provides various interfaces for working with R and R objects
  "ggtext",     # provides simple Markdown and HTML rendering for ggplot2
  "viridis",    # provides the base functions for generating the color maps
  "glue",       # interpret string literals
  "rprojroot"   # finding files in project subdirectories
)

# load required packages (alternative to renv) 
load_requirements <- function() {
  # check and install packages
  new.packages <- REQUIREMENTS[!(REQUIREMENTS %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  # load packages
  lapply(REQUIREMENTS, require, character.only = TRUE)
}


# Test / Train Split -----------------------------------------------------------

# random seed for reproducibility
SEED <- 4630

# data split ratios
TRAIN_RATIO <- 0.7
TEST_RATIO <- 0.3


# Variables of Interest --------------------------------------------------------

# response variable name
RESPONSE <- "expensive"

# predictor variable names
#PREDICTORS <- c()


# Other ------------------------------------------------------------------------

# resolution of graphics (72-96 suitable for GitHub)
DPI <- 96
