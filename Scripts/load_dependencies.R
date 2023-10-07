#
# File: load_dependencies.R
# Description: Load necessary packages and data
#

# list required packages
list.of.packages <- c(
  "tidyverse",  # collection of data science packages
  "GGally",     # ggplot extension that helps combine objects
  "rlang",      # provides various interfaces for working with R and R objects
  "ggtext",     # provides simple Markdown and HTML rendering for ggplot2
  "viridis",    # provides the base functions for generating the color maps
  "glue",       # interpret string literals
  "rprojroot"   # finding files in project subdirectories
)

# check and install packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load packages
lapply(list.of.packages, require, character.only = TRUE)

# get project root
root <- find_root_file(criterion=is_git_root)

# load data
data <- read.csv(file.path(root, "Data", "cost-of-living_v2.csv"))

