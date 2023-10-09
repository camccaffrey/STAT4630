
#
# File: load_dependencies.R
# Description: load necessary packages (alternative to renv)
#

####################################################
# WARNING: This file is now outdated, see config.R #
# ##################################################

# get required packages
source(file.path("Scripts", "config.R"))

# check and install packages
new.packages <- REQUIREMENTS[!(REQUIREMENTS %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load packages
lapply(REQUIREMENTS, require, character.only = TRUE)
