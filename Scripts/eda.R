
#
# File: eda.R
# Description: basic data cleaning and EDA with the "cost-of-living-v2.csv" dataset
#

# Load Dependencies ------------------------------------------------------------

# get global variables
source(file.path("Scripts", "config.R"))

# load libraries
library(tidyverse)
library(glue)
library(ggtext)
library(rlang)
library(GGally)
library(viridis)

# load data
data <- read.csv(DATA_PATH)
data.dict <- read.csv(DICT_PATH)

# Data Cleaning ----------------------------------------------------------------

# train test split
set.seed(SEED)
sample_data <- sample.int(nrow(data), floor(TRAIN_RATIO * nrow(data)), replace = F)
train <- data[sample_data, ]
test <- data[-sample_data, ]

# IGNORE FOR NOW (see Piazza question @61)
#fit <- function(df) {
#  median.x48 <- median(df$x48, na.rm=TRUE)
#  countries <- unique(df$country)
#  mode.country <- names(sort(-table(df$country)))[1]
#  list(median.x48=median.x48, countries=countries, mode.country=mode.country)
#}
#
#transform <- function(df, fit_list) {
#  df.transform <- df %>%
#    # impute x48 value
#    mutate(x48 = ifelse(is.na(x48), fit_list$median.x48 + rnorm(1), x48)) %>%
#    mutate(expensive = as.factor(ifelse(x48 > fit_list$median.x48, 1, 0)),
#           country = as.factor(ifelse(country %in% fit_list$countries, country, fit_list$mode.country)),
#           data_quality = as.factor(data_quality))
#}

data.wide <- data %>%
  mutate(expensive = as.factor(ifelse(x48 > median(x48, na.rm=TRUE), 1, 0)),
         country = as.factor(country),
         data_quality = as.factor(data_quality)) %>%
  filter(!is.na(expensive))

data.long <- data.wide %>%
  pivot_longer(!c(city, country, expensive, data_quality),
               names_to = "type", values_to = "cost") %>%
  filter(!is.na(cost))

# create aliases
aliases <- setNames(data.dict$Display, colnames(data.wide))
rev.aliases <- setNames(colnames(data.wide), data.dict$Display)



# Variables of Interest --------------------------------------------------------

COLS1 <- c("x33", "x36", "x41", "x1", "x9", "x10", "x12", "x17", "x24", "x54")
COLS2 <- COLS1[1:5]



# GGText Functions -------------------------------------------------------------

fColor <- function(text, color) {glue("<span style='color:{color};'>{text}</span>")}
fSize <- function(subtitle, size) {glue("<span style='font-size:{size}pt'>{subtitle}</span>")}
createTitle <- function(title, subtitle) {paste(title, "  \n", fSize(subtitle, 11), sep="")}



# Scatterplot Matrix -----------------------------------------------------------

# custom theme
scatter.theme <- theme_bw() +
  theme(plot.title = element_markdown(lineheight = 1.1, hjust=0.5))

# custom title
title <- "**Scatterplot Matrix of Commodity Prices**"
subtitle <- glue("For cities that fall {above} and {below} the the median average rent for a 1-bedroom apartment",
                 above = fColor("*above*", RED),
                 below = fColor("*below*", BLUE))

scatter.title <- createTitle(title, subtitle)

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
              color = "darkgray", size = 4)
}

# create plot
plot.scatter <- data.wide %>%
  # data cleaning
  select(all_of(COLS2), expensive) %>%
  na.omit() %>%
  # create scatterplot matrix
  ggpairs(columns = 1:5, aes(colour = expensive, alpha = 0.4),
          upper = list(continuous = wrap(upperfun)),
          lower = "blank",
          labeller = as_labeller(aliases)) +
  scatter.theme + labs(title = scatter.title)

plot.scatter



# Rent Box Plots ---------------------------------------------------------------

# custom theme
box.theme <- theme_bw() +
  theme(plot.title = element_markdown(lineheight = 1.1, hjust=0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank())

# custom title
title <- "**Boxplot of Key Commodity Prices**"
subtitle <- glue("For cities that fall {above} and {below} the the median average rent for a 1-bedroom apartment",
                 above = fColor("*above*", RED),
                 below = fColor("*below*", BLUE))

box.title <- createTitle(title, subtitle)


# create plot
plot.box <- data.long %>%
  # data cleaning
  filter(type %in% COLS1) %>%
  # create plot
  ggplot(aes(x=expensive, y=cost, fill = expensive)) +
  facet_wrap(vars(type), ncol = 5, scales = "free_y", labeller = as_labeller(aliases)) +
  stat_boxplot(geom = "errorbar", width = 0.3) +
  geom_boxplot(width = 0.5, show.legend = FALSE,  outlier.shape = 1) +
  box.theme + labs(title = box.title)

plot.box



# Country Box Plots ------------------------------------------------------------

# get top countries
t <- sort(table(data$country), decreasing=TRUE)
topCountries <- names(t[t > 50])
topCountries

# create custom theme
country.theme <- theme_bw() +
  theme(plot.title = element_markdown(lineheight = 1.1, hjust=0.5),
        axis.text.x = element_text(angle = -45, hjust=0),
        panel.grid.major.x = element_blank())

# create custom title
country.title = glue("**Distribution of Average Monthly Salary by Country**")

# create plot
plot.country <- data.wide %>%
  # data cleaning
  group_by(country) %>%
  filter(country %in% topCountries) %>%
  na.omit() %>%
  # create plot
  ggplot(aes(x=reorder(country, -x54, median), y=x54, fill = reorder(country, x54, median))) +
  stat_boxplot(geom = "errorbar", width = 0.3) +
  geom_boxplot(width = 0.5, show.legend = FALSE,  outlier.shape = 1) +
  scale_fill_viridis(discrete=TRUE) +
  country.theme +
  labs(title = country.title, x = "Country", y = "Average Monthly Salary ($)")

plot.country


