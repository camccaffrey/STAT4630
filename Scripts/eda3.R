
#
# File: eda3.R
# Description: EDA for Milestone 3, built off of Milestone 2
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
library(grid)
library(corrplot)

# load data
data.dict <- read.csv(DICT_PATH)
train <- readRDS(EDA_PATH)
train.long <- readRDS(EDALONG_PATH)

# load themes
source(file.path("Scripts", "themes.R"))

# create display names
display <- setNames(data.dict$Display, data.dict$Alias)
rev.display <- setNames(data.dict$Alias, data.dict$Display)


# Variables of Interest --------------------------------------------------------

# original columns (33 -> "x33")
x <- c(33, 36, 41, 1, 9, 10, 12, 17, 24, 54)

# names of columns
COLS1 <- colnames(train)[x+2]
COLS2 <- COLS1[c(3, 4, 6, 7, 10)]


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


# Save Plots -------------------------------------------------------------------

# define paths
path.scatter <- file.path(ROOT, "Output", "scatter-matrix.png")
path.box <- file.path(ROOT, "Output", "boxplots.png")
path.country <- file.path(ROOT, "Output", "countries.png")

# default DPI resolution in config.R
ggsave(path.scatter, plot.scatter, width=7, height=7, dpi=200)
ggsave(path.box, plot.box, width=7, height=7, dpi=200)
ggsave(path.country, plot.country, width=7, height=7, dpi=200)
