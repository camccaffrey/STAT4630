
#
# File: eda.R
# Description: basic EDA, built off of Milestone 2
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
train <- readRDS(TRAIN_PATH)
train.long <- readRDS(TRAINLONG_PATH)

# load themes
source(file.path("Scripts", "themes.R"))

# create display names
display <- setNames(data.dict$Display, data.dict$Alias)
rev.display <- setNames(data.dict$Alias, data.dict$Display)


# Correlation Matrix -----------------------------------------------------------

# M.high <- train %>%
#   filter(expensive == 1) %>%
#   rename(all_of(rev.display)) %>%
#   select(where(is.numeric)) %>%
#   na.omit() %>%
#   cor()
# 
# M.low <- train %>%
#   filter(expensive == 0) %>%
#   rename(all_of(rev.display)) %>%
#   select(where(is.numeric)) %>%
#   na.omit() %>%
#   cor()
# 
# m.combine <- function(m1, m2, order) {
#   if (nrow(m1) != ncol(m1) || nrow(m2) != ncol(m2) || nrow(m1) != nrow(m2)) {
#     stop("Both matrices must be square and have the same dimensions.")
#   }
#   
#   m1 <- abs(m1[, order])
#   m2 <- -abs(m2[, order])
#   
#   n <- nrow(m1)
#   names <- colnames(m1)
#   
#   comb <- matrix(NA, nrow = n, ncol = n)
#   comb[lower.tri(comb, diag = TRUE)] <- m1[lower.tri(m1, diag = TRUE)]
#   comb[upper.tri(comb, diag = TRUE)] <- m2[upper.tri(m2, diag = TRUE)]
#   
#   colnames(comb) <- names
#   rownames(comb) <- names
#   return(comb)
# }

corr.train <- train %>%
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

# my.order <- unique(plot.corr.train$corrPos$xName)
# M <- m.combine(M.high, M.low, my.order)
# 
# plot.corr <- corrplot(M,
#                       title = "Correlation Matrix of Commodity Prices",
#                       method = "circle",
#                       type = "full",
#                       diag = FALSE,
#                       #order = 'alpha',
#                       outline = FALSE,
#                       col = COL2("PuOr", 10),
#                       tl.col = "black",
#                       tl.srt = 45,
#                       tl.cex = 0.4)
# 
# plot.corr
# 
# 
# plot.diff <- corrplot(abs(M.high) - abs(M.low),
#                       title = "Correlation Matrix of Commodity Prices",
#                       method = "circle",
#                       type = "full",
#                       diag = FALSE,
#                       order = 'FPC',
#                       outline = FALSE,
#                       col = COL2("PuOr", 10),
#                       tl.col = "black",
#                       tl.srt = 45,
#                       tl.cex = 0.4)
# 
# 
# 
# plot.diff


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
              color = "darkgray", size = 4)
}

# create plot
plot.scatter <- train %>%
  # data cleaning
  select(all_of(COLS2), expensive) %>%
  na.omit() %>%
  # create scatterplot matrix
  ggpairs(columns = 1:5, aes(colour = expensive, alpha = 0.4),
          upper = list(continuous = wrap(upperfun)),
          lower = "blank",
          labeller = as_labeller(display)) +
  scatter.theme + labs(title = scatter.title)

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
  box.theme + labs(title = box.title)

plot.box



# Country Box Plots ------------------------------------------------------------

# create custom title
country.title = "**Distribution of Average Monthly Salary by Country**"

# create plot
plot.country <- train %>%
  # data cleaning
  group_by(country) %>%
  filter(country != "Other") %>%
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
