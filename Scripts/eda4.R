
#
# File: eda4.R
# Description: EDA for Milestone 4, built off of Milestone 3
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
library(faraway)
library(kableExtra)

# load data
data.dict <- read.csv(DICT_PATH)
train <- readRDS(EDA_PATH)
train.long <- readRDS(EDALONG_PATH)

# load themes
source(file.path("Scripts", "themes.R"))

# create display names
display <- setNames(data.dict$Display, data.dict$Alias)
rev.display <- setNames(data.dict$Alias, data.dict$Display)

# get numeric columns
num.cols <- which(sapply(train, is.numeric))


# Correlation Matrix -----------------------------------------------------------

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
med.domestic <- median(train$salary[train$usa == 1], na.rm=TRUE)
med.foreign <- median(train$salary[train$usa == 0], na.rm=TRUE)

# create plot
plot.hist <- train %>%
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

cormat <- train %>%
  dplyr::select(all_of(num.cols)) %>%
  cor(use="complete.obs")

cors <- sort(cormat[54, -54], TRUE)
df.cors <- data.frame(predictor=names(cors), corr=cors)


# custom title
salary.scatter.title <- createTitle(
  title = "**Monthly Salary vs. Common Commodity Prices**"
)

# create plot
plot.scatter <- train %>%
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
model.full <- lm(salary ~., data=train[num.cols])
vifs <- faraway::vif(model.full)
vifs <- vifs[vifs >= 5]
vif.table <- cbind("VIF"=round(sort(vifs, TRUE), 2))
rownames(vif.table) <- display[rownames(vif.table)]

# output table
vif.table %>%
  kbl(caption = "Variance Inflation Factors") %>%
  kable_classic(full_width = FALSE, html_font = "Times New Roman")


# Save Plots -------------------------------------------------------------------

# define paths
path.hist <- file.path(ROOT, "Output", "histogram.png")
path.scatter <- file.path(ROOT, "Output", "salary-scatter.png")

# default DPI resolution in config.R
ggsave(path.hist, plot.hist, width=7, height=7, dpi=200)
ggsave(path.scatter, plot.scatter, width=7.5, height=12.5, dpi=200)

