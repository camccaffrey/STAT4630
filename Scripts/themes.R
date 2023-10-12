
#
# File: themes.R
# Description: define themes to be used in EDA
# 

# GGText Functions -------------------------------------------------------------

# these are used for creating multicolored titles and subtitles (examples in eda.R)
fColor <- function(text, color) {glue("<span style='color:{color};'>{text}</span>")}
fSize <- function(subtitle, size) {glue("<span style='font-size:{size}pt'>{subtitle}</span>")}
createTitle <- function(title, subtitle) {paste(title, "  \n", fSize(subtitle, 11), sep="")}


# Color Hexcodes ---------------------------------------------------------------

RED <- "#F8766D"
BLUE <- "#00BFC4"


# Custom GGplot Thenes ---------------------------------------------------------

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


