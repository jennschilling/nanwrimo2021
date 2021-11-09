# National Novel Writing Month Word Counts
# Author: Jenn Schilling
# November 2021

#### Libraries ####

library(here)
library(tidyverse)
library(extrafont)
library(ggtext)
library(magick)
library(grid)
library(janitor)
library(lubridate)

#### Data ####

word_counts <- read_csv("word_counts.csv") %>% clean_names()

# NaNoWriMo Logo (https://nanowrimo.org/press#logos)
logo <- image_read("https://nanowrimo.org/images/logged-out/crest-a0660d7655ffe1e6558965e5d95827da.png")
# Image courtesy of National Novel Writing Month.

logo <- rasterGrob(logo, interpolate = TRUE)

#### Formatting ####

font <- "Rockwell"
titlefont <- "Rockwell"
fontcolor <- "#2f3061" # from NaNoWriMo wesbite
bcolor <- "#F2F8FA" # from NaNoWriMo website

nanowrimo_blue <- "#93dee8"
nanowrimo_brown <- "#5a2e14"
nanowrimo_green <- "#73ab9b"


theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(fill = bcolor, color = NA),
  plot.background = element_rect(fill = bcolor, color = NA),
  
  axis.title = element_text(size = 10, color = fontcolor),
  axis.text = element_text(size = 10, color = fontcolor),
  axis.ticks = element_line(color = fontcolor),
  
  axis.line = element_line(color = fontcolor),
  
  strip.text = element_text(size = 10, color = fontcolor, hjust = 0),
  
  legend.text = element_text(size = 10, color = fontcolor),
  legend.title = element_text(size = 12, color = fontcolor),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 20, color = fontcolor, family = titlefont),
  
  plot.subtitle = element_markdown(size = 11, color = fontcolor, family = font, lineheight = 1.5),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor, hjust = 1.04),
  
  plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
)


#### Plot ####

ggplot(data = word_counts,
       mapping = aes(x = date,
                     y = count)) +
  geom_col(fill = nanowrimo_brown)
