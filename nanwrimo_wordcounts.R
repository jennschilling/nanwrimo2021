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
library(scales)
library(patchwork)

#### Data ####

word_counts <- read_csv("word_counts.csv", lazy = FALSE) %>% clean_names()

word_counts <- word_counts %>%
  mutate(date = mdy(date),
         month = month(date, label = TRUE, abbr = FALSE),
         day = day(date),
         week = as.numeric(format(date, "%U")),
         weekday = weekdays(date),
         weekday = factor(weekday, c("Monday", "Tuesday",
                                     "Wednesday", "Thursday", "Friday",
                                     "Saturday", "Sunday")))

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

cal <- ggplot(data = word_counts %>% filter(day <= 7),
       mapping = aes(x = weekday,
                     y = 44,
                     fill = count,
                     label = paste(comma(count), "words", sep = "\n"))) +
  geom_tile(color = "#FFFFFF") +
  geom_text(family = font,
            color = "#FFFFFF",
            size = 5) +
  scale_y_reverse() +
  scale_fill_gradient(low = nanowrimo_blue,
                      high = fontcolor) +
  scale_x_discrete(position = "top") +
  guides(fill = "none") +
  coord_cartesian(expand = FALSE,
                  clip = "off") +
  labs(x = "",
       y = "",,
       subtitle = "Number of words per day") +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 14, 
                                   color = fontcolor,
                                   family = font),
        plot.title = element_markdown(size = 20, 
                                      color = fontcolor, 
                                      family = font))

line <- ggplot(data = word_counts %>% filter(day <= 7),
       mapping = aes(x = date,
                     y = total,
                     group = 1)) +
  geom_line(color = fontcolor,
            size = 2) +
  geom_point(data = word_counts %>% filter(day == 7),
             size = 5,
            # shape = "★"
            ) +
  geom_text(data = word_counts %>% filter(day == 7),
            mapping = aes(label = paste(comma(total), "words", sep = "\n")),
            family = font,
            color = nanowrimo_green,
            size = 5,
            vjust = -0.4,
            nudge_x = 0) +
  scale_x_continuous(limits = c(as_date("2021-11-01"), as_date("2021-11-07"))) +
  coord_cartesian(expand = TRUE,
                  clip = "off") +
  labs(subtitle = "Total words over the first week") +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20))

cal / line +
  inset_element(logo,
                left = 1,
                bottom = 0.1,
                right = 0.85,
                top = 0.6,
                clip = FALSE,
                on_top = TRUE,
                align_to = "full") +
  plot_annotation(title = "Week 1 of National Novel Writing Month, November 2021",
                  caption = "<b>Logo:</b> Image courtesy of National Novel Writing Month | <b>Data & Design:</b> Jenn Schilling")


# Save
ggsave("week_1.png",
       plot = last_plot(),
       device = "png",
       width = 9,
       height = 5,
       type = "cairo")
