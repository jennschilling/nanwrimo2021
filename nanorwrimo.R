# National Novel Writing Month Data Viz
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
library(forcats)

#### Data ####

word_counts <- read_csv("word_counts.csv", lazy = FALSE) %>% clean_names()

word_counts <- word_counts %>%
  mutate(date = mdy(date),
         month = month(date, label = TRUE, abbr = FALSE),
         day = day(date),
         week = as.numeric(format(date, "%U")),
         weekday = weekdays(date, TRUE),
         weekday = factor(weekday, c("Sun", "Mon", "Tue",
                                     "Wed", "Thu", "Fri",
                                     "Sat")),
         time_label = case_when(
           time >= 60 ~ paste("1 hour\n", time-60, " minutes", sep = ""),
           TRUE ~ paste(time, "\nminutes", sep = "")
         ))

# NaNoWriMo Logo (https://nanowrimo.org/press#logos)
logo <- image_read("https://nanowrimo.org/images/logged-out/crest-a0660d7655ffe1e6558965e5d95827da.png")
# Image courtesy of National Novel Writing Month.

logo <- rasterGrob(logo, interpolate = TRUE)

# Goal
goal <- tibble(
  date = as_date("2021-11-30"),
  total = 50000
)

#### Formatting ####

font <- "Rockwell"
titlefont <- "Rockwell"
fontcolor <- "#2f3061" # from NaNoWriMo wesbite
bcolor <- "#F2F8FA" # from NaNoWriMo website

nanowrimo_blue <- "#93dee8"
darker_blue <- "#32c0d3"
nanowrimo_brown <- "#5a2e14"
nanowrimo_green <- "#73ab9b"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(fill = bcolor, color = NA),
  plot.background = element_rect(fill = bcolor, color = NA),
  
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.text.y = element_blank(),
  axis.title = element_blank(),
  
  axis.text.x = element_text(size = 14, 
                             color = fontcolor,
                             family = font),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 20, 
                                color = fontcolor,
                                family = titlefont),
  
  plot.subtitle = element_markdown(size = 14, 
                                   color = fontcolor, 
                                   family = font, 
                                   lineheight = 1.25),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 12, 
                                  color = fontcolor, 
                                  hjust = 1),
  
  legend.text = element_text(color = fontcolor),
  legend.title = element_text(color = fontcolor),
  
  plot.margin = margin(t = 30, r = 20, b = 30, l = 20)
)

#### Plot ####

# Calendar of words per day
words_cal <- ggplot(data = word_counts,
       mapping = aes(x = weekday,
                     y = week,
                     fill = count,
                     label = day)) +
  geom_tile(color = "#FFFFFF") +
  geom_text(family = font,
            size = 8,
            color = "#FFFFFF") +
  scale_y_reverse() +
  scale_fill_gradient(low = nanowrimo_blue,
                      high = fontcolor,
                      labels = comma) +
  scale_x_discrete(position = "top") +
  guides(fill = guide_colorbar(title.position = "top",
                               barwidth = 10,
                               direction = "horizontal")) +
  coord_cartesian(expand = FALSE,
                  clip = "off") +
  labs(x = "",
       y = "",
       fill = "Daily Word Count",
       subtitle = "On the two days I wrote the most words, I spent over 90 minutes writing. These days<br>
       occurred on weeks that were less balanced; I wrote fewer words on some days and<br>more on others.",
       title = "<b>1,672</b> average words per day.") +
  theme(legend.position = c(0.85, 0.05))

# Calender of time per day
time_cal <- ggplot(data = word_counts,
       mapping = aes(x = weekday,
                     y = week,
                     fill = time,
                     label = day)) +
  geom_tile(color = "#FFFFFF") +
  geom_text(family = font,
            size = 8,
            color = "#FFFFFF") +
  scale_y_reverse() +
  scale_fill_gradient(low = nanowrimo_blue,
                      high = fontcolor) +
  scale_x_discrete(position = "top") +
  guides(fill = guide_colorbar(title.position = "top",
                               barwidth = 10,
                               direction = "horizontal")) +
  coord_cartesian(expand = FALSE,
                  clip = "off") +
  labs(x = "",
       y = "",
       fill = "Daily Minutes Writing",
       subtitle = "The words came more easily, and I spent less time writing during the third week and<br>
       the last few days of the month. I struggled more at the start of the month and in the fourth<br>
       week when it took me longer to meet my daily word count goal.",
       title = "<b>67</b> average minutes writing per day.") +
  theme(legend.position = c(0.85, 0.05))

# Calender of word counts per day
speed_cal <- ggplot(data = word_counts,
       mapping = aes(x = weekday,
                     y = week,
                     fill = words_per_minute,
                     label = day)) +
  geom_tile(color = "#FFFFFF") +
  geom_text(family = font,
            size = 8,
            color = "#FFFFFF") +
  scale_y_reverse() +
  scale_fill_gradient(low = nanowrimo_blue,
                      high = fontcolor) +
  scale_x_discrete(position = "top") +
  guides(fill = guide_colorbar(title.position = "top",
                               barwidth = 10,
                               direction = "horizontal")) +
  coord_cartesian(expand = FALSE,
                  clip = "off") +
  labs(x = "",
       y = "",
       fill = "Daily Words Per Minute",
       subtitle = "I wrote the fastest in the middle of the month and end of the month. Beginning the story and<br>
       leading up to the end were my slowest writing times.",
       title = "<b>26</b> average words per minute each day.") +
  theme(legend.position = c(0.85, 0.05))


# Times Per Day
times_bar <- ggplot(data = word_counts,
       mapping = aes(y = times_per_day,
                     label = paste(..count.., " days", sep = ""))) +
  geom_bar(fill = fontcolor) +
  geom_text(stat = "count",
            hjust = -0.1,
            family = font,
            color = fontcolor,
            size = 6)  +
  scale_x_continuous(limits = c(0, 30)) +
  scale_y_reverse(breaks = c(1, 2),
                  labels = c("1 Time", "2 Times")) +
  coord_cartesian(expand = FALSE,
                  clip = "off") +
  labs(title = "I mostly wrote once per day.<br>") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18,
                                   color = fontcolor))

# Time of Day
time_day_bar <- ggplot(data = word_counts,
       mapping = aes(y = fct_rev(fct_infreq(time_of_day)),
                     label = ifelse(..count.. > 1 ,
                                    paste(..count.., " days", sep = ""),
                                    paste(..count.., " day", sep = "")))) +
  geom_bar(fill = fontcolor) +
  geom_text(stat = "count",
            hjust = -0.1,
            family = font,
            color = fontcolor,
            size = 6)  +
  scale_x_continuous(limits = c(0, 30)) +
  coord_cartesian(expand = FALSE,
                  clip = "off") +
  labs(title = "I wrote in the morning most frequently.<br>") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18,
                                   color = fontcolor,
                                   hjust = 1))

# Main Title
title <- ggplot() +
  annotate("text",
           x = -15,
           y = 0,
           label = "November is National Novel Writing Month. Hundreds of thousands of people around the world participate.\nThe goal is to write 50,000 words during the month. In 2021, I participated for the second time. I wrote every\nday during November. I ended the month with a total of 50,179 words after 33 hours and 41 minutes of writing.",
           family = font,
           color = fontcolor,
           size = 10,
           hjust = 0) +
  annotation_custom(logo,
                    xmin = 6,
                    xmax = 16,
                    ymin = -Inf,
                    ymax = Inf) +
  scale_x_continuous(limits = c(-15, 12),
                     expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme(axis.text.x = element_blank(),
        plot.margin = margin(0, 0, 0, 0))


# Put the plots together
cal <- words_cal | time_cal | speed_cal 

bars <- plot_spacer() | times_bar | time_day_bar | plot_spacer() 

title / cal / bars +
  plot_annotation(caption = "<b>Logo:</b> Image courtesy of National Novel Writing Month 
                  | <b>Data & Design:</b> Jenn Schilling | Learn more at nanowrimo.org") +
  plot_layout(heights = c(1, 3, 1))

# Save
ggsave("nanowrimo.png",
       plot = last_plot(),
       device = "png",
       width = 26,
       height = 14,
       type = "cairo")


# Make Instagram carousel version