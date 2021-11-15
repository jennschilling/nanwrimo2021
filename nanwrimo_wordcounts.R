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
                                     "Saturday", "Sunday")),
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
  plot.title = element_markdown(size = 20, color = fontcolor, family = titlefont),
  
  plot.subtitle = element_markdown(size = 14, color = fontcolor, family = font, lineheight = 1.5),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor, hjust = 1.04),
  
  plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
)


#### Week 1 Plot ####

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
       y = "",
       subtitle = "I wrote the most on Saturday Nov. 6 and the least on Wednesday Nov. 3.") 

line <- ggplot(data = word_counts %>% filter(day <= 7),
       mapping = aes(x = date,
                     y = total,
                     group = 1)) +
  geom_line(color = fontcolor,
            size = 2) +
  geom_point(data = word_counts %>% filter(day == 7),
             size = 5,
             color = fontcolor) +
  geom_text(data = word_counts %>% filter(day == 7),
            mapping = aes(label = paste(comma(total), "total words written in Week 1.", sep = " ")),
            family = font,
            color = fontcolor,
            size = 5,
            vjust = -1.5,
            nudge_x = -1) +
  scale_x_continuous(limits = c(as_date("2021-11-01"), as_date("2021-11-07"))) +
  coord_cartesian(expand = TRUE,
                  clip = "off") +
  theme(axis.text.x = element_blank())

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


#### Week 2 Plot ####

cal_1 <- ggplot(data = word_counts %>% filter(day <= 14 & day > 7),
              mapping = aes(x = weekday,
                            y = 44,
                            fill = count,
                            label = paste(comma(count, accuracy = 1),
                                          "words", sep = "\n"))) +
  geom_tile(color = "#FFFFFF") +
  geom_text(family = font,
            color = "#FFFFFF",
            size = 5) +
  scale_y_reverse() +
  scale_fill_gradient(low = darker_blue,
                      high = fontcolor) +
  scale_x_discrete(position = "top") +
  guides(fill = "none") +
  coord_cartesian(expand = FALSE,
                  clip = "off") +
  labs(x = "",
       y = "",
       subtitle = "I wrote the most on Sunday Nov 14 and the least on Wednesday Nov 10.<br>") +
  theme()

cal_2 <- ggplot(data = word_counts %>% filter(day <= 14 & day > 7),
                mapping = aes(x = weekday,
                              y = 44,
                              fill = time,
                              label = time_label)) +
  geom_tile(color = "#FFFFFF") +
  geom_text(family = font,
            color = "#FFFFFF",
            size = 5) +
  scale_y_reverse() +
  scale_fill_gradient(low = darker_blue,
                      high = fontcolor) +
  scale_x_discrete(position = "top") +
  guides(fill = "none") +
  coord_cartesian(expand = FALSE,
                  clip = "off") +
  labs(x = "",
       y = "",
       subtitle = "I spent the most time writing on Monday Nov 8 and the least time writing on Thursday Nov 11.<br>") +
  theme()

line <- ggplot(data = word_counts %>% filter(day <= 14),
               mapping = aes(x = date,
                             y = total,
                             group = 1)) +
  geom_line(color = fontcolor,
            size = 2) +
  geom_point(data = word_counts %>% filter(day == 14 | day == 7),
             # shape = "★",
             size = 5,
             color = fontcolor) +
  geom_text(data = word_counts %>% filter(day == 7), 
            mapping = aes(label = paste("Week 1:\n", comma(total), 
                                        "words", sep = " ")),
            family = font,
            color = fontcolor,
            size = 4.5,
            vjust = -0.5,
            nudge_x = -3.2) +
  annotate("curve",
           x = as_date("2021-11-06"), xend = as_date("2021-11-07"),
           y = 18000, yend = 14500,
           curvature = -0.2, arrow = arrow(length = unit(2, "mm")),
           color = fontcolor, size = 1) + 
  geom_text(data = word_counts %>% filter(day == 14), 
            mapping = aes(label = paste("Week 2:\n", 
                                        comma(total - word_counts[7,]$total), 
                                        "words", sep = " ")),
            family = font,
            color = fontcolor,
            size = 4.5,
            vjust = -0.5,
            nudge_x = -3.2) +
  annotate("curve",
           x = as_date("2021-11-13"), xend = as_date("2021-11-14"),
           y = 29500, yend = 26500,
           curvature = -0.2, arrow = arrow(length = unit(2, "mm")),
           color = fontcolor, size = 1) + 
  geom_segment(data = goal,
               mapping = aes(x = date, xend = date,
                             y = 0, yend = 49500),
               size = 2,
               color = nanowrimo_green) +
  geom_text(data= goal,
            mapping = aes(label = paste("Goal:", comma(total), sep = " ")),
            family = font,
            color = nanowrimo_green,
            size = 5,
            vjust = -0.5) +
  scale_x_continuous(limits = c(as_date("2021-11-01"), as_date("2021-11-30"))) +
  scale_y_continuous(limits = c(0, 50000)) +
  coord_cartesian(expand = TRUE,
                  clip = "off") +
  labs(subtitle = paste("So far I have written", comma(word_counts[14,]$total), "words in 14 days.",
                        sep = " ")) +
  theme(axis.text.x = element_blank())

title <- ggplot() +
  annotate("text",
           x = 11.4,
           y = 0,
           label = "Week 2 of National Novel Writing Month, November 2021",
           family = font,
           color = fontcolor,
           size = 8,
           hjust = 1) +
  annotation_custom(logo,
                    xmin = 8,
                    xmax = 18,
                    ymin = -Inf,
                    ymax = Inf) +
  scale_x_continuous(limits = c(-10, 12)) +
  coord_cartesian(clip = "off") +
  theme(axis.text.x = element_blank(),
        plot.margin = margin(0, 0, 0, 0))


title / line / cal_1 / cal_2  +
  plot_annotation(caption = "<b>Logo:</b> Image courtesy of National Novel Writing Month 
                  | <b>Data & Design:</b> Jenn Schilling") +
  plot_layout(heights = c(1, 2, 1, 1))
  


# Save
ggsave("week_2.png",
       plot = last_plot(),
       device = "png",
       width = 10,
       height = 10,
       type = "cairo")
