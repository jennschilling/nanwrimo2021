# National Novel Writing Month Weekly Word Counts
# Author: Jenn Schilling
# November 2022

# Note: I should probably make a function to generate the weekly plot rather 
# than copying the code each week. Something for next time.

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

word_counts <- read_csv("word_counts_22.csv", lazy = FALSE) %>% clean_names()

word_counts <- word_counts %>%
  mutate(date = mdy(date),
         month = month(date, label = TRUE, abbr = FALSE),
         day = day(date),
         week = as.numeric(format(date, "%U")),
         weekday = weekdays(date),
         weekday = factor(weekday, c("Tuesday",
                                     "Wednesday", "Thursday", "Friday",
                                     "Saturday", "Sunday", "Monday")),
         time_label = case_when(
           is.na(time) ~ "0\nminutes",
           time >= 120 ~ paste("2 hours\n", time-120, " minutes", sep = ""),
           time >= 60 ~ paste("1 hour\n", time-60, " minutes", sep = ""),
           TRUE ~ paste(time, "\nminutes", sep = "")),
         word_label = ifelse(is.na(count), "0\nwords",
                             paste(comma(count, accuracy = 1), "words", sep = "\n")))

# NaNoWriMo Logo (https://nanowrimo.org/press#logos)
logo <- image_read("https://nanowrimo.org/images/logged-out/crest-a0660d7655ffe1e6558965e5d95827da.png")
# Image courtesy of National Novel Writing Month.

logo <- rasterGrob(logo, interpolate = TRUE)

# Goal
goal <- tibble(
  date = as_date("2022-11-30"),
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
                                   lineheight = 1),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, 
                                  color = fontcolor, 
                                  hjust = 1.04),
  
  plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
)


#### Week 1 Plot ####

cal_1 <- ggplot(data = word_counts %>% filter(day <= 7),
                mapping = aes(x = weekday,
                              y = 44,
                              fill = count,
                              label = word_label)) +
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
       subtitle = "I wrote the most on Sunday Nov 6 and the least on Wednesday Nov 2, when I did not write at all.<br>") +
  theme()

cal_2 <- ggplot(data = word_counts %>% filter(day <= 7),
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
       subtitle = "I spent the longest time writing on Thursday Nov 3 and Sunday Nov 6 and the shortest on Tuesday<br>Nov 1, when I used some of my existing writing to get started.<br>") +
  theme()

line <- ggplot(data = word_counts %>% filter(day <= 7),
               mapping = aes(x = date,
                             y = total,
                             group = 1)) +
  geom_line(color = fontcolor,
            size = 2) +
  geom_point(data = word_counts %>% filter(day == 7),
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
            nudge_x = -3.2,
            lineheight = 0.8) +
  annotate("curve",
           x = as_date("2022-11-06"), xend = as_date("2022-11-07"),
           y = 18000, yend = 15500,
           curvature = -0.3, arrow = arrow(length = unit(2, "mm")),
           color = fontcolor, size = 1) + 
  geom_segment(data = goal,
               mapping = aes(x = date, xend = date,
                             y = 0, yend = 49500),
               size = 2,
               color = nanowrimo_green) +
  geom_text(data = goal,
            mapping = aes(label = paste("Goal:", comma(total), sep = " ")),
            family = font,
            color = nanowrimo_green,
            size = 5,
            vjust = -0.5) +
  scale_x_continuous(limits = c(as_date("2022-11-01"), as_date("2022-11-30"))) +
  scale_y_continuous(limits = c(0, 50000)) +
  coord_cartesian(expand = TRUE,
                  clip = "off") +
  labs(subtitle = paste("So far I have written", comma(word_counts[7,]$total), "words in 7 days.",
                        sep = " ")) +
  theme(axis.text.x = element_blank())

title <- ggplot() +
  annotate("text",
           x = 11.4,
           y = 0,
           label = "Week 1 of National Novel Writing Month, November 2022",
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
ggsave("week_1_22.png",
       plot = last_plot(),
       width = 10,
       height = 10)


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
       subtitle = "I wrote the most words on Thursday Nov 10 and the fewest words on Monday Nov 14.<br>") +
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
       subtitle = "I spent the longest time writing on Friday Nov 11 when I started a new chapter and worked on outlining.<br>") +
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
           x = as_date("2022-11-06"), xend = as_date("2022-11-07"),
           y = 18000, yend = 15500,
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
           x = as_date("2022-11-13"), xend = as_date("2022-11-14"),
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
  scale_x_continuous(limits = c(as_date("2022-11-01"), as_date("2022-11-30"))) +
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
           label = "Week 2 of National Novel Writing Month, November 2022",
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
ggsave("week_2_22.png",
       plot = last_plot(),
       device = "png",
       width = 10,
       height = 10,
       type = "cairo")



#### Week 3 Plot ####

cal_1 <- ggplot(data = word_counts %>% filter(day <= 21 & day > 14),
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
       subtitle = "I wrote the most words on Saturday Nov 20 and the fewest words on Sunday Nov 21.<br>") +
  theme()

cal_2 <- ggplot(data = word_counts %>% filter(day <= 21 & day > 14),
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
       subtitle = "I spent the longest time writing on Saturday Nov 20 and the shortest time writing on Sunday Nov 21.<br>") +
  theme()

line <- ggplot(data = word_counts %>% filter(day <= 21),
               mapping = aes(x = date,
                             y = total,
                             group = 1)) +
  geom_line(color = fontcolor,
            size = 2) +
  geom_point(data = word_counts %>% filter(day == 14 | day == 7 | day == 21),
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
           x = as_date("2022-11-06"), xend = as_date("2022-11-07"),
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
           x = as_date("2022-11-13"), xend = as_date("2022-11-14"),
           y = 29500, yend = 26500,
           curvature = -0.2, arrow = arrow(length = unit(2, "mm")),
           color = fontcolor, size = 1) + 
  geom_text(data = word_counts %>% filter(day == 21), 
            mapping = aes(label = paste("Week 3:\n", 
                                        comma(total - word_counts[14,]$total), 
                                        "words", sep = " ")),
            family = font,
            color = fontcolor,
            size = 4.5,
            vjust = -0.5,
            nudge_x = -3.2) +
  annotate("curve",
           x = as_date("2022-11-20"), xend = as_date("2022-11-21"),
           y = 40500, yend = 37600,
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
  scale_x_continuous(limits = c(as_date("2022-11-01"), as_date("2022-11-30"))) +
  scale_y_continuous(limits = c(0, 50000)) +
  coord_cartesian(expand = TRUE,
                  clip = "off") +
  labs(subtitle = paste("So far I have written", comma(word_counts[21,]$total), "words in 21 days.",
                        sep = " ")) +
  theme(axis.text.x = element_blank())

title <- ggplot() +
  annotate("text",
           x = 11.4,
           y = 0,
           label = "Week 3 of National Novel Writing Month, November 2022",
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
ggsave("week_3_22.png",
       plot = last_plot(),
       device = "png",
       width = 10,
       height = 10,
       type = "cairo")

#### Week 4 Plot ####

cal_1 <- ggplot(data = word_counts %>% filter(day <= 28 & day > 21),
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
       subtitle = "I wrote the most words on Monday Nov 22 and the fewest words on Wednesday Nov 24.<br>") +
  theme()

cal_2 <- ggplot(data = word_counts %>% filter(day <= 28 & day > 21),
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
       subtitle = "I spent the longest time writing on Monday Nov 22 and the shortest time writing on Saturday Nov 27.<br>") +
  theme()

line <- ggplot(data = word_counts %>% filter(day <= 28),
               mapping = aes(x = date,
                             y = total,
                             group = 1)) +
  geom_line(color = fontcolor,
            size = 2) +
  geom_point(data = word_counts %>% filter(day == 14 | day == 7 | day == 21 | day == 28),
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
           x = as_date("2022-11-06"), xend = as_date("2022-11-07"),
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
           x = as_date("2022-11-13"), xend = as_date("2022-11-14"),
           y = 29500, yend = 26500,
           curvature = -0.2, arrow = arrow(length = unit(2, "mm")),
           color = fontcolor, size = 1) + 
  geom_text(data = word_counts %>% filter(day == 21), 
            mapping = aes(label = paste("Week 3:\n", 
                                        comma(total - word_counts[14,]$total), 
                                        "words", sep = " ")),
            family = font,
            color = fontcolor,
            size = 4.5,
            vjust = -0.5,
            nudge_x = -3.2) +
  annotate("curve",
           x = as_date("2022-11-20"), xend = as_date("2022-11-21"),
           y = 40500, yend = 37600,
           curvature = -0.2, arrow = arrow(length = unit(2, "mm")),
           color = fontcolor, size = 1) + 
  geom_text(data = word_counts %>% filter(day == 28), 
            mapping = aes(label = paste("Week 4:\n", 
                                        comma(total - word_counts[21,]$total), 
                                        "words", sep = " ")),
            family = font,
            color = fontcolor,
            size = 4.5,
            vjust = -0.5,
            nudge_x = -3.2) +
  annotate("curve",
           x = as_date("2022-11-27"), xend = as_date("2022-11-28"),
           y = 52500, yend = 49400,
           curvature = -0.2, arrow = arrow(length = unit(2, "mm")),
           color = fontcolor, size = 1) + 
  geom_segment(data = goal,
               mapping = aes(x = date, xend = date,
                             y = 0, yend = 50000),
               size = 2,
               color = nanowrimo_green) +
  geom_text(data = goal,
            mapping = aes(label = paste("Goal:", comma(total), sep = "\n")),
            family = font,
            color = nanowrimo_green,
            size = 5,
            vjust = -0.25) +
  scale_x_continuous(limits = c(as_date("2022-11-01"), as_date("2022-11-30"))) +
  scale_y_continuous(limits = c(0, 53000)) +
  coord_cartesian(expand = TRUE,
                  clip = "off") +
  labs(subtitle = paste("So far I have written", comma(word_counts[28,]$total), "words in 28 days.",
                        sep = " ")) +
  theme(axis.text.x = element_blank())

title <- ggplot() +
  annotate("text",
           x = 11.4,
           y = 0,
           label = "Week 4 of National Novel Writing Month, November 2022",
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
ggsave("week_4_22.png",
       plot = last_plot(),
       device = "png",
       width = 10,
       height = 10,
       type = "cairo")
