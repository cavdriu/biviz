df <-
  ggplot2::mpg |>
  group_by(year) |>
  summarise(n = n()) |>
  ungroup() |>
  mutate(percent = n / sum(n),
         # Compute the cumulative percentages (top of each rectangle)
         #ymax = cumsum(percent),
         # Compute the bottom of each rectangle
         #ymin = c(0, head(ymax), n = -1),
         lab_pos = cumsum(percent) - (0.5 * percent),
         group_pos = cumsum(percent) - (0.45 * percent)
         )
  #
df |>
  ggplot(aes(x = 2, y = percent, fill = as.factor(year))) + # fill = "group"
  geom_col(position = "stack", color = "white") +
  coord_polar(theta = "y", start = 0) +
  # values
  geom_text(aes(y = lab_pos, label = n), color = "white") +
  # groups
  geom_text(aes(x = 2.9, y = group_pos, label = as.factor(year)), color = "black") + # lab_pos verwenden
  scale_x_continuous(limits = c(0.5, 3)) +
  #scale_fill_manual(values = colors, guide = NULL) +
  theme_void() +
  #cowplot::theme_map() +
  ggtitle("Test")


# https://semba-blog.netlify.app/07/12/2019/pie-chart-and-donut-plot-with-ggplot2/
# https://github.com/clauswilke/dataviz/blob/master/visualizing_proportions.Rmd Z:31-59



  # rdown.os = rdown %>%
  #   filter(os != "NA") %>%
  #   group_by(os) %>%
  #   count() %>%
  #   ungroup()%>%
  #   arrange(desc(os)) %>%
  #   mutate(percentage = round(n/sum(n),4)*100,
  #          lab.pos = cumsum(percentage)-.5*percentage)

