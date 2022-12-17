#labels <-
  mpg |>
  filter(year == max(year)) |>
  mutate(year = as.factor(year)) |>
  #filter(year == max(year)) |>
  group_by(year, manufacturer) |>
  summarise(hwy_by_manufacturer = mean(hwy)) |>
  ungroup()


df_temp <-
  mpg |>
  mutate(year = as.factor(year)) |>
  group_by(year, manufacturer) |>
  summarise(hwy_by_manufacturer = mean(hwy)) |>
  ungroup() #|>

plot_timeseries_slope(data = df_temp, x = year, y = hwy_by_manufacturer, group = manufacturer)
  #        #cty_by_manufacturer = mean(cty)) |>
  ggplot(aes(x = year, y = hwy_by_manufacturer)) +
  geom_line(aes(group = manufacturer), color = "#b3b3b3a0") +
  # damit zwischen linie und punkt raum entsteht, damit wird ein teil der linie überdekt
  geom_point(color = "white", size = 4) + # position = position_jitter()
  # geom_text(data = labels,
  #           aes(label = manufacturer)) +
  ggrepel::geom_text_repel(data = labels,
                           aes(label = manufacturer),
                           # für die klarheit soll immer eine linie gezeichnet werden
                           min.segment.length = 0,
                           # nudge_y soll automatisch gewählt werden
                           # nudge_x muss einen min. abstand zum x-wert haben
                           #nudge_x = 0.1,
                           direction = "y",
                           hjust = "right") +
  # darstellung des werts, wegen den linien bei geom_text_repel
  # muss dieses geom nach dem geom_text_repel kommmen
  geom_point(color = "#0d7abc", size = 2) +
  scale_x_discrete(expand = expansion(mult = c(0.0, 0.5))) +
  scale_y_continuous(expand = expansion(add = 1)) +
  cowplot::theme_half_open() +
  theme(
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank()
  )


mpg |>
  group_by(year, manufacturer) |>
  summarise(n = n(),
            mean_hwy = mean(hwy)
            ) |>
  pivot_wider(names_from = year, values_from = c(n, mean_hwy))

