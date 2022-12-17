df <-
  rad_auftrage_2022_bearbeitungszeit |>
  pivot_wider(names_from = bearbeitungsfrist_3_tage, values_from = n) |>
  rename(frist_eingehalten = `frist eingehalten`,
         frist_nicht_eingehalten = `frist nicht eingehalten`)


df2 <-
  rad_auftrage_2022_bearbeitungszeit |>
  group_by(bearbeitungsfrist_3_tage, abschlussdatum_month) |> # abschlussdatum_woche
  summarize(n = n()) |>
  ungroup()


colors <- c("frist_nicht_eingehalten" = "grren", "frist_nicht_eingehalten" = "red")

ggplot(df) +
  geom_segment(mapping = aes(x = frist_eingehalten,
                             xend = frist_nicht_eingehalten,
                             y = abschlussdatum_woche,
                             yend = abschlussdatum_woche),
               color = "grey") +
  geom_point(mapping = aes(x = frist_eingehalten,
                           y = abschlussdatum_woche),
             color = "green",
             size = 3) +
  geom_point(mapping = aes(x = frist_nicht_eingehalten,
                           y = abschlussdatum_woche),
             color = "red",
             size = 3) +
  scale_y_date(date_breaks = "1 week",
               date_labels = "%W") +
  # scale_color_manual(values = colors) +
  # labs(color = "Legend") +
  coord_flip() +
  theme_classic()




ggplot(data = rad_auftrage_2022_bearbeitungszeit,
       mapping = aes(x = abschlussdatum_woche, y = n, color = bearbeitungsfrist_3_tage, group = bearbeitungsfrist_3_tage)) +
  geom_line() +
  geom_point(aes(shape = bearbeitungsfrist_3_tage)) +
  # labs(
  #   title = "Entwicklung Aufträge RAD in Zusammenhang mit IVG Art. 16 und 17 im Jahr 2022",
  #   subtitle = "IVG Art. 16 und 17 werden anhand der Zuteilung durch Berufsberatende eruiert"
  # ) +
  scale_x_date(date_breaks = "1 week",
               date_labels = "%W") +
  theme_bw()

# level anpassen wegen den farben im plot (negativ soll rot sein)
bearbeitungsfrist <- fct_relevel(df2$bearbeitungsfrist_3_tage, "frist eingehalten", after = 1)




plot_bearbeitungszeit <-
  rad_auftrage_2022_bearbeitungszeit |>
  group_by(bearbeitungsfrist_3_tage, abschlussdatum_month) |>
  summarize(n = n()) |>
  ungroup() |>
  ggplot(mapping = aes(x = abschlussdatum_month,
                       y = n,
                       fill = fct_relevel(bearbeitungsfrist_3_tage, "frist eingehalten", after = 1))) +
  geom_col(position = "fill") +
  labs(
    title = "Bearbeitungszeit Aufträge RAD in Zusammenhang mit IVG Art. 16 und 17 im Jahr 2022",
    subtitle = "Die Aufträge sollten innerhalb von 3 Tagen bearbeitet werden",
    fill = "Bearbeitungsfrist"
  ) +
  xlab("Monat") +
  ylab(NULL) +
  theme_bw()
