plot_relation <- ggplot(data = data_he_plot,
                        mapping = aes(x = factor(jahr), y = anzahl, fill = leistung)
) +
  geom_col(aes(x = factor(jahr), y = total), fill = "grey", alpha = 0.8, color = "grey") +
  geom_col() +
  facet_grid(cols = vars(leistung)) +
  labs(x = "Jahr",
       y = "Anzahl Entscheide",
       fill = "Sachleistung",
       title = "Anzahl Entscheide HE, HM und MM im Verhältnis zum Total SL",
       subtitle = "Die Daten aus dem Jahr 2022 sind nicht berücksichtigt, da das Jahr noch nicht abgeschlossen ist.\nGrau ist das Total aller Entscheide Sachleistungen.\nQuelle: BSC (inkl. Archiv)"

  ) +
  # scale_fill_discrete(breaks = c("ahv_he", "ahv_hm", "iv_hm", "iv_mm"),
  #                     labels = c("AHV HE", "AHV HM", "IV HM", "IV MM")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     breaks = scales::breaks_pretty(10),
                     labels = scales::label_number(big.mark = "'")
  ) +
  theme_bw() +
  theme(legend.position = "none",
        plot.subtitle = element_text(size = 8)
  )

# -------------------------------------------------------------------------

ggplot(df_plot, aes(x = jahr, y = anzahl, color = bezueger, group = bezueger)) +
  geom_line(color = "blue") +
  geom_point(size = 2, shape = 21, fill = "blue", color = "blue") +
  # geom_text(aes(label = anzahl),
  #           nudge_y = end_scale_y / 50) +
  labs(x = "Jahr",
       y = paste("Anzahl", df_plot$iv_stelle, sep = " ")) +
  scale_y_continuous(
    # limits = c(0, end_scale_y),
    # breaks = seq(0, end_scale_y, by = by_scale_y),
    expand = expansion(add = 0)) +
  theme_bw()

# -------------------------------------------------------------------------

plot_1a <- df_plot_1a %>%
  filter(iv_stelle != "CH") %>%
  ggplot() + # color 1a vs 1b
  geom_point(aes(x = massnahmen, y = quote_renten, color = iv_stelle, size = ind_1a)) +
  geom_hline(aes(yintercept = df_line_1a$quote_renten_mean), linetype = "dotted") +
  annotate("text", x = 8000, y = 10, label = "Durchschnitt der Rentenquote WI 1a") +
  annotate("segment", x = 8000, xend = 8000, y = 9.5, yend = 5.5,
           arrow = arrow(length = unit(2, "mm"))) +
  annotate("text", x = 9800, y = df_zh_1a$quote_renten - 1, label = "ZH") +
  annotate("curve", x = 10200, xend = 10800,
           y = df_zh_1a$quote_renten - 1, yend = df_zh_1a$quote_renten - 0.3,
           curvature = 0.3,
           arrow = arrow(length = unit(2, "mm"))) +
  theme_bw()

# -------------------------------------------------------------------------

plot <- ggplot(data = df,
               aes(x = n_massnahmen,
                   y = n_renten,
                   color = abweichung)) +
  geom_point(alpha = 0.5) +
  geom_point(data = higlight_1,
             aes(x = n_massnahmen,
                 y = n_renten),
             color = "blue") +
  geom_point(data = higlight_2,
             aes(x = n_massnahmen,
                 y = n_renten),
             color = "deeppink") +
  #geom_smooth(se = FALSE) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(subtitle = paste0("Blau: ", higlight_1$iv_stelle[1], " / Pink: ", higlight_2$iv_stelle[1])) +
  theme_bw()


