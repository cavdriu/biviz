# https://github.com/emitanaka/wearerladies/blob/master/day3.md

ggplot(aes(x = year, y = value, group = service, color = service)) +
  geom_line() +
  scale_x_discrete(
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    #limits = c(0, max(data_socsec$value) + 2000),
    breaks = scales::pretty_breaks(),
    labels = scales::number,
    #expand = c(0, 0),
    sec.axis = dup_axis(
      breaks = data_socsec_last$value,
      labels = data_socsec_last$service,
      name = NULL)
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14))

# -------------------------------------------------------------------------

## plot male vs. female (overview)
data_socsec_gender %>% 
  filter(gender != "Total" & service != "Total") %>% 
  ggplot(aes(x = year, y = value, fill = gender)) +
  geom_col(position = "fill") + 
  geom_hline(aes(yintercept = 0.5), linetype = "dotted") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(~service) +
  theme_minimal() +
  theme(
    #legend.position = "none",
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
    plot.title = element_text(size = 14))

# -------------------------------------------------------------------------

ggplot(aes(x = year, y = value, fill = age)) +
  geom_area(aes(x = year, y = value_total), fill = "grey",  alpha = 0.8, color = "grey") + 
  geom_area() +
  geom_line(color = "black") +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(
    breaks = scales::pretty_breaks(),
    labels = scales::number,
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    breaks = c(1900:2200)
  ) +
  facet_wrap(~age) +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size = 14))

# # vergleich zu stacked
# data_socsec_age %>%
#   filter(age != "Total" & service != "Total", service == "ALV") %>%
#   ggplot(aes(x = as.numeric(year), y = value, fill = age)) +
#   geom_area() +
#   scale_fill_brewer(palette = "Dark2") +
#   theme_minimal() +
#   theme(
#     legend.position="none",
#     plot.title = element_text(size = 14))

# -------------------------------------------------------------------------

data_socsec_gender %>% 
  filter(service == "Total") %>% 
  ggplot() +
  
  # add mean and standard deviation for groups
  geom_rect(aes(xmin = stats_m$mean_neg, xmax = stats_m$mean_pos, ymin = 2019.5, ymax = 2009.5), 
            fill = "#D95F02", alpha = 0.03) +
  geom_vline(xintercept = stats_m$mean, linetype = "solid", size = 0.5, alpha = 0.8, color = "#D95F02") +
  geom_rect(aes(xmin = stats_f$mean_neg, xmax = stats_f$mean_pos, ymin = 2019.5, ymax = 2009.5), 
            fill = "#1B9E77", alpha = 0.03) +  
  geom_vline(xintercept = stats_f$mean, color = "#1B9E77", linetype = "solid",  size = 0.5, alpha = 0.8) +
  
  # add range between groups
  geom_segment(data = data_socsec_m,
               aes(x = value, y = year,
                   xend = data_socsec_f$value, yend = data_socsec_f$year),
               color = "#aeb6bf",
               size = 4.5, # segment muss zu den punkten passen
               alpha = 0.8) +
  geom_point(aes(x = value, y = year, color = gender), 
             size = 4) +
  # color points
  scale_color_manual(values = c("#1B9E77","#D95F02"))+
  geom_text(data = diff_gender,
            aes(label = paste("Diff. ", diff), x = x_pos, y = year),
            #fill = "white",
            #family = "Times New Roman"
            color = "#4a4e4d",
            size = 2.5) +
  #add annotations for mean and standard deviations
  geom_text(x = stats_m$mean - 1000, y = 2019.3, label = "MEAN", angle = 90, size = 2.5, color = "#D95F02") + #family = "Segoe UI"
  geom_text(x = stats_m$mean_pos - 1000, y = 2019.3, label = "STDEV", angle = 90, size = 2.5, color = "#D95F02") +  #family = "Segoe UI""
  
  # adjust panel
  scale_y_continuous(expand = c(0, 0),
                     breaks = data_socsec_gender$year,
                     labels = data_socsec_gender$year) +
  
  #coord_flip() +
  
  #scale_x_continuous(breaks = scales::pretty_breaks()) +
  theme(#panel.grid.major.x = element_line(color = "#e3e2e2", linetype = "dashed"),
    #panel.grid.minor.x = element_line(color = "#e3e2e2", linetype = "dashed"),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "#e3e2e2"),
    axis.line.x = element_line(color = "#e3e2e2"),
    text = element_text(color = "#4a4e4d"), #family = "Segoe UI"
    strip.text.y.left  = element_text(angle = 0),
    panel.background = element_rect(fill = "white", color = "white"),
    strip.background = element_rect(fill = "black", color = "white"),
    strip.text = element_text(color = "#4a4e4d"),#, family = "Segoe UI"
    plot.background = element_rect(fill = "white", color = "white"),
    panel.spacing = unit(0, "lines"),
    plot.margin = margin(1,1,0.5,1, "cm"))