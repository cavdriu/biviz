# function
plot_test <- function(data, x, y, fill) {

  plot <-
    ggplot(data = data,
           mapping = aes(x = {{ x }}, y = {{ y }}, fill = {{ fill }})) +
    geom_col(position = "dodge")

  #cond <- enquo(y)

  # if (is.numeric(y)) {
  #   scale <-
  #     scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
  #                        breaks = scales::breaks_extended(),
  #                        labels = scales::label_number(big.mark = "'"))
  # } else {
  #   scale <-
  #     scale_y_discrete(expand = expansion(mult = c(0, 0.05)),
  #                      breaks = scales::breaks_extended(),
  #                      labels = scales::label_number(big.mark = "'"))
  # }

  plot +
    #scale +
    cowplot::theme_minimal_hgrid()
}
