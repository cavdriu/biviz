#' Balkendiagramme
#'
#' @param data Ein Tibble mit den Daten für den Plot.
#' @param x Die Variable für die x-Achse.
#' @param y Die Variable für die y-Achse.
#'
#' @return ggplot object
#'
#' @importFrom ggplot2 ggplot aes geom_col scale_y_continuous
#' scale_y_discrete expansion coord_flip
#'
#' @examples
#' df <-
#' tibble::tibble(
#'  x = c("a", "b", "c", "d", "e"),
#'  y = 1:5
#' )
#'
#' plot_bar_v(df, x, y)
#'
#'@export plot_bar
plot_bar <- function(data, x, y, color = "#0d7abc") {

  cond <- enquo(y)

  if (is.numeric(cond)) { # ohne !!, da hier base r funktin verwendet wird

    ggplot(data = data,
           mapping = aes(x = {{ x }}, y = {{ y }})) +
      geom_col(fill = color) + # alpha = 0.9
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                       breaks = scales::breaks_extended(),
                       labels = scales::label_number(big.mark = "'")) +
      cowplot::theme_minimal_hgrid()

  } else {

    ggplot(data = data,
           mapping = aes(x = {{ x }}, y = {{ y }})) +
      geom_col(fill = color) + # alpha = 0.9
      scale_y_discrete(expand = expansion(mult = c(0, 0.05)),
                         breaks = scales::breaks_extended(),
                         labels = scales::label_number(big.mark = "'")) +
      cowplot::theme_minimal_hgrid()
  }
}

#' @export
#' @rdname plot_bar
plot_bar_v <- function(data, x, y, color = "#0d7abc") {

  cond <- enquo(y)

  if (is.numeric(cond)) {

  ggplot(data = data,
         mapping = aes(x = {{ x }}, y = {{ y }})) +
    geom_col(fill = color) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                       breaks = scales::breaks_extended(),
                       labels = scales::label_number(big.mark = "'")) +
    coord_flip() + #clip = "off"
    cowplot::theme_minimal_vgrid()

  } else {

    ggplot(data = data,
           mapping = aes(x = {{ x }}, y = {{ y }})) +
      geom_col(fill = color) +
      scale_y_discrete(expand = expansion(mult = c(0, 0.05)),
                     breaks = scales::breaks_extended(),
                     labels = scales::label_number(big.mark = "'")) +
      coord_flip() + #clip = "off"
      cowplot::theme_minimal_vgrid()
  }
}

#' @export
#' @rdname plot_bar
plot_bar_g <- function(data, x, y, fill) {

  cond <- enquo(y)

  if (is.numeric(cond)) {

    ggplot(data = data,
           mapping = aes(x = {{ x }}, y = {{ y }}, fill = {{ fill }})) +
      geom_col(position = "dodge") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                         breaks = scales::breaks_extended(),
                         labels = scales::label_number(big.mark = "'")) +
      #scale_fill_manual(values = colors_four, name = NULL) +
      cowplot::theme_minimal_hgrid()

  } else {

    ggplot(data = data,
           mapping = aes(x = {{ x }}, y = {{ y }}, fill = {{ fill }})) +
      geom_col(position = "dodge") +
      scale_y_discrete(expand = expansion(mult = c(0, 0.05)),
                       breaks = scales::breaks_extended(),
                       labels = scales::label_number(big.mark = "'")) +
      #scale_fill_manual(values = colors_four, name = NULL) +
      cowplot::theme_minimal_hgrid()

  }
}

  # ggplot(data = data,
  #        mapping = aes(x = {{ x }}, y = {{ y }})) +
  #   geom_col(position = "dodge") +
  #   scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
  #                      breaks = scales::breaks_extended(),
  #                      labels = scales::label_number(big.mark = "'")) +
  #   #scale_fill_manual(values = colors_four, name = NULL) +
  #   cowplot::theme_minimal_hgrid()
  #
  # }
#' @export
plot_test <- function(data, x, y, fill) {

  plot <-
    ggplot(data = data,
                 mapping = aes(x = {{ x }}, y = {{ y }}, fill = {{ fill }})) +
    geom_col(position = "dodge")

    cond <- rlang::eval_tidy(substitute(y), data = data)
    cond2 <- rlang::eval_tidy(rlang::enquo(y), data = data)
    print(cond2)

    if (is.numeric(cond)) {
      scale <-
        scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                           breaks = scales::breaks_extended(),
                           labels = scales::label_number(big.mark = "'"))
    } else {
      scale <-
        scale_y_discrete(expand = expansion(mult = c(0, 0.05)),
                         breaks = scales::breaks_extended(),
                         labels = scales::label_number(big.mark = "'"))
    }

    plot +
      scale +
      cowplot::theme_minimal_hgrid()
}
