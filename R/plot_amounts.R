#' Mengen visualisieren
#'
#' @param data Ein Tibble mit den Daten für den Plot.
#' @param x Die Variable für die x-Achse.
#' @param y Die Variable für die y-Achse.
#' @param group Gruppierungsvariable für einen einzelnen Plot.
#' @param facet Gruppierungsvariable um mehrere Plots zu machen (Facetten).
#' @param color Farbe der Balken. Default = #0d7abc.
#' @param n_col Anzahl Spalten bei den Facetten. Default = 2.
#'
#' @return ggplot object
#'
#' @importFrom ggplot2
#' ggplot aes geom_col geom_text
#' scale_y_continuous scale_y_discrete scale_fill_manual expansion coord_flip
#' facet_wrap vars
#' theme element_blank
#'
#' @examples
#' # Test Daten als Beispiel verwenden
#' df <-
#' tibble::tibble(
#'  x = c("a", "b", "c", "d", "e"),
#'  y = 1:5
#' )
#'
#' plot_amounts(df, x, y)
#'
#'@export plot_amounts
plot_amounts <- function(data, x, y, color = "#0d7abc") {

  plot <-
    ggplot(data = data,
           mapping = aes(x = {{ x }}, y = {{ y }})) +
    geom_col(fill = color) # alpha = 0.9

  # testen ob die y-achse numerisch ist, damit die richtige skala verwendet werden kann
  cond <- dplyr::pull(data, {{ y }})

    if (is.numeric(cond)) {
      scale <-
        scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                           breaks = scales::breaks_extended(),
                           labels = scales::label_number(big.mark = "'"))
    } else {
      scale <-
        scale_y_discrete(expand = expansion(mult = c(0, 0.05)))
    }

    plot +
      scale +
      cowplot::theme_minimal_hgrid()
}


#' @export
#' @rdname plot_amounts
plot_amounts_horizontal <- function(data, x, y, color = "#0d7abc") {

  plot <-
    ggplot(data = data,
         mapping = aes(x = {{ x }}, y = {{ y }})) +
    geom_col(fill = color)

  # testen ob die y-achse numerisch ist, damit die richtige skala verwendet werden kann
  cond <- dplyr::pull(data, {{ y }})

  if (is.numeric(cond)) {
    scale <-
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                         breaks = scales::breaks_extended(),
                         labels = scales::label_number(big.mark = "'"))
  } else {
    scale <-
      scale_y_discrete(expand = expansion(mult = c(0, 0.05)))
  }

    plot +
      scale +
      coord_flip() + # clip = "off"
      cowplot::theme_minimal_vgrid()

}

#' @export
#' @rdname plot_amounts
plot_amounts_grouped <- function(data, x, y, group) {


# TODO: farbwahl bei nur 2 gruppen ----------------------------------------

  # testen ob die group-variable ein factor ist
  group_as_factor <- dplyr::pull(data, {{ group }})

  if (is.factor(group_as_factor)) {
    group_as_factor
  } else {
    group_as_factor <- as.factor(group_as_factor)
  }

  # anzahl levels wird verwendet um die farbpalette auszuwählen
  levels <- nlevels(group_as_factor)
  #colors <- RColorBrewer::brewer.pal(levels, "Blues") #[3:5] #[5:(5-length(levels))]
  colors <- colorspace::qualitative_hcl(levels, palette = "Dark 3")

  plot <-
    ggplot(data = data,
           mapping = aes(x = {{ x }}, y = {{ y }}, fill = {{ group }})) +
      geom_col(position = "dodge")

 # testen ob die y-achse numerisch ist, damit die richtige skala verwendet werden kann
  cond <- dplyr::pull(data, {{ y }})

    if (is.numeric(cond)) {
      scale <-
        scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                           breaks = scales::breaks_extended(),
                           labels = scales::label_number(big.mark = "'"))
    } else {
      scale <-
        scale_y_discrete(expand = expansion(mult = c(0, 0.05)))
    }

    plot +
      scale +
      scale_fill_manual(values = colors, name = NULL) +
      cowplot::theme_minimal_hgrid()
}

#' @export
#' @rdname plot_amounts
plot_amounts_facets <- function(data, x, y, facet, n_col = 2, color = "#0d7abc") {

  plot <-
    ggplot(data = data,
           mapping = aes(x = {{ x }}, y = {{ y }})) +
    geom_col(fill = color)

  # testen ob die y-achse numerisch ist, damit die richtige skala verwendet werden kann
  cond <- dplyr::pull(data, {{ y }})

  if (is.numeric(cond)) {
    scale <-
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                         breaks = scales::breaks_extended(),
                         labels = scales::label_number(big.mark = "'"))
  } else {
    scale <-
      scale_y_discrete(expand = expansion(mult = c(0, 0.05)))
  }

  plot +
    scale +
    facet_wrap(vars({{ facet }}), ncol = n_col, scales = "free_x") +
    cowplot::theme_minimal_hgrid()

}

#' @export
#' @rdname plot_amounts
plot_amounts_stacked <- function(data, x, y, group) {

  # labes berechnen, damit sie in der mitte des jeweiligen blocks sind
  data <-
    data |>
    dplyr::arrange({{ x }}, dplyr::desc({{ group }})) |>
    dplyr::group_by({{ x }}) |>
    dplyr::mutate(n_label = cumsum({{ y }}) - ({{ y }} / 2)) |>
    dplyr::ungroup()

  plot <-
    ggplot(data = data,
           mapping = aes(x = {{ x }}, y = {{ y }}, fill = {{ group }})) +
    # damit die einzelnen blöcke besser unterscheidbar sind, eine weisse linie hinzufügen
    geom_col(position = "stack", color = "white", size = 0.5, width = 1) + # width = 0.9
    geom_text(
      aes(y = n_label, label = n),
      color = "white", size = 4)

  # testen ob die group-variable ein factor ist
  group_as_factor <- dplyr::pull(data, {{ group }})

  if (is.factor(group_as_factor)) {
    group_as_factor
  } else {
    group_as_factor <- as.factor(group_as_factor)
  }

  # anzahl levels wird verwendet um die farbpalette auszuwählen
  levels <- nlevels(group_as_factor)

  # farbpaletten benötigen mindestens 3 werte (n >= 3)
  if (near(levels, 2)) {
    colors <- c("#ff7b39", "#4565b2")
  } else {
    colors <- colorspace::qualitative_hcl(levels, palette = "Dark 3")
  }

  # testen ob die y-achse numerisch ist, damit die richtige skala verwendet werden kann
  cond <- dplyr::pull(data, {{ y }})

  if (is.numeric(cond)) {
    scale <-
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                         breaks = NULL, #scales::breaks_extended(),
                         name = NULL) #scales::label_number(big.mark = "'"))
  } else {
    scale <-
      scale_y_discrete(expand = expansion(mult = c(0, 0.05)),
                       name = NULL)
  }

  plot +
    scale +
    scale_fill_manual(values = colors, name = NULL) +
    cowplot::theme_minimal_hgrid() +
    theme(axis.title.x = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom",
          legend.justification = "center")
}
