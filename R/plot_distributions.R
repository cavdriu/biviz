#' Verteilungen visualisieren
#'
#' @param data Ein Tibble mit den Daten für den Plot.
#' @param variable Variable die untersucht wird (x-Achse).
#' @param bw Breite (Anzahl werte die zusammengefasst werden) der Behälter.
#' @param color Farbe um die Balken optisch zu trennen.
#' @param group Variable um die Daten zu gruppieren.
#' @param size Grösse der Schrift.
#'
#' @return ggplot2 object
#'
#' @importFrom ggplot2
#' ggplot aes geom_histogram geom_density geom_boxplot geom_point stat_summary
#' stat position_jitter
#' scale_y_continuous scale_x_continuous expansion scale_color_manual scale_fill_manual
#' facet_wrap theme coord_cartesian
#'
#' @examples
#' #tbd
#'
#' @export
#' @rdname plot_distributions
plot_distributions <- function(data, variable, bw = 5, color = "#0d7abc") { #group

  plot <-
    ggplot(data = data,
           mapping = aes(x = {{ variable }})) +
    geom_histogram(
      # breite der behälter (balken), immer verschiedene werte ausprobieren
      binwidth = bw,
      # zentrum in der mitte des behälters definieren
      center = bw / 2,
      # damit die einzelnen bins besser erkennabr sind
      color = "white",
      fill = color)

  # damit die warnung von ggplot beser verstanden wird, die anzahl na ausgeben
  n_na <- sum(is.na(dplyr::pull(data, {{ variable }})))
  print(paste0("Achtung!! Die untersuchte Variable beinhaltet ", n_na, " NAs."))

  plot +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                       breaks = scales::breaks_extended(),
                       labels = scales::label_number(big.mark = "'")) +
    cowplot::theme_minimal_hgrid() +
    # je nach binwidth kann der strich verwirrlich sein, deshalb entfernen
    theme(axis.ticks.x = element_blank())

}

#' @export
#' @rdname plot_distributions
plot_distributions_density <- function(data, variable, bw = 5, color = "#0d7abc") {

  plot <-
    ggplot(data = data,
           mapping = aes(x = {{ variable }})) + # y = stat(count), (scaled) bei skalierter dichte (Anzahl)
    geom_density(
      # breite des bandes, immer verschiedene werte ausprobieren
      bw = bw,
      # kernal wird absichtlich nich angegeben in der funktion, es soll immer mit
      # der default wert gearbeitet werden (kernel = "qaussian")
      fill = color)

  # damit die warnung von ggplot beser verstanden wird, die anzahl na ausgeben
  n_na <- sum(is.na(dplyr::pull(data, {{ variable }})))
  print(paste0("Achtung!! Die untersuchte Variable beinhaltet ", n_na, " NAs."))

  plot +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                       breaks = scales::breaks_extended(),
                       labels = scales::label_number(big.mark = "'")) +
    cowplot::theme_minimal_hgrid()

}

#' @export
#' @rdname plot_distributions
plot_distributions_sidebyside <- function(data, variable, group, bw = 5) {

  plot <-
    ggplot(data = data,
           # y = stat(count) skaliert die dichte auf anzahl, so ist die fläche das total an untersuchungen
           mapping = aes(x = {{ variable }}, y = stat(count))) +
    geom_density(
      data = dplyr::select(data, !{{ group }}),
      mapping = aes(fill = "n_total"),
      color = "transparent") +
    geom_density(
      mapping = aes(fill = {{ group }}),
      # breite des bandes, immer verschiedene werte ausprobieren
      bw = bw,
      color = "transparent"
      # kernal wird absichtlich nich angegeben in der funktion, es soll immer mit
      # der default wert gearbeitet werden (kernel = "qaussian")
      )

  # group-variable als factor
  group_as_factor <- as.factor(dplyr::pull(data, {{ group }}))

  # anzahl levels wird verwendet um die farbpalette auszuwählen
  n_levels <- nlevels(group_as_factor)

  # farbpaletten benötigen mindestens 3 werte (n >= 3)
  if (near(n_levels, 2)) {
    colors <- c("#0d803e", "#4565b2")
    } else {
      colors <- colorspace::qualitative_hcl(n_levels, palette = "Dark 3")
    }

  # farben auf scale mappen
  scale_fill <- scale_fill_manual(
    values = c("#b3b3b3a0", colors),
    # wert der leves ist notwendig für die breaks und labels
    breaks = c("n_total", levels(group_as_factor)),
    labels = c("Total", levels(group_as_factor)),
    name = NULL
    )

  # damit die warnung von ggplot beser verstanden wird, die anzahl na ausgeben
  n_na <- sum(is.na(dplyr::pull(data, {{ variable }})))
  print(paste0("Achtung!! Die untersuchte Variable beinhaltet ", n_na, " NAs."))

  plot +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                       breaks = scales::breaks_extended(),
                       labels = scales::label_number(big.mark = "'"),
                       name = "skalierte Dichte") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) + # limits()?
    scale_fill +
    facet_wrap(vars({{ group }})) +
    cowplot::theme_minimal_hgrid() +
    theme(
      legend.position = "bottom",
      legend.justification = "right"
      )

}


#' @export
#' @rdname plot_distributions
plot_distributions_grouped <- function(data, variable, group, bw = 5) {

  plot <-
    ggplot(data = {{ data }},
           mapping = aes(x = {{ variable }}, fill = {{ group }}, color = {{ group }})) +
    geom_density(
      alpha = 0.3,
      # breite des bandes, immer verschiedene werte ausprobieren
      bw = bw)

  # group-variable muss factor sein
  group_as_factor <- as.factor(dplyr::pull(data, {{ group }}))

  # anzahl levels wird verwendet um die farbpalette auszuwählen
  levels <- nlevels(group_as_factor)

  # farbpaletten benötigen mindestens 3 werte (n >= 3)
  if (near(levels, 2)) {
    colors <- c("#0d803e", "#4565b2")
  } else {
    colors <- colorspace::qualitative_hcl(levels, palette = "Dark 3")
  }

  # damit die warnung von ggplot beser verstanden wird, die anzahl na ausgeben
  n_na <- sum(is.na(dplyr::pull(data, {{ variable }})))
  print(paste0("Achtung!! Die untersuchte Variable beinhaltet ", n_na, " NAs."))

  plot +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                       breaks = scales::breaks_extended(),
                       labels = scales::label_number(big.mark = "'")) +
    scale_fill_manual(values = colors, name = NULL) +
    scale_color_manual(values = colors, name = NULL) +
    cowplot::theme_minimal_hgrid()

}


#' @export
#' @rdname plot_distributions
plot_distributions_raincloud <- function(data, variable, group, bw = 5, color = "#0d7abc") {

  plot <-
    ggplot(data = data,
           mapping = aes(x = {{ group }}, y = {{ variable }})) +
    geom_boxplot(width = 0.25, outlier.shape = NA) +
    geom_point(size = 1.3,
               color = color,
               alpha = 0.3,
               position = position_jitter(seed = 1, width = 0.1)
               #position = position_jitterdodge()
    ) +
    # horizontale linien anstatt punkte (shape = 95)
    # geom_point(shape = 95, size = 10, alpha = 0.2) +
    ggdist::stat_halfeye(
      adjust = 0.5,
      width = 0.6,
      .width = 0,
      justification = -0.5,
      fill = color,
      # punkt beim median entfernen
      point_colour = NA
    ) +
    coord_cartesian(xlim = c(1.2, NA))#, clip = "off")

  plot +
    scale_y_continuous(#expand = expansion(mult = c(0, 0.05)),
                       breaks = scales::breaks_extended(),
                       labels = scales::label_number(big.mark = "'")) +
    cowplot::theme_half_open()

}

#' @export
#' @rdname plot_distributions
plot_distributions_boxplot <- function(data, variable, group, bw = 5, color = "#0d7abc", size = 5) {


# helper function ---------------------------------------------------------
  n_fun <- function(x){
    return(tibble::tibble(y = stats::median(x) + 1.5,
                          label = paste0("n = ",length(x))))
  }

# plot --------------------------------------------------------------------

  plot <-
    ggplot(data = data,
           mapping = aes(x = {{ group }}, y = {{ variable }})) +
    geom_boxplot(width = 0.25, outlier.shape = NA) +
    stat_summary(
      geom = "text",
      fun.data = n_fun,
      size = size
    ) +
    ggdist::stat_halfeye(
      adjust = 0.5,
      width = 0.6,
      .width = 0,
      justification = -0.5,
      fill = color,
      # punkt beim median entfernen
      point_colour = NA
    ) +
    # notwendig??
    coord_cartesian(xlim = c(1.2, NA))#, clip = "off")

  plot +
    scale_y_continuous(#expand = expansion(mult = c(0, 0.05)),
      breaks = scales::breaks_extended(),
      labels = scales::label_number(big.mark = "'")) +
    cowplot::theme_minimal_hgrid() +
    theme(axis.ticks.x = element_blank(),
          axis.line.x = element_blank())


}
