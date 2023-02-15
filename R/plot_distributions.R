#' Verteilungen visualisieren. plot_distributions_* Familie.
#'
#' Hinweis: Die Daten duerfen nicht aggregiert sein. Die Berechnung erfolgt in der Funktion.
#'
#' @param data Ein Tibble mit den Daten für den Plot.
#' @param x Variable die untersucht wird (x-Achse).
#' @param y Variable die untersucht wird (y-Achse).
#' @param bw Breite (Anzahl werte die zusammengefasst werden) der Behälter.
#' [Mehr Infos beim Argument bw in der ggplot2 Dokumentation](https://ggplot2.tidyverse.org/reference/geom_density.html?q=density#arguments)
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
#' df <-
#'   socviz::gss_lon |>
#'    tidyr::drop_na(sex)
#'
#' cowplot::plot_grid(
#'   plot_distributions_histogram(data = df, x = age),
#'   plot_distributions_histogram(data = df, x = age, bw = 2),
#'   labels = c('A: bw = 5 (default)', 'B: bw = 2')
#'   )
#'
#' # mit pipe syntax
#' df |>
#'   tidyr::drop_na(happy) |>
#'   plot_distributions_grouped(x = age, group = happy, bw = 3)
#'
#' # ohne pipe
#'   plot_distributions_sidebyside(data = df, x = age, group = sex)
#'
#' df |>
#'   # jede 100th Zeile im df behalten
#'   # notwendig, da ansonsten zu grosse
#'   # datenmenge für diesen plot
#'   dplyr::filter(dplyr::row_number() %% 100 == 1) |>
#'   plot_distributions_raincloud(x = happy, y = age)
#'
#' cowplot::plot_grid(
#'   plot_distributions_raincloud(data = df, x = happy, y = age),
#'   plot_distributions_boxplot(data = df, x = happy, y = age, size = 3),
#'   labels = c("A: Zuviele Datenpunkte für raincloud plot.",
#'              "B: Beobachtungen als Zahl darstellen."
#'              ),
#'   ncol = 1
#'   )
#'
#'
#' @export
#' @rdname plot_distributions
plot_distributions_histogram <- function(data, x, bw = 5, color = "#0d7abc") {

  ## argument checking
  ## x muss numerisch sein, damit die die histogramme
  # korrekt berechnet werden kann
  stopifnot("x muss numerisch sein. Wandele die Variable im Datensatz um."
            = is.numeric(dplyr::pull(data, {{ x }})))

  # um die warnung von ggplot2 bei na verstaendlicher zu machen, werden die na
  # noch ausgegeben. -> TODO: ifelse
  n_na <- sum(is.na(dplyr::pull(data, {{ x }})))
  message(
    paste0("Die untersuchte Variable (x) beinhaltet ", n_na, " NAs.")
  )

  plot <-
    ggplot(data = data,
           mapping = aes(x = {{ x }})) +
    geom_histogram(
      # breite der behälter (balken), immer verschiedene werte ausprobieren
      binwidth = bw,
      # zentrum in der mitte des behälters definieren
      center = bw / 2,
      # damit die einzelnen bins besser erkennabr sind
      color = "white",
      fill = color)

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
plot_distributions_density <- function(data, x, bw = 5, color = "#0d7abc") {

  # https://de.wikipedia.org/wiki/Kerndichtesch%C3%A4tzer

  ## argument checking
  # vgl. plot_distributions_histogram
  stopifnot("x muss numerisch sein. Wandele die Variable im Datensatz um."
            = is.numeric(dplyr::pull(data, {{ x }})))

  # vgl. plot_distributions_histogram
  n_na <- sum(is.na(dplyr::pull(data, {{ x }})))
  message(
    paste0("Die untersuchte Variable (x) beinhaltet ", n_na, " NAs.")
  )

  plot <-
    ggplot(data = data,
           mapping = aes(x = {{ x }})) + # y = stat(count), (scaled) bei skalierter dichte (Anzahl)
    geom_density(
      # breite des bandes, immer verschiedene werte ausprobieren
      bw = bw,
      # kernal wird absichtlich nich angegeben in der funktion, es soll immer
      # mit dem default wert gearbeitet werden (kernel = "qaussian")
      # https://ggplot2.tidyverse.org/reference/geom_density.html#arguments
      fill = color)

  plot +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                       breaks = scales::breaks_extended(),
                       labels = scales::label_number(big.mark = "'")) +
    cowplot::theme_minimal_hgrid()

}

#' @export
#' @rdname plot_distributions
plot_distributions_sidebyside <- function(data, x, group, bw = 5) {

  ## argument checking
  # vgl. plot_distributions_histogram
  stopifnot("x muss numerisch sein. Wandele die Variable im Datensatz um."
            = is.numeric(dplyr::pull(data, {{ x }})))

  # vgl. plot_distributions_histogram
  n_na <- sum(is.na(dplyr::pull(data, {{ x }})))
  message(
    paste0("Die untersuchte Variable (x) beinhaltet ", n_na, " NAs.")
  )

  plot <-
    ggplot(data = data,
           # y = stat(count) skaliert die dichte auf anzahl, so ist die fläche das total an untersuchungen
           mapping = aes(x = {{ x }}, y = after_stat(count))) +
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
  if (dplyr::near(n_levels, 2)) {
    colors <- c("#ff7b39", "#4565b2")
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
plot_distributions_grouped <- function(data, x, group, bw = 5) {

  ## argument checking
  # vgl. plot_distributions_histogram
  stopifnot("x muss numerisch sein. Wandele die Variable im Datensatz um."
            = is.numeric(dplyr::pull(data, {{ x }})))

  # vgl. plot_distributions_histogram
  n_na <- sum(is.na(dplyr::pull(data, {{ x }})))
  message(
    paste0("Die untersuchte Variable (x) beinhaltet ", n_na, " NAs.")
  )

  plot <-
    ggplot(data = {{ data }},
           mapping = aes(x = {{ x }}, fill = {{ group }}, color = {{ group }})) +
    geom_density(
      # durch die transparenz sind die dichten besser unterscheidbar
      alpha = 0.3,
      # breite des bandes, immer verschiedene werte ausprobieren
      bw = bw)

  # group-variable muss factor sein
  group_as_factor <- as.factor(dplyr::pull(data, {{ group }}))

  # anzahl levels wird verwendet um die farbpalette auszuwählen
  levels <- nlevels(group_as_factor)

  # farbpaletten benötigen mindestens 3 werte (n >= 3)
  if (dplyr::near(levels, 2)) {
    colors <- c("#ff7b39", "#4565b2")
  } else {
    colors <- colorspace::qualitative_hcl(levels, palette = "Dark 3")
  }

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
plot_distributions_raincloud <- function(data, x, y, bw = 5, color = "#0d7abc") {

  # https://www.cedricscherer.com/2021/06/06/visualizing-distributions-with-raincloud-plots-and-how-to-create-them-with-ggplot2/

  ## argument checking
  # vgl. plot_distributions_histogram
  stopifnot("y muss numerisch sein. Wandele die Variable im Datensatz um."
            = is.numeric(dplyr::pull(data, {{ y }})))

  # vgl. plot_distributions_histogram
  n_na <- sum(is.na(dplyr::pull(data, {{ x }})))
  message(
    paste0("Die untersuchte Variable (x) beinhaltet ", n_na, " NAs.")
  )

  plot <-
    ggplot(data = data,
           mapping = aes(x = {{ x }}, y = {{ y }})) +
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
    cowplot::theme_minimal_hgrid() +
    # die x-line ist nicht notwendig und ist wirrwarr
    theme(axis.ticks.x = element_blank(),
          axis.line.x = element_blank())

}

#' @export
#' @rdname plot_distributions
plot_distributions_boxplot <- function(data, x, y, bw = 5, color = "#0d7abc", size = 5) {

  ## argument checking
  # vgl. plot_distributions_histogram
  stopifnot("y muss numerisch sein. Wandele die Variable im Datensatz um."
            = is.numeric(dplyr::pull(data, {{ y }})))

  # vgl. plot_distributions_histogram
  n_na <- sum(is.na(dplyr::pull(data, {{ x }})))
  message(
    paste0("Die untersuchte Variable (x) beinhaltet ", n_na, " NAs.")
  )

# helper function ---------------------------------------------------------
  n_fun <- function(x){
    # in stat_summary wird y für die berechnung verwendet
    # die daten werden via funktion integriert
    return(tibble::tibble(y = stats::median(x) + 1.5,
                          label = paste0("n = ",length(x))))
  }

# plot --------------------------------------------------------------------

  plot <-
    ggplot(data = data,
           mapping = aes(x = {{ x }}, y = {{ y }})) +
    geom_boxplot(width = 0.25, outlier.shape = NA) +
    # auf der y-achse soll n angezeigt werden (vgl. helper function)
    # https://ggplot2-book.org/layers.html?q=stat_summary#stat
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
    coord_cartesian(xlim = c(1.2, NA)) #evt. clip = "off"

  plot +
    scale_y_continuous(#expand = expansion(mult = c(0, 0.05)),
      breaks = scales::breaks_extended(),
      labels = scales::label_number(big.mark = "'")) +
    cowplot::theme_minimal_hgrid() +
    theme(axis.ticks.x = element_blank(),
          axis.line.x = element_blank())


}
