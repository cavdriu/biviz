#' Proportionen visualisieren
#'
#' @param data Ein Tibble mit den Daten für den Plot.
#' @param x Die Variable für die x-Achse.
#' @param y Die Variable für die y-Achse.
#' @param n Der Wert zum ploten.
#' @param percent Prozentangabe als Dezimalzahl (0.0 bis 1.0)
#' @param group Gruppierungsvariable für einen einzelnen Plot.
#' @param facet Gruppierungsvariable um einen Plot je Teilaspekt zu machen (Facetten).
#' @param facet_row Anzahl Zeilen für Darstellung der Teilaspekte.
#' @param bw Breite (Anzahl werte die zusammengefasst werden) der Behälter.
#' @param color Farbe um die Balken optisch zu trennen.
#'
#' @return ggplot object
#'
#' @importFrom ggplot2
#' ggplot aes geom_col geom_text geom_hline geom_density geom_rect
#' scale_y_continuous scale_y_discrete scale_x_continuous scale_x_discrete
#' scale_fill_manual expansion coord_polar coord_flip
#' facet_wrap vars
#' theme theme_void element_blank
#'
#'@importFrom dplyr
#'arrange desc mutate ungroup pull near
#'
#' @examples
#' # tbd
#'
#' @export
#' @rdname plot_proportions
plot_proportions <- function(data, n = n, group) {

  # labels berechnen, damit sie in der mitte des jeweiligen blocks sind
  data <-
    data |>
    arrange(desc({{ n }})) |>
    mutate(n_label = cumsum({{ n }}) - ({{ n }} / 2)) |>
    ungroup()

  plot <-
    ggplot(data = data,
           mapping = aes(x = 1, y = {{ n }}, fill = as.factor({{ group }}))) +
    geom_col(position = "stack", color = "white") +
    geom_text(aes(x = 1, y = n_label,
                  label = paste0({{group}}, ":\nn = ", n)),
              color = "white")

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

  plot +
    scale_y_continuous(
      # expand notwendig, damit die prozent 0 & 100 ganz dargestellt werden
      #expand = expansion(mult = c(0.05, 0.05)), # mit dem default geht es auch
      labels = scales::label_percent(),
      #damit nach coord_flip die y-achse oben ist
      position = "right"
    ) +
    #scale_y_continuous(expand = c(0, 0)) +
    scale_x_discrete(expand = c(0, 0),
                     breaks = NULL,
                     name = NULL) +
    scale_fill_manual(values = colors,
                      guide = NULL) +
    coord_flip() +
    # wegen der erweiterung der y-scale
    theme(panel.background = element_blank())

}

#' @export
#' @rdname plot_proportions
plot_proportions_donut <- function(data, n = n, percent = percent, group) {

# https://semba-blog.netlify.app/07/12/2019/pie-chart-and-donut-plot-with-ggplot2/
# es ist einfacher alles zuerst ohne coord_polar() einzustellen, da ich gewohnt bin
# kartesischen koordinatensystem zu denken

  # positionen für text berechnen
  data <-
    data |>
    arrange(desc({{ n }})) |>
    mutate(pos_value = cumsum({{ percent }}) - (0.5 * {{ percent }} ),
           pos_label = cumsum({{ percent }}) - (0.45 * {{ percent }} )) |>
    ungroup()

  plot <-
    ggplot(data = data,
           mapping = aes(x = 2, y = {{ percent }}, fill = as.factor({{ group }}))) +
    geom_col(position = "stack", color = "white") +
    # damit ein kreis entsteht das koordinatensystem anpassen
    coord_polar(theta = "y", start = 0) +
    # values
    geom_text(aes(x = 2, y = pos_value, label = {{ n }}), color = "white") +
    # groups
    geom_text(aes(x = 2.9, y = pos_label, label = as.factor({{ group }})), color = "black")

  # group-variable als factor
  group_as_factor <- as.factor(dplyr::pull(data, {{ group }}))

  # anzahl levels wird verwendet um die farbpalette auszuwählen
  levels <- nlevels(group_as_factor)

  # farbpaletten benötigen mindestens 3 werte (n >= 3)
  if (near(levels, 2)) {
    colors <- c("#ff7b39", "#4565b2")
  } else {
    colors <- colorspace::qualitative_hcl(levels, palette = "Dark 3")
  }

  plot +
    scale_x_continuous(limits = c(0.5, 3)) +
    scale_fill_manual(values = colors, guide = NULL) +
    theme_void()

}

#' @export
#' @rdname plot_proportions
plot_proportions_stacked <- function(data, x, y, group) {

  plot <-
    ggplot(data = data,
           mapping = aes(x = {{ x }}, y = {{ y }}, fill = {{ group }})) +
    # damit die einzelnen blöcke besser unterscheidbar sind, eine weisse linie hinzufügen
    geom_col(position = "stack", color = "white", size = 0.75) +  # "fill", width = 1
    # damit die 50% marke einfacher erkennbar ist
    geom_hline(
      yintercept = 0.5,
      color = "black", size = 0.5, linetype = 2, alpha = 0.9
    ) +
    geom_hline(
      yintercept = 1,
      color = "black", size = 0.5, linetype = 1
    )

  # group-variable als factor
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

  plot +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.05)),
      labels = scales::label_percent()
      ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_manual(values = colors, name = NULL) +
    cowplot::theme_half_open() +
    theme(axis.line.y = element_blank())
}

#' @export
#' @rdname plot_proportions
plot_proportions_sidebyside_bar <- function(data, x, percent, facet) {

  # grundgerüst vom plot
  plot <-
    ggplot(data = data,
           mapping = aes(x = {{ x }}, y = {{ percent }}, fill = {{ x }})) +
    geom_col() +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.05)),
      labels = scales::label_percent()
      )

  # testen ob die group-variable ein factor ist
  group_as_factor <- dplyr::pull(data, {{ x }})

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

  plot +
    scale_fill_manual(values = colors, name = NULL) +
    facet_wrap(vars({{ facet }})) +
    cowplot::theme_half_open() +
    # damit das facet label nicht in einer grauen box ist
    theme(strip.background = element_blank())
}

#' @export
#' @rdname plot_proportions
plot_proportions_sidebyside_density1 <- function(data, x, group, bw = 5, color = "#0d7abc", facet_row = 1) {

  plot <-
    ggplot(data = data,
           # y = after_stat(count) skaliert die dichte auf anzahl, so ist die fläche das total an untersuchungen
           mapping = aes(x = {{ x }}, y = after_stat(count))) +
    geom_density(
      data = dplyr::select(data, !{{ group }}),
      mapping = aes(fill = "n_total"),
      color = "transparent"
      ) +
    geom_density(
      mapping = aes(fill = "highlighted"),
      # breite des bandes, immer verschiedene werte ausprobieren
      bw = bw,
      color = "transparent"
      # kernal wird absichtlich nich angegeben in der funktion, es soll immer mit
      # der default wert gearbeitet werden (kernel = "qaussian")
    )

  # farben auf scale mappen
  scale_fill <- scale_fill_manual(
    values = c("#b3b3b3a0", color),
    # wert der leves ist notwendig für die breaks und labels
    breaks = c("n_total", "highlighted"),
    labels = c("alle Beobachtungen", "hervorgehobene Gruppe"),
    name = NULL
  )

  # damit die warnung von ggplot beser verstanden wird, die anzahl na ausgeben
  n_na <- sum(is.na(dplyr::pull(data, {{ x }})))
  print(paste0("Achtung!! Die untersuchte Variable beinhaltet ", n_na, " NAs."))

  plot +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                       breaks = scales::breaks_extended(),
                       labels = scales::label_number(big.mark = "'"),
                       name = "Anzahl") +
    #scale_x_continuous(expand = expansion(mult = c(0, 0.05))) + # limits()?
    scale_fill +
    facet_wrap(vars({{ group }}), nrow = facet_row) +
    cowplot::theme_minimal_hgrid() +
    theme(
      legend.position = "bottom",
      legend.justification = "right"
    )

}


#' @export
#' @rdname plot_proportions
plot_proportions_sidebyside_density2 <- function(data, x, group, bw = 5, color = "#0d7abc", facet_row = 1) {

  # # wie mit map() schreiben?
  # output <- vector("logical")
  # for (i in seq_along(data)) {
  #   output[[i]] <- is.na(df[[i]])
  # }
  #stopifnot(all(!is.na(data)))

  # legende muss noch optimiert werden
  #
  # Hilfe Funktionen sind notwendig, da die einzelne facette über alle daten ist (proportional)
  #
  # https://stackoverflow.com/questions/5249673/how-should-i-handle-helper-functions-in-an-r-package
  #
  # https://www.tidyverse.org/blog/2020/04/dplyr-1-0-0-and-vctrs/
  #
  #   Achtung: labelled daten müssen in factor umgewandet werden
  #
  # bw = bw & color = "transparent" kontrollieren ob das notwendig ist

# helper functions -----------------------------------------------------------------------

  ## pro plot (facette) das verhältnis andere und untersuchte gruppe berechnen
  ## wird in der funktion df_build_group() benötigt
  build_group <- function(data, variable, focus_group){

    data |>
      dplyr::mutate(
        # r4ds, S. 217
        # https://forcats.tidyverse.org/reference/fct_collapse.html
        focus = as.character(forcats::fct_collapse({{ variable }},
                                                   factor = focus_group,
                                                   # aa damit es immer oben ist im plot
                                                   aa_other = setdiff(levels(pull({{ data }}, {{ variable }})),
                                                                   focus_group))),
        # notwendig, dass bei fc_collapse() nicht direkt den wert aus focus_group extrahiert werden kann als name
        focus = ifelse(focus == "factor", focus_group, focus),
        highlight = focus_group
      )
  }

  ## daten der einzelnen facetten zusammeführen
  df_build_group <- function(data, variable) {

    output <- tibble::tibble()
    var <- pull(data, {{ variable }})
    focus_group <- levels(var)

    for (i in seq_along(focus_group)) {
      df <- build_group(data, {{ variable }}, focus_group[[i]])
      output <- dplyr::bind_rows(output, df)
    }

    output

  }

# plot --------------------------------------------------------------------

  # die daten werden anhand der anzahl teilaspekte (facetten) multipliziert
  data <- df_build_group(data, {{ group }})
  #print(data)

  ## check for numeric, wegen scale::percent ist eine dezihamlzahl notwendig

  plot <-
    ggplot(data = data,
           mapping = aes(x = {{ x }})) +
    geom_density(
      mapping = aes(y = after_stat(count), fill = focus), #{{}}
      position = "fill",
      color = "transparent"
    )

 levels <-
    data |>
    dplyr::pull({{ group }})

  first_level <- levels(levels)[[1]]
  v_levels <- levels(levels)
  n_levels <- nlevels(levels)

  #print(n_levels)
  #print(rep(color, n_levels))

  # TODO --------------------------------------------------------------------
  # nur einmal hervorgehobene .. als legende
  # df_build_group(gss_happy, happy) |>
  #   # factor arg bei plot funktion ergänzen, damit die reihenfolge kontrolliert werden kann
  #   #mutate(higlight = factor(higlight, levels = levels(lev)))

  # farben auf scale mappen
  scale_fill <- scale_fill_manual(
    name = NULL,
    values = c("#b3b3b3a0", rep(color, n_levels)),
    #breaks = c("aa_other", first_level),
    #labels = c("alle Beobachtungen", "hervorgehobene Gruppe")
    breaks = c("aa_other", v_levels),
    labels = c("alle Beobachtungen", rep("hervorgehobene Gruppe", n_levels))
  )

  # damit die warnung von ggplot beser verstanden wird, die anzahl na ausgeben
  n_na <- sum(is.na(dplyr::pull(data, {{ x }})))
  print(paste0("Achtung!! Die untersuchte Variable beinhaltet ", n_na, " NAs."))

  plot +
    facet_wrap(vars(highlight), nrow = facet_row) + #{{}}
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                       breaks = scales::breaks_extended(),
                       labels = scales::percent,
                       name = "relativer Anteil") +
    scale_fill +
    #facet_wrap(vars(highlight), nrow = facet_row) + #{{}}
    cowplot::theme_minimal_hgrid() +
    theme(
      axis.line.x = element_blank(),
      #legend.position = "none"
      legend.position = "bottom",
      legend.justification = "right"
    )

  }

