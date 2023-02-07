#' Proportionen visualisieren. plot_proportions_* Familie.
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
#' @param color Farbe um Gruppierungen zu markieren.
#'
#' @return ggplot object
#'
#' @importFrom ggplot2
#' ggplot aes geom_col geom_text geom_hline geom_density geom_rect
#' scale_y_continuous scale_y_discrete scale_x_continuous scale_x_discrete
#' scale_fill_manual expansion coord_polar coord_flip
#' facet_wrap vars after_stat
#' theme theme_void element_blank
#'
#' @examples
#' df1 <- tibble::tibble(
#'   x = c("a", "b", "c"),
#'   y = c(3, 7, 10)
#'   )
#'
#' df1 |>
#'   dplyr::mutate(percent = y / sum(y)) |>
#'   plot_proportions_bar(group = x, n = y, percent = percent)
#'
#'# aufbereitung der daten
#' df2 <- socviz::gss_sib |>
#'   dplyr::count(year, sex) |>
#'   dplyr::group_by(year) |>
#'   dplyr::mutate(sum_year = sum(n)) |>
#'   dplyr::group_by(year, sex) |>
#'   dplyr::summarise(percent = n / sum_year) |>
#'   dplyr::ungroup()
#'
#'# mit pipe syntax
#' df2 |>
#'   plot_proportions_stacked(x = year, y = percent, group = sex)
#'
#'# ohne pipe
#' plot_proportions_sidebyside_col(data = df2, x = year, y = percent, color = sex, facet = sex)
#'
#' socviz::gss_lon |>
#'   plot_proportions_sidebyside_density1(x = as.numeric(age), group = happy)
#'
#'
#' @export
#' @rdname plot_proportions
plot_proportions_bar <- function(data, group, percent = percent, n = n) {

  # labels berechnen, damit sie in der mitte des jeweiligen blocks sind
  data <-
    data |>
    dplyr::arrange(dplyr::desc({{ percent }})) |>
    dplyr::mutate(n_label = cumsum({{ percent }}) - ({{ percent }} / 2))

  plot <-
    ggplot(data = data,
           mapping = aes(x = 1, y = {{ percent }}, fill = as.factor({{ group }}))) +
    geom_col(position = "stack", color = "white") +
    geom_text(aes(x = 1, y = n_label,
                  label = paste0({{ group }}, ":\nn = ", {{ n }})
                  ),
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
  if (dplyr::near(levels, 2)) {
    colors <- c("#ff7b39", "#4565b2")
  } else {
    colors <- colorspace::qualitative_hcl(levels, palette = "Dark 3")
  }

  plot +
    scale_y_continuous(
      # expand notwendig, damit die prozent 0 & 100 ganz dargestellt werden
      labels = scales::label_percent(),
      # damit nach coord_flip die y-achse oben ist
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
plot_proportions_donut <- function(data, group, n = n, percent = percent) {

# evt. donut aus dem portfolio nehmen
# https://semba-blog.netlify.app/07/12/2019/pie-chart-and-donut-plot-with-ggplot2/
# es ist einfacher alles zuerst ohne coord_polar() einzustellen, da ich gewohnt bin
# kartesischen koordinatensystem zu denken

  # positionen für text berechnen
  data <-
    data |>
    dplyr::arrange(dplyr::desc({{ n }})) |>
    dplyr::mutate(pos_value = cumsum({{ percent }}) - (0.5 * {{ percent }} ),
           pos_label = cumsum({{ percent }}) - (0.45 * {{ percent }} ))

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
  if (dplyr::near(levels, 2)) {
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

  # testen ob die x-achse numerisch ist, wenn ja darf am ende und anfang kein
  # whitespace sein und deshalb wird die erweiterung der x-achse unterbunden.
  cond <- dplyr::pull(data, {{ x }})

  if (is.numeric(cond)) {
    scale_x <- scale_x_continuous(expand = c(0, 0))
  } else {
    scale_x <- scale_x_discrete()
    }

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
  if (dplyr::near(levels, 2)) {
    colors <- c("#ff7b39", "#4565b2")
  } else {
    colors <- colorspace::qualitative_hcl(levels, palette = "Dark 3")
  }

  plot +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.05)),
      labels = scales::label_percent()
      ) +
    #scale_x_continuous(expand = c(0, 0)) +
    scale_x +
    scale_fill_manual(values = colors, name = NULL) +
    cowplot::theme_half_open() +
    theme(axis.line.y = element_blank())
}

#' @export
#' @rdname plot_proportions
plot_proportions_sidebyside_col <- function(data, x, y, color, facet) {

  ## argument checking
  # color muss ein factor sein, damit die labels für die farben eruiert werden können
  stopifnot("x muss ein factor sein. Wandele die Variable im Datensatz um."
            = is.factor(dplyr::pull(data, {{ color }})))

  # grundgerüst vom plot
  plot <-
    ggplot(data = data,
           mapping = aes(x = {{ x }}, y = {{ y }}, fill = {{ color }})) +
    geom_col() +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.05)),
      labels = scales::label_percent()
      )

  # testen ob die x-achse numerisch ist, wenn ja darf am ende und anfang kein
  # whitespace sein und deshalb wird die erweiterung der x-achse unterbunden.
  cond <- dplyr::pull(data, {{ x }})

  if (is.numeric(cond)) {
    scale_x <- scale_x_continuous(expand = c(0, 0))
  } else {
    scale_x <- scale_x_discrete()
  }

  # anzahl levels wird verwendet um die farbpalette auszuwählen
  levels <- nlevels(dplyr::pull(data, {{ color }}))

  # farbpaletten benötigen mindestens 3 werte (n >= 3)
  if (dplyr::near(levels, 2)) {
    colors <- c("#ff7b39", "#4565b2")
  } else {
    colors <- colorspace::qualitative_hcl(levels, palette = "Dark 3")
  }

  plot +
    scale_x +
    scale_fill_manual(values = colors, name = NULL) +
    facet_wrap(vars({{ facet }})) +
    cowplot::theme_half_open() +
    # damit das facet label nicht in einer grauen box ist
    theme(strip.background = element_blank())
}

#' @export
#' @rdname plot_proportions
plot_proportions_sidebyside_density1 <- function(data, x, group, bw = 5, color = "#0d7abc", facet_row = 1) {

  ## argument checking
  ## x muss numerisch sein, damit die die dichte (density)
  # korrekt berechnet werden kann
  stopifnot("x muss numerisch sein. Wandele die Variable im Datensatz um."
            = is.numeric(dplyr::pull(data, {{ x }})))

  # fuer die density funktion dürfen die daten nicht aggregiert sein.
  # anders als bei den balken diagrammen. die berechnung erfolgt in der funktion.
  # um die warnung von ggplot2 bei na verstaendlicher zu machen, werden die na
  # noch ausgegeben.
  n_na <- sum(is.na(dplyr::pull(data, {{ x }})))
  message(
    paste0("Hinweis: Die Daten duerfen nicht aggregiert sein. Die Berechnung erfolg in der Funktion. Die untersuchte Variable (x) beinhaltet ", n_na, " NAs.")
    )

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

  # funktion funtioniert noch nicht korrekt. deshalb soll sich abgebrochen
  # werden bei einem aufruf
  stop("Die Funktion ist noch nicht anwendbar. Check: Quellcode.")

  # TODO --------------------------------------------------------------------

  # nur einmal hervorgehobene .. als legende
  # df_build_group(gss_happy, happy) |>
  #   # factor arg bei plot funktion ergänzen, damit die reihenfolge kontrolliert werden kann
  #   #mutate(higlight = factor(higlight, levels = levels(lev)))
  # legende muss noch optimiert werden
  #
  # Hilfe Funktionen sind notwendig, da die einzelne facette über alle daten ist (proportional)
  #
  # https://stackoverflow.com/questions/5249673/how-should-i-handle-helper-functions-in-an-r-package
  #
  # https://www.tidyverse.org/blog/2020/04/dplyr-1-0-0-and-vctrs/
  #
  # Achtung: labelled daten müssen in factor umgewandet werden
  #
  # bw = bw & color = "transparent" kontrollieren ob das notwendig ist
  #
  # genaue anwendung noch definieren
  #
  # Beispiel:
  # socviz::gss_lon |>
  #   mutate(age = as.numeric(age)) |>
  #   select(age, happy) |>
  #   filter(!is.na(happy)) |>
  #   plot_proportions_sidebyside_density2(x = age, group = happy)




# checking ----------------------------------------------------------------

  ## argument checking
  ## x muss numerisch sein, damit die die dichte (density)
  # korrekt berechnet werden kann
  stopifnot("x muss numerisch sein." = is.numeric(dplyr::pull(data, {{ x }})))

  # fuer die density funktion dürfen die daten nicht aggregiert sein.
  # anders als bei den balken diagrammen. die berechnung erfolgt in der funktion.
  # um die warnung von ggplot2 bei na verstaendlicher zu machen, werden die na
  # noch ausgegeben.
  n_na <- sum(is.na(dplyr::pull(data, {{ x }})))
  message(
    paste0("Hinweis: Die Daten duerfen nicht aggregiert sein. Die Berechnung erfolg in der Funktion. Die untersuchte Variable (x) beinhaltet ", n_na, " NAs.")
  )


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
                                                   aa_other = setdiff(levels(dplyr::pull({{ data }}, {{ variable }})),
                                                                      focus_group))),
        # notwendig, da bei fc_collapse() nicht direkt der wert aus focus_group extrahiert werden kann als name
        focus = ifelse(focus == "factor", focus_group, focus),
        highlight = focus_group
      )
  }

  ## daten der einzelnen facetten zusammeführen
  df_build_group <- function(data, variable) {

    output <- tibble::tibble()
    var <- dplyr::pull(data, {{ variable }})
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

  # farben auf scale mappen
  scale_fill <- scale_fill_manual(
    name = NULL,
    values = c("#b3b3b3a0", rep(color, n_levels)),
    #breaks = c("aa_other", first_level),
    #labels = c("alle Beobachtungen", "hervorgehobene Gruppe")
    breaks = c("aa_other", v_levels),
    labels = c("alle Beobachtungen", rep("hervorgehobene Gruppe", n_levels))
  )

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

