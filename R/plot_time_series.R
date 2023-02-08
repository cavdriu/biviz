#' Zeitreihen visualisieren. plot_timeseries_* Familie.
#'
#'
#' @param data Ein Tibble mit den Daten für den Plot.
#' @param x Die Variable für die x-Achse.
#' @param y Die Variable für die y-Achse.
#' @param group Gruppierungsvariable für einen einzelnen Plot.
#' @param color_point Farbe der Punkte.
#' @param color_line Farbe für die Linie.
#' @param color_area Farbe vom Bereich unter dem Linienplot.
#' @param color_trend Farbe für die Trendlinie.
#' @param y_limits Die beiden Poole bei der Y-Achse.
#' [Mehr Infos beim Argument limits in der ggplot2 Dokumentation](https://ggplot2.tidyverse.org/reference/scale_continuous.html?q=scale_y_con#arguments)
#' @param method Methode die in der Funktion verwendet wird. Variiert je nach Funktion.
#' [Mehr Infos beim Argument method in der ggplot2 Dokumentation](https://ggplot2.tidyverse.org/reference/geom_smooth.html?q=geom_smooth#arguments)
#' @param formula Berechnungsformel der Methode.
#' [Mehr Infos beim Argument formula in der ggplot2 Dokumentation](https://ggplot2.tidyverse.org/reference/geom_smooth.html?q=geom_smooth#arguments)
#' @param facet Gruppierungsvariable um einen Plot je Teilaspekt zu machen (Facetten).
#'
#'
#'
#' @importFrom ggplot2
#' ggplot aes geom_line geom_point geom_area geom_smooth
#' scale_x_discrete scale_y_continuous scale_x_date scale_color_manual expansion dup_axis
#' theme element_blank element_line
#'
#' @importFrom rlang
#' :=
#'
#' @return ggplot object
#'
#' @examples
#' ggplot2::mpg |>
#'   dplyr::group_by(year, manufacturer) |>
#'   dplyr::summarise(hwy_by_manufacturer = mean(hwy)) |>
#'   dplyr::ungroup() |>
#'   plot_timeseries_slope(x = year, y = hwy_by_manufacturer, group = manufacturer)
#'
#' stock_markets <- tsbox::ts_tbl(datasets::EuStockMarkets) |>
#'   dplyr::mutate(time = lubridate::as_date(time))
#'
#' # ein Aktienmarkt (1 Merkmal)
#' stock_markets |>
#'   dplyr::filter(id == "FTSE") |>
#'   plot_timeseries_line(x = time, y = value, group = id)
#'
#' # mehrere Aktienmaerkte (n Merkmale)
#' stock_markets |>
#'   dplyr::filter(id != "FTSE") |>
#'   plot_timeseries_line(x = time, y = value, group = id)
#'
#' # ein Aktienmarkt (1 Merkmal)
#' stock_markets |>
#'   dplyr::filter(id == "FTSE") |>
#'   plot_timeseries_trend(x = time, y = value)
#'
#' # mehrere Aktienmaerkte (n Merkmale)
#' stock_markets |>
#'   dplyr::filter(id != "FTSE") |>
#'   plot_timeseries_trend(x = time, y = value) +
#'   # Erweiterung mit ggplot2 Funktion
#'   ggplot2::facet_wrap(ggplot2::vars(id))
#'
#' # trendbereinigte monatliche Entwicklung
#' stock_markets |>
#'   # zuerst die durchschnittliche
#'   # monatliche entwicklung berechnen
#'   dplyr::mutate(time = format(time, "%Y-%m")) |>
#'   dplyr::group_by(id, time) |>
#'   dplyr::summarise(value = mean(value)) |>
#'   dplyr::ungroup() |>
#'   plot_timeseries_detrend(x = time, y = value, group = id)
#'
#' @export
#' @rdname plot_timeseries
plot_timeseries_slope <- function(data, x, y, group, color_point = "#0d7abc", color_line = "#b3b3b3a0") {

  ## argument checking
  ## x muss numerisch sein, damit die labels für das höchste jahr eruiert werden kann
  stopifnot("x muss numerisch sein." = is.numeric(dplyr::pull(data, {{ x }})))

  ## labels für die einzelne slops (direkt labeling)
  labels <-
    data |>
    ## beim filter muss links von == das {{}} (curly curly) in klammern gesetzt werden
    ## da == vorrang gegenueber {{}} hat, muss man explizit sein
    ## https://dplyr.tidyverse.org/articles/programming.html
    ## http://modern-rstats.eu/defining-your-own-functions.html#the-enquo---approach
    dplyr::filter(({{ x }}) == max({{ x }})) |>
    dplyr::mutate({{ x }} := as.factor({{ x }}))

  data <-
    data |>
    ## die x-achse ist diskret, deshalb muss x zu faktor umgewandelt werden
    dplyr::mutate({{ x }} := as.factor({{ x }}))

  plot <-
    ggplot(data = data,
           mapping = aes( x = {{ x }}, y = {{ y }})) +
    geom_line(aes(group = {{ group }}), color = color_line) +
    ## damit zwischen linie und punkt raum entsteht, damit wird ein teil der linie ueberdekt
    geom_point(color = "white", size = 4) +
    ggrepel::geom_text_repel(data = labels,
                             aes(label = {{ group }}),
                             ## fuer die klarheit soll immer eine linie gezeichnet werden
                             min.segment.length = 0,
                             ## nudge_y soll automatisch gewaehlt werden
                             ## nudge_x muss einen min. abstand zum x-wert haben
                             nudge_x = 0.2,
                             direction = "y",
                             hjust = "right") +
    ## darstellung des werts, wegen den linien bei geom_text_repel
    ## muss dieses geom nach dem geom_text_repel kommmen
    geom_point(color = color_point, size = 2)

  plot +
    scale_x_discrete(expand = expansion(mult = c(0.05, 0.3))) +
    scale_y_continuous(expand = expansion(add = 1),
                       breaks = scales::breaks_extended(),
                       labels = scales::label_number(big.mark = "'")) +
    cowplot::theme_half_open() +
    theme(
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank()
    )

}


#' @export
#' @rdname plot_timeseries
plot_timeseries_line <- function(data, x, y, group, color_line = "#0d7abc",
                                 color_area = "#6fa8dc", y_limits = NULL) {

  ## argument checking
  stopifnot("x muss ein datum (yyyy-mm-dd) sein." = lubridate::is.Date(dplyr::pull(data, {{ x }}))) # "x must be numeric"
  stopifnot("y muss numerisch sein." = is.numeric(dplyr::pull(data, {{ y }})))

  ## labels fuer die einzelne linien/gruppen (direkt labeling)
  labels <-
    data |>
    ## vgl. plot_timeseries_slope
    dplyr::select({{ x }}, {{ y }}, {{ group }}) |>
    ## damit das hoechste datum (max(x)) je gruppe genommen wird
    ## ohne group_by kann es sein, dass eine gruppe
    ## keine beschriftung erhaelt. dies ist der fall, falls beim
    ## letzten datum kein wert vorhanden ist.
    dplyr::group_by({{ group }}) |>
    dplyr::filter(({{ x }}) == max({{ x }})) |>
    dplyr::ungroup() |>
    ## namen der variablen unabhaengig vom df für das direkt labeling
    dplyr::rename(name = {{ group}},
                  value = {{ y }})

  ## direkt labeling ja oder nein, wenn nur eine gruppe dargestellt wird = nein
  ## deshalb hier das geom_line und scale_y definieren
  if (dplyr::near(length(labels$name), 1)) {
    geom_line <- NULL
    geom_area <- geom_area(color = color_line,
                           fill = color_area)
                           #alpha = 0.5)

    scale_y <- scale_y_continuous(
      limits = y_limits,
      expand = expansion(mult = c(0, 0.02)),
      breaks = scales::breaks_extended(),
      labels = scales::label_number(big.mark = "'")
      )
  } else {
    geom_area <- NULL
    geom_line <- geom_line(aes(color = {{ group }}))

    scale_y <- scale_y_continuous(
      limits = y_limits,
      expand = expansion(mult = 0.02),
      breaks = scales::breaks_extended(),
      labels = scales::label_number(big.mark = "'"),
                       ## y-achse duplizieren, damit die linien
                       ## direkt beschriftet werden koennen
                       sec.axis = dup_axis(
                         breaks = labels$value,
                         labels = labels$name,
                         name = NULL
                         )
                       )
  }

  ## group-variable als factor damit levels gezaehlt werden
  ## anzahl levels wird verwendet um die farbpalette auszuwählen
  levels <- nlevels(as.factor(dplyr::pull(data, {{ group }})))

  ## farbpaletten benoetigen mindestens 3 werte (n >= 3)
  if (dplyr::near(levels, 2)) {
    colors <- c("#ff7b39", "#4565b2")
  }
  else {
    colors <- colorspace::qualitative_hcl(levels, palette = "Dark 3")
  }

  ## ploten der daten
  plot <-
    ggplot(data = data,
           mapping = aes(x = {{ x }}, y = {{ y }})) +
    ## wird oben definiert
    ## wenn das objekt NULL ist, wird es nicht verwendet
    geom_line +
    geom_area
    #geom_line(aes(color = {{ group }}))

  ## plot darstellen
  plot +
    ## keine erweiterung der x-achse, insbesondere damit
    ## das direkt labeling gut funktioniert bei der y-achse
    scale_x_date(expand = c(0, 0)) +
    ## wird oben definiert
    scale_y +
    ## die legende des aes color entfernen,
    ## damit nur die direkte beschriftung erscheint
    ## dieses scale kommt nur zum zug, wenn mehrere gruppen
    ## dargestellt werden. das heisst, wenn color in aes() verwendet wird
    ## wird color ausserhalb von aes() verwendet, dann wird diese
    ## ignoriert funktion
    scale_color_manual(values = colors,
                       guide = NULL) +
    #guides(color = "none") +
    cowplot::theme_half_open() +
    theme(
      panel.grid.major.y = element_line(color = "grey85", size = 0.5),
      axis.line.y.right = element_blank())
    # cowplot::theme_minimal_hgrid() +
    # theme(
    # axis.line = element_line(color = "black", size = 0.5, lineend = "square"),
    # axis.line.x = NULL,
    # axis.line.y = NULL,
    # axis.ticks = element_line(color = "black", size = 0.5),
    # axis.line.y.right = element_blank()
    # )
}

#' @export
#' @rdname plot_timeseries
plot_timeseries_trend <- function(data, x, y, method = NULL, formula = NULL,
                                  color_line = "black", color_trend = "#0d7abc", y_limits = NULL) {

  ## argument checking
  stopifnot("x muss ein datum (yyyy-mm-dd) sein" = lubridate::is.Date(dplyr::pull(data, {{ x }})))
  stopifnot("y muss numerisch sein." = is.numeric(dplyr::pull(data, {{ y }})))

  plot <-
    ggplot(data = data,
           mapping = aes(x = {{ x }}, y = {{ y }})) +
    geom_line(color = color_line) +
    geom_smooth(
      se = FALSE,
      method = method,
      formula = formula,
      color = color_trend
      )

    plot +
      scale_x_date(expand = c(0, 0)) +
      scale_y_continuous(
        limits = y_limits,
        expand = expansion(mult = c(0, 0.02)),
        breaks = scales::breaks_extended(),
        labels = scales::label_number(big.mark = "'")) +
      cowplot::theme_half_open() +
      theme(panel.grid.major.y = element_line(color = "grey85", size = 0.5))
}

#' @export
#' @rdname plot_timeseries
plot_timeseries_detrend <- function(data, x, y, method = "pc", group, facet = TRUE,
                                    color_line = "#0d7abc", color_area = "#6fa8dc",
                                    y_limits = NULL ) {

    ## argument checking
    stopifnot("y muss numerisch sein." = is.numeric(dplyr::pull(data, {{ y }})))
    ## analog tsbox::ts_trend meldung
    stopifnot("Keine Trendschaetzung fuer Zeitreihen unter 7 Beobachtungen." = nrow(data) > 7)

    ## data preparation
    ## die gruppierung ist notwendig, da unten tsbox::ts_pc ansonsten nicht funktioniert
    ## bei dieser funktion kein zweitwert mehrmals vorkommen pro gruppe (id)
    df <-
      data |>
      dplyr::select({{ group }}, {{ x }}, {{ y }}) |>
      dplyr::mutate(id := {{ group }}, time := {{ x }}, value := {{ y }}, .keep = "none")

    ## methoden fuer detrend checken
    ## https://www.tsbox.help
    methods <- c("pc", "pcy", "pca", "diff", "diffy", "scale")

    if (!(method %in% methods)) {
      message(
        paste0("Methode nicht vorhanden. Es stehen ",
               paste(methods, collapse = ", "),
               " zur Verfuegung.",
               " Weitere infos: https://www.tsbox.help")) # /reference/ts_pc.html
    } else if (method == methods[1]) {
      df_detrend <- tsbox::ts_pc(df)
    } else if (method == methods[2]) {
      df_detrend <- tsbox::ts_pcy(df)
    } else if (method == methods[3]) {
      df_detrend <- tsbox::ts_pca(df)
    } else if (method == methods[4]) {
      df_detrend <- tsbox::ts_diff(df)
    } else if (method == methods[5]) {
      df_detrend <- tsbox::ts_diffy(df)
    } else if (method == methods[6]) {
      df_detrend <- tsbox::ts_scale(df)
    } else {
      message("Fehler in Programmierung. Check Quellcode.")
    }

    ## plot
    plot <-
      ggplot(data = df_detrend,
             mapping = aes(x = time, y = value)) +
      geom_hline(yintercept = 0, color = "grey50", size = 0.5) +
      geom_line(color = color_line) +
      scale_x_date(expand = c(0, 0)) +
      scale_y_continuous(
        limits = y_limits,
        expand = expansion(mult = c(0, 0.02)),
        breaks = scales::breaks_extended(),
        labels = scales::label_number(big.mark = "'")) +
      cowplot::theme_half_open() +
      theme(panel.grid.major.y = element_line(color = "grey85", size = 0.5))

    plot

    ## facet

    ## group-variable als factor damit levels gezaehlt werden
    ## anzahl levels wird verwendet um zu entscheiden ob es ein facet braucht
    levels <- nlevels(as.factor(dplyr::pull(data, {{ group }})))

    if (isTRUE(facet) && levels > 1) {
      plot + facet_wrap(vars(id))
    } else if (isFALSE(facet) && dplyr::near(levels, 1)) {
      plot
    } else if (isFALSE(facet) && levels > 1) {
      message("Achtung!\nDas df hat mehrere Gruppen aber 'facet' hat den Wert FALSE. Ist eine Aufteilung nach Gruppe (facet) sinnvoll?")
      plot
    } else {
      stop("Keine gueltige Eingabe. Pruefe die Argumente facet und group. facet = TRUE braucht mehr als 1 Merkmal in group.")
    }

}
