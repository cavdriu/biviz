
<!-- README.md is generated from README.Rmd. Please edit that file -->

# biviz

<!-- badges: start -->
<!-- badges: end -->

Mit biviz können häufig verwendete Datenvisualisierungen mit einfachen
Funktionen erstellt werden. biviz stellt insbesondere
Datenvisualisierungen die oft im Bereich Business Intelligence (BI)
verwendet werden zur Verfügung. Das Paket implementiert Trends der
Datenvisualisierung, wodurch sich das Paket auch für andere Zwecke
eignet. In biviz werden [ggplot2](https://ggplot2.tidyverse.org/)
Wrapper zur Verfügung gestellt, welche die gängigen
Datenvisualisierungen erzeugen. Dies hat zum Vorteil, dass bei
Standardgrafiken die Datenvisualisierung nicht Schicht für Schicht
programmiert werden, wie es beim ggplot2 Framework vorgesehen ist. Eine
Zeile Code reicht um eine Datenvisualisierung zu erstellen. Da das Paket
auf ggplot2 aufbaut kann das erzeugte Objekt, zu einem späteren
Zeitpunkt dennoch angepasst werden.

Die Datenvisualisierungen orientieren sich Stark am Buch [Data
Visualization von Claus O. Wilke](https://clauswilke.com/dataviz/)

## Installation

Das Paket biviz kann direkt von [GitHub](https://github.com/)
installiert werden:

``` r
# install.packages("devtools")
devtools::install_github("tricktracktriu/biviz")
```

## Intro

Um die Wrapper schlank zu halten, werden bei den meisten Funktionen in
biviz die Daten vor der Übergabe in die Funktion so aufbereitet, dass in
der Visualisierung die Datenpunkte in den Daten abgebildet werden und
keine Berechnungen innerhalb der Visualisierungsfunktion vorgenommen
werden.

Die Visualisierungen sind in 4 Gruppen (amounts, distributions,
proportions und time series) aufgeteilt und folgen immer der gleichen
Syntax. Jede Funktion startet mit `plot_*` anschliessend wird die Gruppe
definiert `plot_amounts_*` und am Ende die Form `plot_amounts_grouped`.

## Beispiele

``` r
library(biviz)

plot1 <- 
  socviz::gss_lon |> 
  tidyr::drop_na() |>
  plot_distributions_sidebyside(
    as.numeric(age), 
    group = sex, 
    bw = 3)
#> [1] "Achtung!! Die untersuchte Variable beinhaltet 0 NAs."

plot1
```

<img src="man/figures/README-socviz-1.png" width="100%" />

### Datenvisualisierungen verfeinern

Da der Output von biviz ein ggplot2 Objekt ist, können es anschliessend
innerhalb des ggplot2 Frameworks weiter bearbeitet werden. Der ganze
Datensatz aus dem Beipsiel stehen unter
[opendata.swiss](https://opendata.swiss/de/dataset/abfallmengen-und-abfallgebuhren-in-den-gemeinden-im-kanton-zurich-ab-2000/resource/2f2cd364-6f42-4611-be73-98d3f0bb6acc)
frei zur Verfügung.

``` r
df <- readr::read_csv2("data/sample_abfall_zh.csv")

plot2 <- 
  df |> 
  plot_amounts_grouped(
    x = forcats::fct_reorder(Gemeinde, Wert),
    y = Wert, 
    group = Gemeinde)

plot2
```

<img src="man/figures/README-abfall-1.png" width="100%" />

``` r

plot3 <- 
  plot2 +
  ggplot2::ggtitle("Anzahl Brennbare Abfälle und Sperrgut\nje Gemeinde im Jahr 2021") +
    ggplot2::labs(
    x = "Gemeinde",
    y = "Menge in Tonnen"
  ) +
  ggplot2::theme(legend.position = "none")

plot3
```

<img src="man/figures/README-abfall-2.png" width="100%" />

``` r

plot4 <- 
  plot3 +
  ggplot2::geom_col(
    data = dplyr::filter(df, abfall_pro_person != min(df$abfall_pro_person)),
    mapping = ggplot2::aes(
      x = forcats::fct_reorder(Gemeinde, Wert),
      y = Wert
      ),
    fill = "lightgrey",
    position = "dodge"
           ) +
    ggplot2::ggtitle(
    paste0(
      "<span style = 'color:lightgrey;'>Im Jahr 2021 hatte die </span><br>",
      "<span style = 'color:#909800; style = font-size:24pt'>Gemeinde Affoltern a.A.</span><br>",
      "mit 5.6 Tonnen ",
      "<span style = 'color:lightgrey;'>brennbaren Abfällen <br>und Sperrgut die</span>",
      " kleinste pro Kopf Abfallmenge"
      )
    ) +
    ggplot2::labs(y = "Abfallmenge Total\nin Tonnen") +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(),
      axis.title.x =  ggplot2::element_blank()
      ) 

plot4 
```

<img src="man/figures/README-abfall-3.png" width="100%" />

``` r

## plots zusammenfügen
#cowplot::plot_grid(plot2, plot3, plot4, NULL, labels = c("A", "B", "C"), ncol = 2)
# first_row <- cowplot::plot_grid(plot2, plot3, labels = c("A", "B"))
# cowplot::plot_grid(first_row, plot4, labels = c("", "C"), ncol = 1)
```

## Help

Die einzelnen Dokumentationen zu den Funktionen sind mit `help()` oder
`?function` ersichtlich alternativ kann mit `example()` Beispiele
aufgerufen werden.

``` r

?plot_distribution
#> Keine Dokumentation für 'plot_distribution' in angegebenen Paketen und Bibliotheken:
#> Sie können '??plot_distribution' versuchen
```
