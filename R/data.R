#' Abfallmengen nach Abfallarten und Gemeinden Kanton Zuerich
#'
#' Source: https://opendata.swiss/de/dataset/abfallmengen-und-abfallgebuhren-in-den-gemeinden-im-kanton-zurich-ab-2000/
#'
#' Eine Teilmenge ist im Datensatz "sample_abfall_zh" zusammengefasst und mit Informationen vom
#' [Gemeindeportraet](https://www.zh.ch/de/politik-staat/gemeinden/gemeindeportraet.html) ergänzt.
#'
#' @format
#' Ein Datensatz mit 6 Variablen und 34668 Zeilen:
#' \describe{
#'    \item{Jahr}{Jahr der Erhebung}
#'    \item{Gemeinde_Nr}{Kantonale Gemeinde ID}
#'    \item{Gemeinde}{Name der Gemeinde}
#'    \item{Abfallart}{Spezifizierung des Abfalles}
#'    \item{Wert}{numerische Mengenangabe}
#'    \item{Einheit}{Masseinheit des Wertes}
#' }
"abfall_zh"
