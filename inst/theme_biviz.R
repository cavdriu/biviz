#' theme_biviz()
#'
#' @description
#' Diverse Themes für die unterschiedlichen Plots der IV. Sie kontrollieren die
#' nicht Datenbezogenen Eigenschaften eines Plots (z.B. Hintergrund, Linien oder Schriftart).
#' theme_iv() basieren auf den unterschiedlichen theme in pkg cowplot.
#' https://wilkelab.org/cowplot/articles/themes.html
#'
#' https://github.com/clauswilke/dviz.supp/blob/master/R/themes.R
#'
#' @param NULL
#'
#'
#' @return
#'
#' @examples
#' noch zu machen
#'
#' @export
theme_biviz_hgrid <- function() {

  cowplot::theme_minimal_hgrid()

}

#' @export
theme_biviz_vgrid <- function() {

  cowplot::theme_minimal_vgrid()

}
