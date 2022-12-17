# Mengen plots testen

# Data für Test -----------------------------------------------------------
library(dplyr)

test_df <-
  ggplot2::mpg |>
  group_by(manufacturer, class) |>
  summarise(n = n()) |>
  ungroup()

# Standard Barplot --------------------------------------------------------

test_that("plot_amounts() plots as expected", {
  local_edition(3)
  vdiffr::expect_doppelganger(
    title = "plot_amounts()",
    fig = plot_amounts(
      data = test_df,
      x = manufacturer,
      y = n
    )
  )
})

# Horizontaler Barplot --------------------------------------------------------

test_that("plot_amounts_horizontal() plots as expected", {
  local_edition(3)
  vdiffr::expect_doppelganger(
    title = "plot_amounts_horizontal()",
    fig = plot_amounts_horizontal(
      data = test_df,
      x = manufacturer,
      y = n
    )
  )
})


# Gruppierter Barplot -----------------------------------------------------

test_that("plot_amounts_grouped()) plots as expected", {
  local_edition(3)
  vdiffr::expect_doppelganger(
    title = "plot_amounts_grouped()",
    fig = plot_amounts_grouped(
      data = test_df,
      x = manufacturer,
      y = n,
      group = class
    )
  )
})

# Facetten (Teilaspekte) Barplot -----------------------------------------------------

test_that("plot_amounts_facets() plots as expected", {
  local_edition(3)
  vdiffr::expect_doppelganger(
    title = "plot_amounts_facets()",
    fig = plot_amounts_facets(
      data = test_df,
      x = manufacturer,
      y = n,
      facet = class
    )
  )
})

# Gestapelter Barplot -----------------------------------------------------

test_that("plot_amounts_stacked() plots as expected", {
  local_edition(3)
  vdiffr::expect_doppelganger(
    title = "plot_amounts_stacked()",
    fig = plot_amounts_stacked(
      data = test_df,
      x = manufacturer,
      y = n,
      group = class
    )
  )
})

