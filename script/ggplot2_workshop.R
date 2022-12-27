#### ggplot2 workshop ####

# source: https://rstudio-conf-2022.github.io/ggplot2-graphic-design/

# ### Introduction
#
# {ggplot2} is a system for declaratively creating graphics, based on "The Grammar of Graphics" (Wilkinson, 2005).
#
# You provide the data, tell {ggplot2} how tomap variables to aesthetics, what graphical primitives to use, and it takes care of the details.
#
# https://github.com/allisonhorst/stats-illustrations
#
# ### Concept Pt. 1
#
# -   {ggplot2} is a powerful library for reproducible graphic design
# -   the components follow a consistent syntax each ggplot needs at least data, some aesthetics, and a layer
# -   we set constant propeties outside aes() ...
# -   and map data-related properties inside aes()
# -   local settings and mappings override global properties
# -   grouping allows applying layers for subsets
# -   we can store a ggplot object and extend it afterwards
# -   we can change the appearance for all plots with theme_set() and theme_update()
#
# Component Function Explanation Data ggplot(data) The raw data that you want to visualise. Aesthetics aes() Aesthetic mappings between variables and visual properties. Geometries geom\_*() The geometric shapes representing the data. Statistics stat\_*() The statistical transformations applied to the data. Scales scale\_*() Maps between the data and the aesthetic dimensions. Coordinate System coord\_*() Maps data into the plane of the data rectangle. Facets facet\_*() The arrangement of the data into a grid of plots. Visual Themes theme() and theme\_*() The overall visual defaults of a plot.
#
# Aesthetic Mapping = link variables to graphical properties: - positions (x, y) - colors (color, fill) - shapes (shape, linetype) - size (size) - transparency (alpha) - groupings (group)
#
# aes() inside, explicit matching `ggplot(data = bikes, mapping = aes(x = temp_feel, y = count))`
#
# Geometries (geom\_\*) = interpret aesthetics as graphical representations
#
# Visual Properties of Layers
#
# Setting vs Mapping of Visual Properties
#
# Mapping Expressions
#
# Local vs. Global Encoding
#
# Adding More Layers
#
# Overwrite Global Aesthetics
#
# `stat_*()` and `geom_*()`
#
# Statistical Summaries
#
# -   stat_summary()
#
# Inspect a ggplot Object
#
# Extend a ggplot Object: Add Layers & Remove a Layer from the Legend
#
# -   `geom_rug(alpha = 0.2, show.legend = FALSE)`
#
# Extend a ggplot Object: Add Labels
#
# -   `labs()`
#
# -   `x = NULL`um ein lab zu entfernen
#
# Change the Theme Base Settings g + theme_light( base_size = 14, base_family = "Roboto Condensed") theme_set(theme_light(...)) -\> globale Veränderung
#
# # install.packages("systemfonts")
# library(systemfonts)
# system_fonts() %>%
#   filter(str_detect(family, "Cabinet")) %>%
#   pull(name) %>%
#   sort()
#
# register_variant(
#   name = "Cabinet Grotesk Black",
#   family = "Cabinet Grotesk",
#   weight = "heavy",
#   features = font_feature(letters = "stylistic")
# )
#
# Overwrite Specific Theme Settings
#
# g +
#   theme(
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(face = "bold"),
#     legend.position = "top",
#     plot.title.position = "plot"
#   )
#
# theme_update(
#   panel.grid.minor = element_blank(),
#   plot.title = element_text(face = "bold"),
#   legend.position = "top",
#   plot.title.position = "plot"
# )
#
# g
#
# Save the Graphic
#
# ggsave(g, filename = "my_plot.png")
#
# ggsave("my_plot.png")
#
# ggsave("my_plot.png", width = 8, height = 5, dpi = 600)
#
# ggsave("my_plot.pdf", width = 20, height = 12, unit = "cm", device = cairo_pdf)
#
# grDevices::cairo_pdf("my_plot.pdf", width = 10, height = 7)
# g
# dev.off()
#
# Vector graphic vs.Raster graphic https://rstudio-conf-2022.github.io/ggplot2-graphic-design/materials/img/concepts/vector-raster-canva.png
#
# ### How to Work with Aspect Ratios
#
# don't rely on the Rstudio viewer pane! once you have a "it's getting close" prototype, settle on a plot size
#
# Approach 1: save the file to disk and inspect it; go back to your IDE
#
# -   tedious and time-consuming...
#
# **Approach 2: use a qmd or rmd with inline output and chunk settings**
#
# -   **set fig.width and fig.height per chunk or globally**
#
# -   rmd:
#
#     \`\`\`{r plot, fig.width = 10, fig.height = 4} plot \`\`\`
#
# Approach 3: use our {camcorder} package
#
# -   saves output from all ggplot() calls and displays it in the viewer pane
# -   gg_record()


# setup -------------------------------------------------------------------

packages <- c(
  "ggplot2", "readr", "tibble", "tidyr", "forcats", "stringr",
  "lubridate", "here", "systemfonts", "magick", "scales", "grid",
  "grDevices", "colorspace", "viridis", "RColorBrewer", "rcartocolor",
  "scico", "ggsci", "ggthemes", "nord", "MetBrewer", "ggrepel",
  "ggforce", "ggtext", "ggdist", "ggbeeswarm", "gghalves", "patchwork",
  "palmerpenguins", "rnaturalearth", "sf", "rmapshaper", "devtools"
)

install.packages(setdiff(packages, rownames(installed.packages())))

## install {colorblindr} and requirements
remotes::install_github("wilkelab/cowplot")
remotes::install_github("clauswilke/colorblindr")

# Part 1 -------------------------------------------------------------------

bikes <- readr::read_csv(
  here::here("data", "london-bikes-custom.csv"),
  col_types = "Dcfffilllddddc"
)

bikes$season <- forcats::fct_inorder(bikes$season)



