# advanced boxplot

#  # https://www.cedricscherer.com/2021/06/06/visualizing-distributions-with-raincloud-plots-and-how-to-create-them-with-ggplot2/

# ## install CRAN packages if needed
# pckgs <- c("ggplot2", "systemfonts", "ggforce",
#            "ggdist", "ggbeeswarm", "devtools")
# new_pckgs <- pckgs[!(pckgs %in% installed.packages()[,"Package"])]
# if(length(new_pckgs)) install.packages(new_pckgs)
#
# ## install gghalves from GitHub if needed
# if(!require(gghalves)) {
#   devtools::install_github('erocoar/gghalves')
# }

# setup -------------------------------------------------------------------

library(tidyverse)
library(systemfonts)

# own theme ---------------------------------------------------------------

# wie funktioniert das?

## overwrite default ggplot2 theme
theme_set(
  theme_minimal(
    ## increase size of all text elements
    base_size = 18,
    ## set custom font family for all text elements
    base_family = "Oswald")
)

## overwrite other defaults of theme_minimal()
theme_update(
  ## remove major horizontal grid lines
  panel.grid.major.x = element_blank(),
  ## remove all minor grid lines
  panel.grid.minor = element_blank(),
  ## remove axis titles
  axis.title = element_blank(),
  ## larger axis text for x
  axis.text.x = element_text(size = 16),
  ## add some white space around the plot
  plot.margin = margin(rep(8, 4))
)



# data --------------------------------------------------------------------

set.seed(2021)

data <- tibble(
  group = factor(c(rep("Group 1", 100), rep("Group 2", 250), rep("Group 3", 25))),
  value = c(seq(0, 20, length.out = 100),
            c(rep(0, 5), rnorm(30, 2, .1), rnorm(90, 5.4, .1), rnorm(90, 14.6, .1), rnorm(30, 18, .1), rep(20, 5)),
            rep(seq(0, 20, length.out = 5), 5))
) %>%
  rowwise() %>%
  mutate(value = if_else(group == "Group 2", value + rnorm(1, 0, .4), value))



# plots -------------------------------------------------------------------

n_fun <- function(x){
  return(data.frame(y = median(x) - 1.25,
                    label = paste0("n = ",length(x))))
}

## boxplot with points
# ggbeeswarm::geom_quasirandom() or ggforce::geom_sina()

data |>
  ggplot(aes(x = group, y = value)) +
  geom_boxplot(fill = "grey92") +
  geom_point(
    size = 2,
    alpha = 0.3,
    # overplotting vermeiden
    position = position_jitter(
      # randomness fixiren, breite der streuung der punkte
      seed = 1, width = 0.2
      )
    ) +
  # text hinzüfen, auch mit geom_text() möglich
  stat_summary(
    geom = "text",
    fun.data = n_fun,
    family = "Oswald",
    size = 5
  )

## boxplot with violin (bei vielen datenpunkte -> lesbarkeit und berechnungszeit)
# mit ggplot halbes violin machen evt. mit desity?
# code für ggdist::stat_halfeye() anschauen
data |>
  ggplot(aes(x = group, y = value)) +
  geom_violin(
    fill = "grey72",
    color = NA,
    # breite zeigt die anzahl untersuchungen
    scale = "count",
    # bandweite definieren (breite density kurve)
    bw = 0.5
    ) +
  geom_boxplot(
    fill = NA,
    width = 0.1
  )

## raincloud
data |>
  ggplot(aes(x = group, y = value)) +
  geom_boxplot(width = 0.25, outlier.shape = NA) +
  geom_point(size = 1.3, alpha = 0.3,
             position = position_jitter(seed = 1, width = 0.1)
             #position = position_jitterdodge()
             ) +
  # # horizontale linien anstatt punkte (shape = 95)
  # geom_point(shape = 95, size = 10, alpha = 0.2) +
  ggdist::stat_halfeye(
    adjust = 0.5,
    width = 0.6,
    .width = 0,
    justification = -0.3,
    point_colour = NA
  ) +
  coord_cartesian(xlim = c(1.2, NA), clip = "off")



