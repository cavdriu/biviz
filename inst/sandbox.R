#### sandbox ####

library(devtools)
library(tidyverse)
library(lubridate)
library(socviz)
library(biviz)
load_all()
#library(socviz)  # Book: Data Visualization by Kieran Healy, https://github.com/kjhealy/socviz


# data --------------------------------------------------------------------

gss_sm <- socviz::gss_sm
gss_sib <- socviz::gss_sib
gss_lon <- socviz::gss_lon            # als test df ?
midwest
mpg <- ggplot2::mpg
AirPassengers # data()

tech_stocks <-
  read_csv("https://wilkelab.org/SDS375/datasets/tech_stocks.csv") |>
  mutate(date = ymd(date))


# test amounts ------------------------------------------------------------

gss_lon |>
  #mutate(n = n()) |>
  count(degree) |>
  plot_amounts_vertical(degree, n)
  #plot_amounts_vertical(n, degree)
  #plot_amounts_horizontal(degree, count)
  #count(age, degree) |>
  #plot_amounts_facets(age, n, degree)
  #count(sex, degree) |>
 #plot_amounts_grouped(x = degree, y = n, group = sex) #+
  #scale_fill_discrete_sequential(palette = "Purples 2")
  plot_amounts_stacked(x = degree, n, sex)

  gss_lon |>
    mutate(count = n()) |>
    count(degree, sex) |>
    group_by(degree, sex) |>
    filter(n() > 1) |>
    nrow() < 1

# test distributions ------------------------------------------------------

gss_lon|>
  #filter(happy != "NA") |>
  #drop_na() |>
  #count(age)
  #mutate(age = as.character(age)) |>
  #plot_distributions_histogram(age)#, bw = 3)
  #mutate(age = as.character(age)) |>
  #plot_distributions_density(age, bw = 3)
  #plot_distributions_sidebyside(x = age, group = sex)
  drop_na(happy) |>
  plot_distributions_grouped(age, group = happy, bw = 3)

gss_lon |># nrow()
  #filter(!is.na(happy)) |>
  filter(row_number() %% 100 == 1) |>
  #plot_distributions_raincloud(y = age, happy)
  plot_distributions_boxplot(y = age, x = happy, size = 3)



# test proportionen --------------------------------------------------------------------
mpg |>
  group_by(manufacturer, year) |>
  summarise(n = n()) |>
  group_by(year) |>
  mutate(sum_year = sum(n)) |>
  group_by(manufacturer, year) |>
  summarise(percent = n / sum_year) |>
  ungroup() |>
  # group_by(year) |>
  # summarise(sum = sum(percent))
  plot_proportions_sidebyside_bar(x = manufacturer, percent = percent, facet = year)

gss_lon |>
  #count(age, happy) |>
  # müssen dichte plots immer eine numerische x achse haben?
  #plot_proportions_sidebyside_density1(x = age, group = happy)
  #mutate(age = as.character(age)) |>
  plot_proportions_sidebyside_density1(x = age, group = happy)
  #plot_proportions_sidebyside_density1(x = age, group = happy)

# age = labelled -> prüfungs muss in der funktion noch implementiert werden
socviz::gss_lon |>
  mutate(age = as.numeric(age)) |>
  select(age, happy) |>
  filter(!is.na(happy)) |>
  plot_proportions_sidebyside_density2(x = age, group = happy)

gss_sib |>
  #filter(year == 1980) |>
  #group_by(year, sex) |>
  #summarise(n = n()) |>
  count(year, sex) |>
  group_by(year) |>
  mutate(sum_year = sum(n)) |>
  group_by(year, sex) |>
  summarise(percent = n / sum_year) |>
  ungroup() |> #pull(year) |> is.numeric()
  #mutate(year = as.factor(year)) |>
  #plot_proportions_bar(group = sex)
  plot_proportions_stacked(x = year, y = percent, group = sex)
  #plot_proportions_sidebyside_col(x = year, y = percent, color = sex, facet = sex)


mpg |>
  # group_by(year) |>
  # reframe(n = n(),
  #         manufacturer = manufacturer) |>
  count(year) |>
  ungroup() |>
  mutate(percent = n / sum(n)) |> #glimpse()
  #plot_proportions_donut(group = year)
  plot_proportions_bar(year)
  facet_wrap(vars(manufacturer))



# test timeseries ---------------------------------------------------------

ggplot2::mpg |>
  group_by(year, manufacturer) |>
  summarise(hwy_by_manufacturer = mean(hwy)) |>
  ungroup() |>
  plot_timeseries_slope(x = year, y = hwy_by_manufacturer, group = manufacturer)

ts_tbl(EuStockMarkets) |>
  mutate(time = as_date(time)) |>
  #filter(id != "FTSE") |>
  #filter(id == "FTSE") |>
  #plot_timeseries_line(x = time, y = value, group = id)
  #plot_timeseries_trend(x = time, y = value) + facet_wrap(vars(id))
  mutate(time = format(time, "%Y-%m")) |>
  group_by(id, time) |>
  summarise(value = mean(value)) |>
  ungroup() |>
  plot_timeseries_detrend(x = time, y = value, group = id)




tech_stocks |>
  select(company, date, price_indexed) |>
  filter(company == "Apple") |>
  plot_timeseries_line(x = date, y = price_indexed, group = company)

tech_stocks |>
  select(company, date, price_indexed) |>
  #filter(company == "Apple") |>
  plot_timeseries_trend(x = date, y = price_indexed, method = "loess") +  facet_wrap(vars(company))

## detrendig

tech_stocks |>
  #slice(6) |>
  #filter(company == "Facebook") |>
  # mutate(date = year(date)) |>
  mutate(date = format(date, "%Y-%m")) |>
  # #mutate(date = as.yearmon(date, "%Y %m")) |>
  #filter(company == "Apple") |>
  #group_by(date) |>
  group_by(company, date) |>
  summarise(price_indexed = mean(price_indexed)) |>
  ungroup() |>
  plot_timeseries_detrend(x = date, y = price_indexed, group = company) #, facet = FALSE)#, group = company, facet = TRUE)
  ## wenn nur eine gruppe
  plot_timeseries_detrend(x = date, y = price_indexed, method = "pc", group = company, facet = FALSE)


# library(tsbox)
# temp <- fdeaths |>
#   ts_tbl()
#
# temp |>
#   mutate(group = "group") |>
#   plot_timeseries_detrend(x = time, y = value, group = group, facet = FALSE, method = "scale")
#
# temp |>
#   #ts_pc() |>
#   ts_scale() |>
#   ts_ggplot()
# #   mutate(id = "fdeaths") |>
# #   ts_pc() |>
# #   #ts_trend() |>
# #   #ts_scale() |>
# #   ts_ggplot()
#
# library(zoo)
# temp_m <- lm(temp$value ~ temp$time)
# temp_detr <- zoo(resid(temp_m), temp$time)
# autoplot(temp_detr)
#
# # https://stackoverflow.com/questions/37704212/extract-month-and-year-from-date-in-r
# # https://rc2e.com/timeseriesanalysis#recipe-id088
# # https://stackoverflow.com/questions/10128617/test-if-characters-are-in-a-string
#
# load(here::here("inst", "yield.Rdata"))
# m <- lm(coredata(yield) ~ index(yield))
# detr <- zoo(resid(m), index(yield))
# autoplot(detr)

# erinnerung --------------------------------------------------------------------

plot_bar(gss_sm,forcats::fct_reorder(region, childs, count), childs, color = "black")
# https://stackoverflow.com/questions/63269490/reorder-a-variable-by-another-object-variable-in-r
# https://forcats.tidyverse.org/reference/fct_reorder.html

plot_bar_v(gss_sm, agegrp, childs) #+
  ggplot2::labs(
    x = "Movies",
    title = "Something"
  ) +
  cowplot::theme_minimal_vgrid()


  set.seed(1)

  x.Date <- as.Date(paste(2004, rep(1:4, 4:1), sample(1:28, 10), sep = "-"))
  x <- zoo::zoo(rnorm(12), x.Date)
  zoo::rollmean(x, 3)

# temp2 <- temp |> filter(row_number() %% 214 == 1)
# df.new = df[seq(1, nrow(df), 5), ]


# 21.01.2023 --------------------------------------------------------------

p <- ggplot(data = midwest,
              mapping = aes(x = area, color = state))
p + geom_density(alpha = 0.3)
p + geom_line(stat = "density")


# 06.02.2023 --------------------------------------------------------------

stock_markets <- tsbox::ts_tbl(datasets::EuStockMarkets) |>
  dplyr::mutate(time = lubridate::as_date(time))
stock_markets |>
  dplyr::filter(id != "FTSE") |>
  plot_timeseries_trend(x = time, y = value) +
  # Erweiterung mit ggplot2 Funktion
  ggplot2::facet_wrap(ggplot2::vars(id))
