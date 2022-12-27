#### sandbox ####

library(devtools)
library(tidyverse)
library(lubridate)
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

# test distributions ------------------------------------------------------

gss_lon|>
  filter(happy != "NA") |>
  #drop_na() |>
  #count(age)
  #plot_distributions(as.numeric(age), bw = 3)
  #plot_distributions_density(as.numeric(age), bw = 3)
  plot_distributions_sidebyside(as.numeric(age), group = sex, bw = 3)
  #plot_distributions_grouped(as.numeric(age), group = happy, bw = 5)

gss_lon |>
  filter(!is.na(happy)) |>
  #plot_distributions_raincloud(age, happy)
  plot_distributions_boxplot(age, happy, size = 3)



# test proportionen --------------------------------------------------------------------
mpg |>
  group_by(manufacturer, year) |>
  summarise(n = n()) |>
  group_by(year) |>
  mutate(sum_year = sum(n)) |>
  group_by(manufacturer, year) |>
  summarise(percent = n / sum_year) |>
  ungroup()#|>
  # group_by(year) |>
  # summarise(sum = sum(percent))
  # plot_proportions_sidebyside(x = manufacturer, percent = percent, facet = year)

gss_lon |>
  # müssen dichte plots immer eine numerische x achse haben?
  plot_proportions_sidebyside_density1(x = as.numeric(age), group = happy)
  #plot_proportions_sidebyside_density1(x = as.character(age), group = happy)
  #plot_proportions_sidebyside_density1(x = age, group = happy)

# age = labelled -> prüfungs muss in der funktion noch implementiert werden
gss_lon |>
  mutate(age = as.numeric(age)) |>
  select(age, happy) |>
  filter(!is.na(happy)) |>
  plot_proportions_sidebyside_density2(x = age, group = happy)

gss_sib |>
  group_by(year, sex) |>
  summarise(n = n()) |>
  group_by(year) |>
  mutate(sum_year = sum(n)) |>
  group_by(year, sex) |>
  summarise(percent = n / sum_year) |>
plot_proportions_stacked(year, percent, sex)

mpg |>
  group_by(year) |>
  summarise(n = n()) |>
  ungroup() |>
  mutate(percent = n / sum(n)) |>
  plot_proportions_donut(group = year)
  #plot_proportions(n = percent, year)



# test timeseries ---------------------------------------------------------

df_temp <-
  mpg |>
  #mutate(year = as.factor(year)) |>
  group_by(year, manufacturer) |>
  summarise(hwy_by_manufacturer = mean(hwy)) |>
  ungroup() #|>

plot_timeseries_slope(data = df_temp, x = year, y = hwy_by_manufacturer, group = manufacturer)

tech_stocks |>
  select(company, date, price_indexed) |>
  filter(company == "Facebook") |>
  plot_timeseries_line(x = date, y = price_indexed, group = company)

tech_stocks |>
  select(company, date, price_indexed) |>
  filter(company == "Apple") |>
  plot_timeseries_trend(x = date, y = price_indexed, method = "loess") #+  facet_wrap(vars(company))

## detrendig

tech_stocks |>
  #slice(6) |>
  #filter(company == "Facebook") |>
  # mutate(date = year(date)) |>
  mutate(date = format(date, "%Y-%m")) |>
  # #mutate(date = as.yearmon(date, "%Y %m")) |>
  group_by(company, date) |>
  summarise(price_indexed = mean(price_indexed)) |>
  ungroup() |>
  plot_timeseries_detrend(x = date, y = price_indexed, method = "pc", group = company, facet = TRUE)

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

