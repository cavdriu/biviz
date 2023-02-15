## code to prepare `sample_abfall_zh` dataset goes here
library(biviz)
library(dplyr)

sample_abfall_zh <-
  biviz::abfall_zh |>
  select(!c("Gemeinde_Nr", "Einheit")) |>
  filter(
    Gemeinde %in% c("Aeugst a.A.",
                    "Affoltern a.A.",
                    "Bonstetten",
                    "Hausen a.A."),
    Abfallart == "Brennbare Abfälle und Sperrgut",
    row_number() %% 214 == 1
  ) |>
  arrange(Gemeinde) |>
  # quelle: https://www.zh.ch/de/politik-staat/gemeinden/gemeindeportraet.html
  mutate(Bevoelkerung = c(1986, 12358, 5632, 3850)) |>
  group_by(Gemeinde) |>
  mutate(abfall_pro_person = Bevoelkerung / Wert)

usethis::use_data(sample_abfall_zh, overwrite = TRUE)
