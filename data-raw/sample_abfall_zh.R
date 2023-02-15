## code to prepare `sample_abfall_zh` dataset goes here

sample_abfall_zh <-
  biviz::abfall_zh |>
  dplyr::select(!c("Gemeinde_Nr", "Einheit")) |>
  dplyr::filter(
    Gemeinde %in% c("Aeugst a.A.",
                    "Affoltern a.A.",
                    "Bonstetten",
                    "Hausen a.A."),
    Abfallart == "Brennbare Abfälle und Sperrgut",
    dplyr::row_number() %% 214 == 1
  ) |>
  dplyr::arrange(Gemeinde) |>
  # quelle: https://www.zh.ch/de/politik-staat/gemeinden/gemeindeportraet.html
  dplyr::mutate(Bevoelkerung = c(1986, 12358, 5632, 3850)) |>
  dplyr::group_by(Gemeinde) |>
  dplyr::mutate(abfall_pro_person = Bevoelkerung / Wert)

usethis::use_data(sample_abfall_zh, overwrite = TRUE)
