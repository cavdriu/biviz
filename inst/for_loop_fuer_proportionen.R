
gss_happy <- select(gss_lon, age, happy) |>
  filter(!is.na(happy)) |>
  mutate(age = as.factor(age))

build_group <- function(data, variable, focus_group){

  data |>
    mutate(
      focus = as.character(fct_collapse({{ variable }},
                                          factor = focus_group,
                                          other = setdiff(levels(pull({{ data }}, {{ variable }})),
                                                          focus_group))),
      # notwendig, dass bei fc_collapse() nicht direkt den wert aus focus_group extrahiert werden kann als name
      focus = ifelse(focus == "factor", focus_group, focus),
      higlight = focus_group
    )
}

k <- build_group(gss_happy, happy, "Very Happy") #|> count(focus)

###

df_build_group <- function(data, variable) {

  output <- tibble()
  var <- pull(data, {{ variable }})
  focus_group <- levels(var)

  for (i in seq_along(focus_group)) {
    df <- build_group(data, {{ variable }}, focus_group[[i]])
    output <- bind_rows(output, df)
  }

  output

}

df_build_group(gss_happy, happy) |> filter(is.na(age))
  # factor arg bei plot funktion ergänzen, damit die reihenfolge kontrolliert werden kann
  #mutate(higlight = factor(higlight, levels = levels(lev)))

output <- c()
for (i in seq_along(df)) {
  output[[i]] <- is.na(df[[i]])
}


