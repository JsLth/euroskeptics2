requireNamespace("stats")
requireNamespace("bslib")
requireNamespace("colorspace")
requireNamespace("countrycode")
requireNamespace("dplyr")
requireNamespace("ggplot2")
requireNamespace("scales")
requireNamespace("sf")
requireNamespace("shiny")
requireNamespace("shinyWidgets")
requireNamespace("waiter")
requireNamespace("munsell")

code_files <- dir("R", pattern = "\\.R$", full.names = TRUE) |>
  normalizePath()
for (file in code_files) {
  source(file)
}

data <- load("R/sysdata.rda")
for (obj in data) {
  assign(obj, get(obj), envir = environment(ui))
  assign(obj, get(obj), envir = environment(server))
}

run_app()
