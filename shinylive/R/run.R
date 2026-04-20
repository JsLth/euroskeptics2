run_app <- function() {
  shinyApp(ui = ui, server = server)
}


utils::globalVariables(c(
  "age", "bl_prob", "gender", "group", "label", "predictor",
  "prob", "prob_change", "var"
))
