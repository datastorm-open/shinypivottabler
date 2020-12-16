require(shinypivottabler)

# create artificial dataset
data <- data.frame("V1" = sample(c("A", "B", "C", "D"), size = 1000000,
                                 prob = rep(1, 4), replace = T),
                   "V2" = sample(c("E", "F", "G", "H"), size = 1000000,
                                 prob = rep(1, 4), replace = T),
                   "V3" = sample(c("I", "J", "K", "L"), size = 1000000,
                                 prob = rep(1, 4), replace = T),
                   "V4" = sample(c("M", "N", "O", "P"), size = 1000000,
                                 prob = rep(1, 4), replace = T),
                   "V5" = 1:1000000,
                   "V6" = 1000000:1)

initialization <- list(
  "rows" = c("V1", "V2"),
  "cols" = "V3",
  "target" = "V4",
  "combine_target" = NULL,
  "idc" = "Count",
  "combine_idc" = NULL,
  "combine" = NULL,
  "idcs" = c(list(c("label" = "Init_variable_1",
                    "target" = "V3", "idc" = "Count",
                    "nb_decimals" = 0,
                    "sep_thousands" = " ",
                    "sep_decimal" = ".",
                    "prefix" = "",
                    "suffix" = "",
                    "combine" = "/", "combine_target" = "V4", "combine_idc" = "Count")),
             list(c("label" = "NULL",
                    "target" = "V3", "idc" = "Count")))
)

server = function(input, output, session) {
  shiny::callModule(module = shinypivottabler,
                    id = "id",
                    data = data,
                    pivot_cols = c("V1", "V2", "V3", "V4"),
                    initialization = initialization)
}

ui = shiny::fluidPage(
  shinypivottablerUI(id = "id", app_colors = NULL, app_linewidth = 0)
)

shiny::shinyApp(ui = ui, server = server)
