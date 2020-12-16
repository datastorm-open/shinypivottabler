#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$dataset <- shiny::renderDataTable(
        data
    )

    shiny::callModule(module = shinypivottabler,
                      id = "basic",
                      data = data)

    shiny::callModule(module = shinypivottabler,
                      id = "table",
                      show_title = FALSE,
                      theme = theme,
                      data = data)


    shiny::callModule(module = shinypivottabler,
                      id = "fct",
                      show_title = FALSE,
                      theme = theme,
                      additional_expr_num = additional_expr_num,
                      additional_expr_char = additional_expr_char,
                      additional_combine = additional_combine,
                      data = data)

    shiny::callModule(module = shinypivottabler,
                      id = "init",
                      show_title = FALSE,
                      theme = theme,
                      initialization = initialization,
                      data = data)

})
