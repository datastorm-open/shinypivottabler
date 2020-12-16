#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinypivottabler)

# Define UI for application that draws a histogram
navbarPage(title = HTML(paste0('<p style="margin-top: 0.05cm;">', paste0(rep("&nbsp;", 25), collapse = ""), '&nbspshinypivottabler</p>')), id = "nav-id", collapsible = TRUE,
           position = "fixed-top", theme = "css/custom.css",
           header = div(
             br(), br(), br(), br(),
             a(href = "https://www.datastorm.fr",
               target = "_blank", img(src = "img/img-datastorm-logo-white.png", class = "ribbon", style = "margin-left: 0cm;margin-top: 0.1cm;height: 55px")),
             a(href = "https://github.com/datastorm-open/shinypivottabler",
               target = "_blank", img(src = "img/github.png", class = "ribbon", style = "margin-left: 3cm;margin-top: 0cm;height: 60px")),
             singleton(tags$script(src = 'events.js')),
             singleton(tags$script(src = 'is.min.js')),
             # footer
             div(class = "ds_app_footer", div(p("copyright © Datastorm 2020", style = "color:white"), align = "center")),
           ),
           windowTitle = "shinypivottabler",
           tabPanel("Introduction",
                    includeMarkdown("www/script/intro.md")
           ),
           tabPanel("Dataset",
                    shiny::dataTableOutput("dataset")
           ),
           tabPanel("Basic",
                    includeMarkdown("www/script/basic.md"),
                    hr(),
                    shinypivottablerUI(id = "basic")
           ),
           navbarMenu("Advanced",
                      tabPanel("CSS",
                               includeMarkdown("www/script/ui_css.md"),
                               hr(),
                               shinypivottablerUI(id = "table",
                                                  app_colors = c("#e6e6e6", "#430838"),
                                                  app_linewidth = 3
                               )
                      ),
                      tabPanel("New functions",
                               includeMarkdown("www/script/fct.md"),
                               hr(),
                               shinypivottablerUI(id = "fct",
                                                  app_colors = c("#e6e6e6", "#430838"),
                                                  app_linewidth = 3
                               )
                      ),
                      tabPanel("Initialization",
                               includeMarkdown("www/script/init.md"),
                               hr(),
                               shinypivottablerUI(id = "init",
                                                  app_colors = c("#e6e6e6", "#430838"),
                                                  app_linewidth = 3
                               )
                      )

           ),
           br(), br(), br()
)
