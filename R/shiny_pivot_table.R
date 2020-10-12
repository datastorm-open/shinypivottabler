###############################
###  PIVOT TABLE FOR SHINY  ###
###############################



get_expr <- function(idc, target) {
  text_idc <- list(
    "Count" = "'n()'",
    "Count_distinct" = "paste0('n_distinct(', target, ', na.rm = TRUE)')",
    "Sum" = "paste0('sum(', target, ', na.rm = TRUE)')",
    "Mean" = "paste0('mean(', target, ', na.rm = TRUE)')",
    "Min" = "paste0('min(', target, ', na.rm = TRUE)')",
    "Max" = "paste0('max(', target, ', na.rm = TRUE)')",
    "Standard_deviation" = "paste0('sd(', target, ', na.rm = TRUE)')"
  )
  
  return(eval(parse(text = text_idc[[idc]])))
}



#' Shiny module to render and export pivot tables.
#'
#' @param input shiny input
#' @param output shiny input
#' @param session shiny input
#' @param id \code{character}. Module input to allow multiple instanciation of the module.
#' @param data \code{data.frame} / \code{data.table}. Initial data table.
#' @param pivot_cols \code{character} (NULL). Columns to be used as pivot in rows and cols.
#' @param indicator_cols \code{character} (NULL). Columns on which indicators will be calculated.
#' @param theme \code{list} (NULL). Theme to customize the output of the pivot table.
#' @param export_styles \code{boolean} (TRUE). Whether or not to apply styles (like the theme) when exporting to Excel. 
#' Some styles may not be supported by Excel.
#'
#' @return Nothing. Starts a Shiny module.
#' 
#' @import pivottabler shiny shinyjs openxlsx shinyBS
#' 
#' @export
#' @rdname shiny_pivot_table
#'
#' @examples
#' \dontrun{\donttest{
#' 
#' # create artificial dataset
#' data <- data.table("V1" = sample(c("A", "B", "C", "D"), size = 1000000, prob = runif(4, 0, 1), replace = T),
#'                    "V2" = sample(c("E", "F", "G", "H"), size = 1000000, prob = runif(4, 0, 1), replace = T),
#'                    "V3" = sample(c("I", "J", "K", "L"), size = 1000000, prob = runif(4, 0, 1), replace = T),
#'                    "V4" = sample(c("M", "N", "O", "P"), size = 1000000, prob = runif(4, 0, 1), replace = T),
#'                    "V5" = 1:1000000,
#'                    "V6" = 1000000:1)
#'                    
#' theme <- list(
#'   fontName="Courier New, Courier",
#'   fontSize="1.5em",
#'   headerBackgroundColor = "#217346",
#'   headerColor = "rgb(255, 255, 255)",
#'   cellBackgroundColor = "rgb(255, 255, 255)",
#'   cellColor = "rgb(0, 0, 0)",
#'   outlineCellBackgroundColor = "rgb(192, 192, 192)",
#'   outlineCellColor = "rgb(0, 0, 0)",
#'   totalBackgroundColor = "#59bb28",
#'   totalColor = "rgb(0, 0, 0)",
#'   borderColor = "rgb(64, 64, 64)")
#'                    
#' ui = shiny::fluidPage(
#'   shinypivottablerUI(id = "id")
#' )
#' 
#' server = function(input, output, session) {
#'   shiny::callModule(module = shinypivottabler, 
#'                     id = "id", 
#'                     data = data,
#'                     pivot_cols = c("V1", "V2", "V3", "V4"),
#'                     theme = theme)
#' }
#' 
#' shiny::shinyApp(ui = ui, server = server)
#' 
#' }}
shinypivottabler <- function(input, output, session, 
                             data,
                             pivot_cols = NULL,
                             indicator_cols = NULL,
                             theme = NULL,
                             export_styles = TRUE) {
  
  ns <- session$ns
  
  observeEvent(input$combine, {
    if (! is.null(input$combine) && input$combine != "None") {
      shinyjs::addClass("id_padding_1", "combine_padding")
      shinyjs::addClass("id_padding_2", "combine_padding")
      shinyjs::addClass("id_padding_3", "combine_padding")
      shinyjs::addClass("id_padding_4", "combine_padding")
    } else {
      shinyjs::removeClass("id_padding_1", "combine_padding")
      shinyjs::removeClass("id_padding_2", "combine_padding")
      shinyjs::removeClass("id_padding_3", "combine_padding")
      shinyjs::removeClass("id_padding_4", "combine_padding")
    }
  })
  
  # reactive controls
  if (! shiny::is.reactive(data)) {
    get_data <- shiny::reactive(data)
  } else {
    get_data <- data
  }
  
  if (! shiny::is.reactive(pivot_cols)) {
    get_pivot_cols <- shiny::reactive(pivot_cols)
  } else {
    get_pivot_cols <- pivot_cols
  }
  
  if (! shiny::is.reactive(indicator_cols)) {
    get_indicator_cols <- shiny::reactive(indicator_cols)
  } else {
    get_indicator_cols <- indicator_cols
  }
  
  if (! shiny::is.reactive(theme)) {
    get_theme <- shiny::reactive(theme)
  } else {
    get_theme <- theme
  }
  
  if (! shiny::is.reactive(export_styles)) {
    get_export_styles <- shiny::reactive(export_styles)
  } else {
    get_export_styles <- export_styles
  }
  
  # update inputs
  observe({
    pivot_cols <- get_pivot_cols()
    
    isolate({
      if (is.null(pivot_cols)) {
        updateSelectInput(session = session, "rows",
                          choices = c("", names(get_data())),
                          selected = "")
        updateSelectInput(session = session, "cols",
                          choices = c("", names(get_data())),
                          selected = "")
      } else {
        updateSelectInput(session = session, "rows",
                          choices = c("", pivot_cols),
                          selected = "")
        updateSelectInput(session = session, "cols",
                          choices = c("", pivot_cols),
                          selected = "")
      }
    })
  })
  
  observe({
    indicator_cols <- get_indicator_cols()
    
    isolate({
      if (is.null(indicator_cols)) {
        updateSelectInput(session = session, "target",
                          choices = c("", names(which(sapply(get_data(), is.numeric)))),
                          selected = "")
      } else {
        updateSelectInput(session = session, "target",
                          choices = c("", indicator_cols),
                          selected = "")
      }
    })
  })  
  
  format <- reactiveVal("%.2f")
  observe({
    cpt <- input$specify_format
    
    isolate({
      if (! is.null(cpt) && cpt > 0) {
        showModal(
          modalDialog(
            title = "Format the cells",
            fluidRow(
              column(4,
                     numericInput(ns("format_digit"), label = "Nb. digits", 
                                  min = 0, max = Inf, value = 2, step = 1, width = "100%")
              ),
              column(4,
                     selectInput(ns("format_suffix"), label = "Suffix",
                                 choices = c("Aucun" = " ", "%" = " %%"), selected = "", width = "100%")
              )
            ),
            easyClose = FALSE,
            footer = div(style = "margin-right: 20px;", 
                         fluidRow(
                           column(3, 
                                  div(actionButton(inputId = ns("format_valid"), label = "Validate", width = "100%"), align = "left")
                           ),
                           column(3, offset = 6,
                                  div(actionButton(inputId = ns("format_cancel"), label = "Cancel", width = "100%"), align = "right")
                           ))
            ))
        )
      }
    })
  })
  observe({
    cpt_valid <- input$format_valid
    cpt_cancel <- input$format_cancel
    
    isolate({
      if (! is.null(cpt_valid) && ! is.null(cpt_cancel) && (cpt_valid > 0 || cpt_cancel > 0)) {
        if (cpt_valid > 0) {
          format(paste0("%.", input$format_digit, "f", input$format_suffix)) 
        }
        
        shiny::removeModal()
      }
    })
  })
  
  idcs <- reactiveVal()
  
  output$is_idcs <- reactive({
    ! is.null(idcs()) && length(idcs()) > 0
  })
  outputOptions(output, "is_idcs", suspendWhenHidden = FALSE)
  
  observe({
    cpt <- input$add_idc
    
    isolate({
      if (! is.null(cpt) && cpt > 0 && ! is.null(input$target) && input$target != "" && 
          (! is.null(input$combine) && input$combine == "None" || (! is.null(input$combine_target) && input$combine_target != ""))) {
        if (input$combine == "None") {
          label = ifelse(input$label %in% c("Auto", ""),
                         paste0(input$target, "_", input$idc),
                         input$label)
          
          idcs(c(idcs(), list(c("label" = label,
                                "target" = input$target, "idc" = input$idc, "format" = format()))))
        } else {
          label = ifelse(input$label %in% c("Auto", ""),
                         paste0(input$target, "_", input$idc, " ", input$combine, " ", input$combine_target, "_", input$combine_idc),
                         input$label)
          idcs(c(idcs(), list(c("label" = label,
                                "target" = input$target, "idc" = input$idc, "format" = format(),
                                "combine" = input$combine, "combine_target" = input$combine_target, "combine_idc" = input$combine_idc)))) 
        }
      }
    })
  })
  
  observe({
    indicators <- idcs()
    
    isolate({
      if (! is.null(indicators) && length(indicators) > 0) {
        lapply(1:length(indicators), function(index) {
          output[[paste0("idc_name_", index)]] <- renderText(indicators[[index]][["label"]])
        })
      }
    })
  })
  output$selected_indicators <- renderUI({
    indicators <-  idcs()
    
    isolate({
      if (! is.null(indicators) && length(indicators) > 0) {
        (lapply(1:length(indicators), function(index) {
          popup <- paste0("<b> Target : </b>", indicators[[index]][["target"]],
                          "<br> <b>Indicator : </b>", tolower(indicators[[index]][["idc"]]),
                          if (! "combine" %in% names(indicators[[index]])) {
                            ""
                          } else {
                            paste0("<br><b> Combine : </b>", indicators[[index]][["combine"]],
                                   "<br><b> Target 2 : </b>", indicators[[index]][["combine_target"]],
                                   "<br><b> Indicator 2 : </b>", tolower(indicators[[index]][["combine_idc"]]))
                          },
                          "<br><b> Format : </b>", indicators[[index]][["format"]])
          
          fluidRow(
            column(3,
                   div(checkboxInput(ns(paste0("idc_name_box_", index)), label = "", 
                                     value = T), style = "margin-top: -12px; margin-bottom: -10px;"),
            ),
            column(9,
                   div(textOutput(ns(paste0("idc_name_", index)), container = span), style = "margin-bottom: -10px; margin-left: -20%;")
            ),
            shinyBS::bsPopover(id = ns(paste0("idc_name_", index)),
                               title = paste0("<b>", indicators[[index]][["label"]], "</b>"),
                               content = popup,
                               placement = "bottom",
                               options = list(container = "body"))
          )
        }))
      }
    })
  })
  
  observe({
    input$add_idc
    
    isolate({
      updateTextInput(session = session, "label",
                      value = "Auto")
    })
  })
  
  observe({
    cpt <- input$reset_table
    
    isolate({
      if(! is.null(cpt) && cpt > 0) {
        idcs(list())
      }
    })
  })
  
  store_pt <- reactiveVal(NULL)
  output$pivottable <- renderPivottabler({
    cpt <- input$go_table
    input$reset_table
    
    isolate({
      idcs <- idcs()
      data <- get_data()
      
      if (! is.null(cpt) && cpt > 0 && ! is.null(idcs) && length(idcs) > 0 && ! is.null(data)) {
        shiny::withProgress(message = 'Creation of the table', value = 0.5, {
          
          input$reset_table
          pt <- PivotTable$new()
          pt$addData(data)
          
          # rows and columns
          for (col in input$cols) {
            if (col != "") {pt$addColumnDataGroups(col)}
          }
          
          for (row in input$rows) {
            if (row != "") {pt$addRowDataGroups(row)}
          }
          
          for (index in 1:length(idcs)) {
            if (input[[paste0("idc_name_box_", index)]]) {
              
              label <- idcs[[index]][["label"]]
              target <- gsub(" ", "_", idcs[[index]]["target"])
              idc <- gsub(" ", "_", idcs[[index]][["idc"]])
              format <- idcs[[index]][["format"]]
              
              combine <- if ("combine" %in% names(idcs[[index]])) {gsub(" ", "_", idcs[[index]]["combine"])} else {NULL} 
              combine_target <- if ("combine_target" %in% names(idcs[[index]])) {gsub(" ", "_", idcs[[index]]["combine_target"])} else {NULL} 
              combine_idc <- if ("combine_target" %in% names(idcs[[index]])) {gsub(" ", "_", idcs[[index]][["combine_idc"]])} else {NULL} 
              
              pt$defineCalculation(calculationName = paste0(target, "_", tolower(idc), "_", index),
                                   caption = label,
                                   summariseExpression = get_expr(idc, target),
                                   format = ifelse(format == "None", format, "%.2f"),
                                   visible = ifelse(is.null(combine_target), T, F))
              
              if (! is.null(combine_target) && combine_target != "") {
                pt$defineCalculation(calculationName = paste0(combine_target, "_", tolower(combine_idc), "_combine_", index),
                                     summariseExpression = get_expr(combine_idc, combine_target),
                                     format = "%.2f",
                                     visible = FALSE)
                pt$defineCalculation(calculationName = paste0(combine_target, "_", tolower(combine_idc), combine, combine_target, "_", tolower(combine_idc), "_combine_", index),
                                     caption = label,
                                     basedOn = c(paste0(target, "_", tolower(idc), "_", index), paste0(combine_target, "_", tolower(combine_idc), "_combine_", index)),
                                     type = "calculation",
                                     calculationExpression = paste0("values$", paste0(target, "_", tolower(idc), "_", index), combine, "values$", paste0(combine_target, "_", tolower(combine_idc), "_combine_", index)),
                                     format = format)
              } 
            }
          }
          
          if (is.null(get_theme())) {
            theme <- list(
              fontName="Courier New, Courier",
              fontSize="1.5em",
              headerBackgroundColor = "#217346",
              headerColor = "rgb(255, 255, 255)",
              cellBackgroundColor = "rgb(255, 255, 255)",
              cellColor = "rgb(0, 0, 0)",
              outlineCellBackgroundColor = "rgb(192, 192, 192)",
              outlineCellColor = "rgb(0, 0, 0)",
              totalBackgroundColor = "#59bb28",
              totalColor = "rgb(0, 0, 0)",
              borderColor = "rgb(64, 64, 64)")
          } else {
            theme <- get_theme()
          }
          
          pt$theme <- theme
          
          pt$evaluatePivot()
          store_pt(pt)
          pivottabler(pt, width = "100%", height = "100%")
        })
      }
    })
  })
  
  output$is_pivottable <- reactive({
    ! is.null(idcs()) && length(idcs()) > 0 && ! is.null(input$go_table) && input$go_table > 0
  })
  outputOptions(output, "is_pivottable", suspendWhenHidden = FALSE)
  
  get_wb <- reactive({
    pt <- store_pt()
    
    isolate({
      if (! is.null(pt)) {
        wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
        addWorksheet(wb, "Pivot table")
        
        pt$writeToExcelWorksheet(wb = wb, wsName = "Pivot table",
                                 topRowNumber = 1, leftMostColumnNumber = 1,
                                 outputValuesAs = "rawValue",
                                 applyStyles = get_export_styles(), mapStylesFromCSS = TRUE)
        wb
      }
    })
  })
  
  output$export <- downloadHandler(
    filename = function() {
      paste0("pivot_table_", base::format(Sys.time(), format = "%Y%m%d_%H%M%S") ,".xlsx")
    },
    content = function(file) {
      saveWorkbook(get_wb(), file=file, overwrite = TRUE)
    }
  )
}



#' @import pivottabler shiny shinyjs
#' 
#' @export
#'
#' @rdname shiny_pivot_table
#' 
shinypivottablerUI <- function(id) {
  ns <- shiny::NS(id)
  
  fluidPage(
    shinyjs::useShinyjs(), 
    tags$head(
      tags$style(HTML("
        div.combine_padding { padding-top: 35px; }
        "))),
    
    
    div(h2(HTML("<b>Shiny pivot table</b>")), style = "color: #217346;, margin-left: 15px;"),
    
    br(),
    
    # tags
    tags$head(
      tags$style(HTML("
      .Table {
          display: table;
          border-collapse: collapse;
          margin-left: auto;
          margin-right: auto;
      }
    "))
    ),
    tags$head(
      tags$style(HTML("
        ul {
          list-style: none; /* Remove default bullets */
        }
    
        ul li::before {
          content: '\\2022'; 
          color: #59bb28; 
          font-weight: bold; 
          display: inline-block; 
          width: 1em; 
          margin-left: -1em; 
          font-size: 20px;
        }
      "))
    ),
    
    tags$head(
      tags$style("
        .btn { width: 100%; }
      ")
    ),
    
    fluidRow(style = "padding-left: 1%; padding-right: 1%;",
             column(12, style = "border-radius: 5px; border-top: 10px solid #59bb28; border-bottom: 10px solid #217346; border-left: 10px solid #59bb28; border-right: 10px solid #217346;", 
                    h3(HTML("<b>Definition of the pivot table</b>")),
                    
                    br(),
                    
                    column(2, 
                           fluidRow(
                             div(h4(HTML("<b>Selected indicators</b>")), align = "center", style = "padding-right: 12px;"),
                             
                             conditionalPanel(condition = paste0("output['", ns("is_idcs"), "']"),
                                              div(uiOutput(ns("selected_indicators")), style = "margin-top: 15px; overflow-y: auto; height: 130px; overflow-x: hidden; margin-right: 10px;")                
                             ),
                             conditionalPanel(condition = paste0("! output['", ns("is_idcs"), "']"),
                                              div(h3("None"), align = "center", style = "padding-top: 20px; padding-right: 12px; color: #217346;")
                             )
                           )
                    ),
                    
                    column(10, style = "margin-bottom: 15px; border-left: 2px solid #59bb28;", 
                           fluidRow(style = "margin-left: 0px; margin-bottom: -15px;",
                                    fluidRow(
                                      column(3,
                                             selectInput(ns("rows"), label = "Selected rows",
                                                         choices = NULL, multiple = T, width = "100%")
                                      ),
                                      column(3,
                                             selectInput(ns("cols"), label = "Selected columns",
                                                         choices = NULL, multiple = T, width = "100%")
                                      )
                                    ),
                                    
                                    div(hr(style = "border: 1px solid #59bb28;"), style = "margin-top: -10px;"),
                                    
                                    fluidRow(
                                      column(1, 
                                             div(id = "id_padding_1", textInput(ns("label"), label = "Label", value = "Auto", width  = "100%"))
                                      ),
                                      column(4,
                                             fluidRow(
                                               column(6,
                                                      selectInput(ns("target"), label = "Selected target",
                                                                  choices = NULL, width = "100%")
                                               ),
                                               column(6,
                                                      selectInput(ns("idc"), label = "Selected indicator",
                                                                  choices = c("Count",
                                                                              "Count distinct",
                                                                              "Sum",
                                                                              "Mean",
                                                                              "Min",
                                                                              "Max",
                                                                              "Standard deviation"),
                                                                  selected = "Count", width = "100%")
                                               )
                                             ),
                                             conditionalPanel(condition = paste0("input['", ns("combine"), "'] !== 'None'"),
                                                              fluidRow(
                                                                column(6,
                                                                       selectInput(ns("combine_target"), label = "Selected target",
                                                                                   choices = c("", "V5", "V6"),
                                                                                   selected = "", width = "100%")
                                                                ),
                                                                column(6,
                                                                       selectInput(ns("combine_idc"), label = "Selected indicator",
                                                                                   choices = c("Count",
                                                                                               "Count distinct",
                                                                                               "Sum",
                                                                                               "Mean",
                                                                                               "Min",
                                                                                               "Max",
                                                                                               "Standard deviation"),
                                                                                   selected = "Count", width = "100%")
                                                                )
                                                              )            
                                             )
                                      ),
                                      column(1, 
                                             div(id = "id_padding_2", selectInput(ns("combine"), label = "Combine", 
                                                                                  choices = c("None" = "None",
                                                                                              "Add" = "+",
                                                                                              "Substract" = "-",
                                                                                              "Multiply" = "*",
                                                                                              "Divise" = "/"),
                                                                                  selected = "No", width = "100%"))
                                      ),
                                      column(2, 
                                             div(id = "id_padding_3", actionButton(ns("specify_format"), label = "Specify format", width = "100%"), style = "margin-top: 25px")
                                      ),
                                      column(4, 
                                             div(id = "id_padding_4", actionButton(ns("add_idc"), label = "Add indicator", width = "75%"), align = "center", style = "margin-top: 25px")
                                      ),
                                    ) 
                           )
                    )
             )
    ),
    
    br(),
    br(),
    
    fluidRow(style = "padding-left: 1%; padding-right: 1%;",
             column(12, style = "padding: 2.5%; overflow-x: auto; overflow-y: auto; border-radius: 5px; border-top: 10px solid #59bb28; border-bottom: 10px solid #217346; border-left: 10px solid #59bb28; border-right: 10px solid #217346;", 
                    fluidRow(
                      column(4, offset = 2,
                             div(actionButton(ns("go_table"), label = "Display table", width = "100%" ), align = "right")
                      ),
                      column(4, 
                             div(actionButton(ns("reset_table"), label = "Reset table", width = "100%"), align = "left")
                      )
                    ),
                    
                    br(),
                    
                    conditionalPanel(condition = paste0("output['", ns("is_pivottable"), "']"), 
                                     div(pivottablerOutput(ns('pivottable'), width = "100%", height = "100%"), style = "padding-top: 1.5%;"),
                                     br(),
                                     column(6, offset = 3,
                                            div(downloadButton(ns("export"), label = "Download table"), align = "center", style = "width: 100%;")
                                     )
                    ),
                    conditionalPanel(condition = paste0("! output['", ns("is_pivottable"), "']"), 
                                     div(h3("No data to display"), align = "center"), style = "color: #217346;")
             )
    )
  ) 
}