###############################
###  PIVOT TABLE FOR SHINY  ###
###############################



get_expr <- function(idc, target, additional_expr) {

  text_idc <- c(
    list(
      "Count" = "'n()'",
      "Count_distinct" = "paste0('n_distinct(', target, ', na.rm = TRUE)')",
      "Sum" = "paste0('sum(as.numeric(', target, '), na.rm = TRUE)')",
      "Mean" = "paste0('mean(as.numeric(', target, '), na.rm = TRUE)')",
      "Min" = "paste0('min(as.numeric(', target, '), na.rm = TRUE)')",
      "Max" = "paste0('max(as.numeric(', target, '), na.rm = TRUE)')",
      "Standard_deviation" = "paste0('sd(as.numeric(', target, '), na.rm = TRUE)')"
    ),
    additional_expr)

  return(eval(parse(text = text_idc[[idc]])))
}



#' Shiny module to render and export pivot tables.
#'
#' @param input shiny input
#' @param output shiny input
#' @param session shiny input
#' @param data \code{data.frame} / \code{data.table}. Initial data table.
#' @param pivot_cols \code{character} (NULL). Columns to be used as pivot in rows and cols.
#' @param max_n_pivot_cols \code{numeric} (100). Maximum unique values for a \code{pivot_cols} if pivot_cols = NULL
#' @param indicator_cols \code{character} (NULL). Columns on which indicators will be calculated.
#' @param additional_expr_num \code{named list} (list()). Additional computations to be allowed for quantitative vars.
#' @param additional_expr_char \code{named list} (list()). Additional computations to be allowed for qualitative vars.
#' @param additional_combine \code{named list} (list()). Additional combinations to be allowed.
#' @param theme \code{list} (NULL). Theme to customize the output of the pivot table.
#' @param export_styles \code{boolean} (TRUE). Whether or not to apply styles (like the theme) when exporting to Excel.
#' @param show_title \code{boolean} (TRUE). Whether or not to display the app title.
#' Some styles may not be supported by Excel.
#' @param initialization  \code{named list} (NULL). Initialization parameters to display a table when launching the module.
#' Available fields are :
#'\itemize{
#'  \item{\code{rows:}}{ Selected pivot rows.}
#'  \item{\code{cols:}}{ Selected pivot columns.}
#'  \item{\code{target, combine target:} { Selected target and combine_target columns.}.
#'  \item{\code{idc, combine_idc:}}{ Selected idc and combine_idc columns.}
#'  \item{\code{combine:}}{ Selected combine operator.}
#'  \item{\code{format_digit, format_prefix, format_suffix, format_sep_thousands, format_decimal:}}{ Selected formats for the table idc.}
#'  \item{\code{idcs:}}{ idcs to be displayed (list of named list), see the example to get the fields.}
#'}
#'
#' @return Nothing. Starts a Shiny module.
#'
#' @import pivottabler shiny openxlsx
#' @importFrom colourpicker colourInput
#'
#' @export
#' @rdname shiny_pivot_table
#'
#' @examples
#' \dontrun{\donttest{
#'
#' require(shinypivottabler)
#' require(shiny)
#'
#' # create artificial dataset
#' data <- data.frame("V1" = sample(c("A", "B", "C", "D"), size = 1000000,
#'                                  prob = rep(1, 4), replace = T),
#'                    "V2" = sample(c("E", "F", "G", "H"), size = 1000000,
#'                                  prob = rep(1, 4), replace = T),
#'                    "V3" = sample(c("I", "J", "K", "L"), size = 1000000,
#'                                  prob = rep(1, 4), replace = T),
#'                    "V4" = sample(c("M", "N", "O", "P"), size = 1000000,
#'                                  prob = rep(1, 4), replace = T),
#'                    "V5" = 1:1000000,
#'                    "V6" = 1000000:1)
#'
#' # Minimal example
#'
#' ui = shiny::fluidPage(
#'   shinypivottablerUI(id = "id")
#' )
#'
#' server = function(input, output, session) {
#'   shiny::callModule(module = shinypivottabler,
#'                     id = "id",
#'                     data = data)
#' }
#'
#' shiny::shinyApp(ui = ui, server = server)
#'
#'
#'
#' # Complete example
#'
#' initialization <- list(
#'   "rows" = "V1",
#'   "cols" = "V2",
#'   "target" = "V3",
#'   "combine_target" = "V4",
#'   "idc" = "Count",
#'   "combine_idc" = "Count",
#'   "combine" = "/",
#'   "idcs" = c(list(c("label" = "Init_variable_1",
#'                   "target" = "V3", "idc" = "Count",
#'                   "nb_decimals" = 0,
#'                   "sep_thousands" = " ",
#'                   "sep_decimal" = ".",
#'                   "prefix" = "",
#'                   "suffix" = "",
#'                   "combine" = "/", "combine_target" = "V4", "combine_idc" = "Count")),
#'                list(c("label" = "Init_variable_2",
#'                   "target" = "V3", "idc" = "Count",
#'                   "nb_decimals" = 0,
#'                   "sep_thousands" = " ",
#'                   "sep_decimal" = ".",
#'                   "prefix" = "",
#'                   "suffix" = "")))
#' )
#'
#' theme <- list(
#'   fontName="Courier New, Courier",
#'   fontSize="1em",
#'   headerBackgroundColor = "red",
#'   headerColor = "rgb(255, 255, 255)",
#'   cellBackgroundColor = "rgb(255, 255, 255)",
#'   cellColor = "rgb(0, 0, 0)",
#'   outlineCellBackgroundColor = "rgb(192, 192, 192)",
#'   outlineCellColor = "rgb(0, 0, 0)",
#'   totalBackgroundColor = "#59bb28",
#'   totalColor = "rgb(0, 0, 0)",
#'   borderColor = "rgb(64, 64, 64)"
#' )
#'
#' ui = shiny::fluidPage(
#'   shinypivottablerUI(id = "id")
#' )
#'
#' # we add two functions, one for quantitative variables (the median) and
#' # one for qualitatives variables (the mode, with a custom function), and
#' # one possible combination (the modulo).
#' my_mode <- function(x) names(which.max(table(x)))
#'
#' server = function(input, output, session) {
#'   shiny::callModule(module = shinypivottabler,
#'                     id = "id",
#'                     data = data,
#'                     pivot_cols = c("V1", "V2", "V3", "V4"),
#'                     additional_expr_num = list(
#'                       "Add_median" = "paste0('median(as.numeric(', target, '), na.rm = TRUE)')"
#'                     ),
#'                     additional_expr_char = list(
#'                       "Add_mode" = "paste0('my_mode(', target, ')')"
#'                     ),
#'                     additional_combine = c("Add_modulo" = "%%"),
#'                     theme = theme,
#'                     initialization = initialization)
#' }
#'
#' shiny::shinyApp(ui = ui, server = server)
#'
#' }}
shinypivottabler <- function(input, output, session,
                             data,
                             pivot_cols = NULL,
                             indicator_cols = NULL,
                             max_n_pivot_cols = 100,
                             additional_expr_num = list(),
                             additional_expr_char = list(),
                             additional_combine = list(),
                             theme = NULL,
                             export_styles = TRUE,
                             show_title = TRUE,
                             initialization = NULL) {

  ns <- session$ns

  observe({
    if (! is.null(idcs()) && length(idcs()) > 0) {
      toggleBtnSPivot(session = session, inputId = ns("go_table"), type = "enable")
    } else {
      toggleBtnSPivot(session = session, inputId = ns("go_table"), type = "disable")
    }
  })
  observe({
    if (! is.null(input$combine) && input$combine != "None") {
      combine_padding(session = session, inputId = ns("id_padding_1"), type = "combine")
      combine_padding(session = session, inputId = ns("id_padding_2"), type = "combine")
      combine_padding(session = session, inputId = ns("id_padding_3"), type = "combine")
      combine_padding(session = session, inputId = ns("id_padding_4"), type = "combine")
    } else {
      combine_padding(session = session, inputId = ns("id_padding_1"), type = "regular")
      combine_padding(session = session, inputId = ns("id_padding_2"), type = "regular")
      combine_padding(session = session, inputId = ns("id_padding_3"), type = "regular")
      combine_padding(session = session, inputId = ns("id_padding_4"), type = "regular")
    }
  })

  # reactive controls
  if (! shiny::is.reactive(data)) {
    get_data <- shiny::reactive(data)
  } else {
    get_data <- data
  }

  have_data <- reactive({
    data <- get_data()
    ! is.null(data) && any(c("data.frame", "tbl", "tbl_df", "data.table") %in% class(data)) && nrow(data) > 0
  })
  output$ui_have_data <- reactive({
    have_data()
  })
  outputOptions(output, "ui_have_data", suspendWhenHidden = FALSE)

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

  if (! shiny::is.reactive(max_n_pivot_cols)) {
    get_max_n_pivot_cols <- shiny::reactive(max_n_pivot_cols)
  } else {
    get_max_n_pivot_cols <- max_n_pivot_cols
  }

  get_theme <- reactiveVal(NULL)
  observe({
    if (! shiny::is.reactive(theme)) {
      if (is.null(theme)) {
        get_theme(list(
          fontName="Courier New, Courier",
          fontSize="1.2em",
          headerBackgroundColor = "#217346",
          headerColor = "rgb(255, 255, 255)",
          cellBackgroundColor = "rgb(255, 255, 255)",
          cellColor = "rgb(0, 0, 0)",
          outlineCellBackgroundColor = "rgb(192, 192, 192)",
          outlineCellColor = "rgb(0, 0, 0)",
          totalBackgroundColor = "#59bb28",
          totalColor = "rgb(0, 0, 0)",
          borderColor = "rgb(64, 64, 64)"))
      } else {
        get_theme(theme)
      }
    } else {
      if (is.null(theme())) {
        get_theme(list(
          fontName="Courier New, Courier",
          fontSize="1.2em",
          headerBackgroundColor = "#217346",
          headerColor = "rgb(255, 255, 255)",
          cellBackgroundColor = "rgb(255, 255, 255)",
          cellColor = "rgb(0, 0, 0)",
          outlineCellBackgroundColor = "rgb(192, 192, 192)",
          outlineCellColor = "rgb(0, 0, 0)",
          totalBackgroundColor = "#59bb28",
          totalColor = "rgb(0, 0, 0)",
          borderColor = "rgb(64, 64, 64)"))
      } else {
        get_theme(theme())
      }
    }
  })

  if (! shiny::is.reactive(export_styles)) {
    get_export_styles <- shiny::reactive(export_styles)
  } else {
    get_export_styles <- export_styles
  }

  if (! shiny::is.reactive(show_title)) {
    get_show_title <- shiny::reactive(show_title)
  } else {
    get_show_title <- show_title
  }

  if (! shiny::is.reactive(additional_expr_num)) {
    get_additional_expr_num <- shiny::reactive(additional_expr_num)
  } else {
    get_additional_expr_num <- additional_expr_num
  }

  if (! shiny::is.reactive(additional_expr_char)) {
    get_additional_expr_char <- shiny::reactive(additional_expr_char)
  } else {
    get_additional_expr_char <- additional_expr_char
  }

  if (! shiny::is.reactive(additional_combine)) {
    get_additional_combine <- shiny::reactive(additional_combine)
  } else {
    get_additional_combine <- additional_combine
  }

  if (! shiny::is.reactive(initialization)) {
    trigger_initialization <- shiny::reactive(initialization)
  } else {
    trigger_initialization <- shiny::reactive(initialization())
  }

  get_initialization <- reactiveVal(NULL)
  observe({
    if (! shiny::is.reactive(initialization)) {
      get_initialization(initialization)
    } else {
      get_initialization(initialization())
    }
  })

  output$show_title <- reactive({
    get_show_title()
  })
  outputOptions(output, "show_title", suspendWhenHidden = FALSE)

  # update inputs
  observe({
    data <- get_data()
    trigger_initialization()

    isolate({
      pivot_cols <- get_pivot_cols()
      initialization <- get_initialization()

      if (is.null(pivot_cols)) {
        ctrl_col <- sapply(data, function(x) length(unique(x)) <= get_max_n_pivot_cols())

        choix <- names(ctrl_col)[ctrl_col]

        updateSelectInput(session = session, "rows",
                          choices = c("", choix),
                          selected = if (is.null(initialization$rows)) {""} else {initialization$rows})
        updateSelectInput(session = session, "cols",
                          choices = c("", choix),
                          selected = if (is.null(initialization$cols)) {""} else {initialization$cols})
      } else {
        updateSelectInput(session = session, "rows",
                          choices = c("", pivot_cols),
                          selected = if (is.null(initialization$rows)) {""} else {initialization$rows})
        updateSelectInput(session = session, "cols",
                          choices = c("", pivot_cols),
                          selected = if (is.null(initialization$cols)) {""} else {initialization$cols})
      }
    })
  })

  observe({
    data <- get_data()
    trigger_initialization()

    isolate({
      indicator_cols <- get_indicator_cols()
      initialization <- get_initialization()

      if (is.null(indicator_cols) && have_data()) {
        updateSelectInput(session = session, "target",
                          choices = c("", names(which(sapply(data, function(x) any(class(x) %in% c("logical", "numeric", "integer", "character", "factor")))))),
                          selected = if (is.null(initialization$target)) {""} else {initialization$target})
      } else {
        updateSelectInput(session = session, "target",
                          choices = c("", indicator_cols),
                          selected = if (is.null(initialization$target)) {""} else {initialization$target})
      }
    })
  })

  observe({
    target <- input$target
    trigger_initialization()

    isolate({
      initialization <- get_initialization()

      if (is.null(get_data()[[target]]) || is.numeric(get_data()[[target]])) {
        choices <- c(
          c("Count", "Count distinct", "Sum", "Mean", "Min", "Max", "Standard deviation"),
          names(get_additional_expr_num())
        )
        updateSelectInput(session = session, "idc",
                          choices = choices,
                          selected = if (is.null(initialization$idc)) {ifelse(input$idc %in% choices, input$idc, "Count")} else {initialization$idc})
      } else if (is.character(get_data()[[target]]) || is.factor(get_data()[[target]])) {
        choices <- c(
          c("Count", "Count distinct"),
          names(get_additional_expr_char())
        )
        updateSelectInput(session = session, "idc",
                          choices = choices,
                          selected = if (is.null(initialization$idc)) {ifelse(input$idc %in% choices, input$idc, "Count")} else {initialization$idc})
      }
    })
  })
  observe({
    data <- get_data()
    combine <- input$combine
    trigger_initialization()

    isolate({
      indicator_cols <- get_indicator_cols()
      initialization <- get_initialization()

      if (! is.null(combine) && combine != "None") {
        if (is.null(input$combine_target) || input$combine_target == "") {
          if (is.null(indicator_cols) && have_data()) {
            updateSelectInput(session = session, "combine_target",
                              choices = c("", names(which(sapply(data, function(x) any(class(x) %in% c("logical", "numeric", "integer", "character", "factor")))))),
                              selected = if (is.null(initialization$combine_target)) {""} else {initialization$combine_target})
          } else {
            updateSelectInput(session = session, "combine_target",
                              choices = c("", indicator_cols),
                              selected = if (is.null(initialization$combine_target)) {""} else {initialization$combine_target})
          }
        }
      }
    })
  })
  observe({
    combine_target <- input$combine_target
    input$combine
    trigger_initialization()

    isolate({
      initialization <- get_initialization()

      if (is.null(combine_target) || is.null(get_data()[[combine_target]]) || is.numeric(get_data()[[combine_target]])) {
        choices <- c(
          c("Count", "Count distinct", "Sum", "Mean", "Min", "Max", "Standard deviation"),
          names(get_additional_expr_num())
        )
        updateSelectInput(session = session, "combine_idc",
                          choices = choices,
                          selected = if (is.null(initialization$combine_idc)) {ifelse(input$combine_idc %in% choices, input$combine_idc, "Count")} else {initialization$combine_idc})
      } else if (is.character(get_data()[[combine_target]]) || is.factor(get_data()[[combine_target]])) {
        choices <- c(
          c("Count", "Count distinct"),
          names(get_additional_expr_char())
        )
        updateSelectInput(session = session, "combine_idc",
                          choices = choices,
                          selected = if (is.null(initialization$combine_idc)) {ifelse(input$combine_idc %in% choices, input$combine_idc, "Count")} else {initialization$combine_idc})
      }
    })
  })

  observe({
    trigger_initialization()

    isolate({
      initialization <- get_initialization()

      updateSelectInput(session = session, "combine",
                        choices = c(c("None" = "None",
                                      "Add" = "+",
                                      "Substract" = "-",
                                      "Multiply" = "*",
                                      "Divise" = "/"),
                                    get_additional_combine()),
                        selected = if (is.null(initialization$combine)) {"None"} else {initialization$combine})
    })
  })

  store_format <- reactiveValues("format_digit" = 1,
                                 "format_prefix" = "",
                                 "format_suffix" = "",
                                 "format_sep_thousands" = " ",
                                 "format_decimal" = ",")

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
                                  min = 0, max = Inf, value = store_format[["format_digit"]], step = 1, width = "100%")
              ),
              column(4,
                     textInput(ns("format_prefix"), label = "Prefix (excel only)",
                               value = store_format[["format_prefix"]], width = "100%")
              ),
              column(4,
                     textInput(ns("format_suffix"), label = "Suffix (excel only)",
                               value = store_format[["format_suffix"]], width = "100%")
              )
            ),
            fluidRow(
              column(4,
                     selectInput(ns("format_sep_thousands"), label = "Thousands sep.",
                                 choices = c("None", "Space" = " ", ","), selected = store_format[["format_sep_thousands"]], width = "100%")
              ),
              column(4,
                     selectInput(ns("format_sep_decimals"), label = "Decimal sep.",
                                 choices = c(".", ","), selected = store_format[["format_decimal"]], width = "100%")
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
      if (! is.null(cpt_valid) && cpt_valid > 0) {
        store_format$format_digit <- input$format_digit
        store_format$format_prefix <- input$format_prefix
        store_format$format_suffix <- input$format_suffix
        store_format$format_sep_thousands <- input$format_sep_thousands
        store_format$format_decimal <- input$format_sep_decimals

        shiny::removeModal()
      }
      if (! is.null(cpt_cancel) && cpt_cancel > 0) {
        updateNumericInput(session = session, "format_digit",
                           value = store_format[["format_digit"]])
        updateTextInput(session = session, "format_prefix",
                        value = store_format[["format_digit"]])
        updateTextInput(session = session, "format_suffix",
                        value = store_format[["format_suffix"]])
        updateSelectInput(session = session, "format_sep_thousands",
                          selected = store_format[["format_sep_thousands"]])
        updateSelectInput(session = session, "format_sep_decimals",
                          selected = store_format[["format_decimal"]])

        shiny::removeModal()
      }
    })
  })

  idcs <- reactiveVal()

  output$is_idcs <- reactive({
    ! is.null(idcs()) && length(idcs()) > 0
  })
  outputOptions(output, "is_idcs", suspendWhenHidden = FALSE)

  observeEvent(input$reset_table || is.null(get_data()), {

    isolate({
        idcs(list())
        store_pt(NULL)
    })
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

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
                                "target" = input$target, "idc" = input$idc,
                                "nb_decimals" = ifelse(input$idc %in% c("Count", "Count distinct"), 0,
                                                       ifelse(!is.null(input$format_digit), input$format_digit, store_format$format_digit)),
                                "sep_thousands" = ifelse(!is.null(input$format_sep_thousands), input$format_sep_thousands, store_format$format_sep_thousands),
                                "sep_decimal" = ifelse(!is.null(input$format_sep_decimals), input$format_sep_decimals, store_format$format_decimal),
                                "prefix" = ifelse(!is.null(input$format_prefix), input$format_prefix, store_format$format_prefix),
                                "suffix" = ifelse(!is.null(input$format_suffix), input$format_suffix, store_format$format_suffix)))))
        } else {
          label = ifelse(input$label %in% c("Auto", ""),
                         paste0(input$target, "_", input$idc, " ", input$combine, " ", input$combine_target, "_", input$combine_idc),
                         input$label)
          idcs(c(idcs(), list(c("label" = label,
                                "target" = input$target, "idc" = input$idc,
                                "nb_decimals" = ifelse(input$idc %in% c("Count", "Count distinct"), 0,
                                                       ifelse(!is.null(input$format_digit), input$format_digit, store_format$format_digit)),
                                "sep_thousands" = ifelse(!is.null(input$format_sep_thousands), input$format_sep_thousands, store_format$format_sep_thousands),
                                "sep_decimal" = ifelse(!is.null(input$format_sep_decimals), input$format_sep_decimals, store_format$format_decimal),
                                "prefix" = ifelse(!is.null(input$format_prefix), input$format_prefix, store_format$format_prefix),
                                "suffix" = ifelse(!is.null(input$format_suffix), input$format_suffix, store_format$format_suffix),
                                "combine" = input$combine, "combine_target" = input$combine_target, "combine_idc" = input$combine_idc))))
        }
      }
    })
  })

  observe({
    trigger_initialization()

    isolate({
      initialization <- get_initialization()

      if (! is.null(initialization)) {
        # update format defaults
        store_format$format_digit <- ifelse(is.null(initialization$format_digit), store_format$format_digit, initialization$format_digit)
        store_format$format_prefix <- ifelse(is.null(initialization$format_prefix), store_format$format_prefix, initialization$format_prefix)
        store_format$format_suffix <- ifelse(is.null(initialization$format_suffix), store_format$format_suffix, initialization$format_suffix)
        store_format$format_sep_thousands <- ifelse(is.null(initialization$format_sep_thousands), store_format$format_sep_thousands, initialization$format_sep_thousands)
        store_format$format_decimal <- ifelse(is.null(initialization$format_decimal), store_format$format_decimal, initialization$format_decimal)

        # update idcs
        if (! is.null(initialization$idcs)) {
          idcs(initialization$idcs)
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
    indicators <- idcs()

    isolate({
      if (! is.null(indicators) && length(indicators) > 0) {

        lapply(1:length(indicators), function(index) {
          popup <- paste0("<b> Target : </b>", indicators[[index]][["target"]],
                          "<br><b> Indicator : </b>", tolower(indicators[[index]][["idc"]]),
                          if (! "combine" %in% names(indicators[[index]])) {
                            ""
                          } else {
                            paste0("<br><b> Combine : </b>", indicators[[index]][["combine"]],
                                   "<br><b> Target 2 : </b>", indicators[[index]][["combine_target"]],
                                   "<br><b> Indicator 2 : </b>", tolower(indicators[[index]][["combine_idc"]]))
                          },
                          "<br><b> Nb. decimal : </b>", ifelse("nb_decimals" %in% names(indicators[[index]]), indicators[[index]][["nb_decimals"]], 2),
                          "<br><b> Decimal sep : </b>", ifelse("sep_decimal" %in% names(indicators[[index]]), indicators[[index]][["sep_decimal"]], ","),
                          "<br><b> Thousands sep : </b>", ifelse("sep_thousands" %in% names(indicators[[index]]), indicators[[index]][["sep_thousands"]], " "),
                          "<br><b> Prefix sep : </b>", ifelse("prefix" %in% names(indicators[[index]]), indicators[[index]][["prefix"]], ""),
                          "<br><b> Suffix sep : </b>", ifelse("suffix" %in% names(indicators[[index]]), indicators[[index]][["suffix"]], ""))

          fluidRow(
            column(3,
                   div(checkboxInput(ns(paste0("idc_name_box_", index)), label = "",
                                     value = if (length(idcs()) < index + 1) {T} else {! is.null(get_initialization()) || input[[paste0("idc_name_box_", index)]]}), style = "margin-top: -12px; margin-bottom: -10px; margin-left: 2px;")
            ),
            column(9,
                   div(textOutput(ns(paste0("idc_name_", index)), container = span), style = "margin-bottom: -10px; margin-left: -20%;")
            ),
            extract_bsPopover(id = ns(paste0("idc_name_", index)),
                              title = paste0("<b>", indicators[[index]][["label"]], "</b>"),
                              content = popup,
                              placement = "bottom",
                              options = list(container = "body"))
          )
        })
      }
    })
  })

  observe({
    input$add_idc

    isolate({
      updateTextInput(session = session, "label", value = "Auto")
    })
  })

  store_pt <- reactiveVal(NULL)
  observe({
    cpt <- input$go_table
    trigger_initialization()

    isolate({
      idcs <- isolate(idcs())
      data <- isolate(get_data())
      initialization <- get_initialization()

      if (! is.null(data) && (((! is.null(cpt) && cpt > 0 && ! is.null(idcs)) || ! is.null(initialization)) && length(idcs) > 0)) {
        shiny::withProgress(message = 'Creating the table...', value = 0.5, {

          pt <- pivottabler::PivotTable$new()
          pt$addData(data)

          # rows and columns
          cols <- if (is.null(initialization$cols)) {input$cols} else {initialization$cols}
          for (col in cols) {
            if (! is.null(col) && col != "") {pt$addColumnDataGroups(col)}
          }
          rows <- if (is.null(initialization$rows)) {input$rows} else {initialization$rows}
          for (row in rows) {
            if (! is.null(row) && row != "") {pt$addRowDataGroups(row)}
          }

          for (index in 1:length(idcs)) {
            is_checked <- input[[paste0("idc_name_box_", index)]]

            if ((! is.null(is_checked) && is_checked) || ! is.null(initialization)) {
              label <- idcs[[index]][["label"]]
              target <- gsub(" ", "_", idcs[[index]]["target"])
              idc <- gsub(" ", "_", idcs[[index]][["idc"]])
              nb_decimals <- ifelse(is.na(idcs[[index]]["nb_decimals"]), 1, idcs[[index]]["nb_decimals"])
              sep_thousands <- ifelse(is.na(idcs[[index]]["sep_thousands"]), " ", idcs[[index]]["sep_thousands"])
              sep_decimal <- ifelse(is.na(idcs[[index]]["sep_decimal"]), ".", idcs[[index]]["sep_decimal"])
              prefix <- ifelse(is.na(idcs[[index]]["prefix"]), "", idcs[[index]]["prefix"])
              suffix <- ifelse(is.na(idcs[[index]]["suffix"]), "", idcs[[index]]["suffix"])

              combine <- if ("combine" %in% names(idcs[[index]])) {gsub(" ", "_", idcs[[index]]["combine"])} else {NULL}
              combine_target <- if ("combine_target" %in% names(idcs[[index]])) {gsub(" ", "_", idcs[[index]]["combine_target"])} else {NULL}
              combine_idc <- if ("combine_target" %in% names(idcs[[index]])) {gsub(" ", "_", idcs[[index]][["combine_idc"]])} else {NULL}

              pt$defineCalculation(calculationName = paste0(target, "_", tolower(idc), "_", index),
                                   caption = label,
                                   summariseExpression = get_expr(idc, target, additional_expr = c(get_additional_expr_num(), get_additional_expr_char())),
                                   format = list("digits" = nb_decimals, "nsmall" = nb_decimals,
                                                 "decimal.mark" = sep_decimal,
                                                 "big.mark" = ifelse(sep_thousands == "None", "", sep_thousands),
                                                 scientific = F),
                                   cellStyleDeclarations = list("xl-value-format" = paste0(prefix, ifelse(sep_thousands == "None", "", paste0("#", sep_thousands)), "##0", ifelse(nb_decimals > 0, paste0(sep_decimal, paste0(rep(0, nb_decimals), collapse = "")), ""), suffix)),
                                   visible = ifelse(is.null(combine_target), T, F))

              if (! is.null(combine_target) && combine_target != "") {
                pt$defineCalculation(calculationName = paste0(combine_target, "_", tolower(combine_idc), "_combine_", index),
                                     summariseExpression = get_expr(combine_idc, combine_target, additional_expr = c(get_additional_expr_num(), get_additional_expr_char())),
                                     visible = FALSE)
                pt$defineCalculation(calculationName = paste0(combine_target, "_", tolower(combine_idc), combine, combine_target, "_", tolower(combine_idc), "_combine_", index),
                                     caption = label,
                                     basedOn = c(paste0(target, "_", tolower(idc), "_", index), paste0(combine_target, "_", tolower(combine_idc), "_combine_", index)),
                                     type = "calculation",
                                     calculationExpression = paste0("values$", paste0(target, "_", tolower(idc), "_", index), combine, "values$", paste0(combine_target, "_", tolower(combine_idc), "_combine_", index)),
                                     format = list("digits" = nb_decimals, "nsmall" = nb_decimals,
                                                   "decimal.mark" = sep_decimal,
                                                   "big.mark" = ifelse(sep_thousands == "None", "", sep_thousands), scientific = F),
                                     cellStyleDeclarations = list("xl-value-format" = paste0(prefix, ifelse(sep_thousands == "None", "", paste0("#", sep_thousands)), "##0", ifelse(nb_decimals > 0, paste0(sep_decimal, paste0(rep(0, nb_decimals), collapse = "")), ""), suffix)))
              }
            }
          }

          pt$evaluatePivot()
          store_pt(pt)
        })
      } else {
        store_pt(NULL)
      }
    })
  })

  observe({
    cpt <- input$update_theme

    isolate({
      theme <- get_theme()

      if (! is.null(cpt) && cpt > 0) {
        showModal(
          modalDialog(
            title = "Update the theme",

            fluidRow(
              column(12,
                     textInput(ns("theme_fontname"), label = "Font name",
                               value = theme$fontName),
                     numericInput(ns("theme_fontsize"), label = "Font size (em)",
                                  value = as.numeric(gsub("em$", "", theme$fontSize)), min = 0, max = 10, step = 0.5),
                     column(6,
                            colourpicker::colourInput(ns("theme_headerbgcolor"), label = "Header bg color",
                                                      value = theme$headerBackgroundColor)
                     ),
                     column(6,
                            colourpicker::colourInput(ns("theme_headercolor"), label = "Header text color",
                                                      value = theme$headerColor)
                     ),
                     column(6,
                            colourpicker::colourInput(ns("theme_cellbgcolor"), label = "Cell bg color",
                                                      value = theme$cellBackgroundColor)
                     ),
                     column(6,
                            colourpicker::colourInput(ns("theme_cellcolor"), label = "Cell text color",
                                                      value = theme$cellColor)
                     ),
                     column(6,
                            colourpicker::colourInput(ns("theme_outlinecellbgcolor"), label = "Outline cell bg color",
                                                      value = theme$outlineCellBackgroundColor)
                     ),
                     column(6,
                            colourpicker::colourInput(ns("theme_outlinecellcolor"), label = "Outline text cell color",
                                                      value = theme$OutlineCell)
                     ),
                     column(6,
                            colourpicker::colourInput(ns("theme_totalbgcolor"), label = "Total bg color",
                                                      value = theme$totalBackgroundColor)
                     ),
                     column(6,
                            colourpicker::colourInput(ns("theme_totalcolor"), label = "Total text color",
                                                      value = theme$totalColor)
                     ),
                     colourpicker::colourInput(ns("theme_bordercolor"), label = "Border color",
                                               value = theme$borderColor)
              )
            ),
            easyClose = FALSE,
            footer = div(style = "margin-right: 20px;",
                         fluidRow(
                           column(3,
                                  div(actionButton(inputId = ns("theme_valid"), label = "Validate", width = "100%"), align = "left")
                           ),
                           column(3, offset = 6,
                                  div(actionButton(inputId = ns("theme_cancel"), label = "Cancel", width = "100%"), align = "right")
                           ))
            ))
        )
      }
    })
  })

  observe({
    cpt_valid <- input$theme_valid
    cpt_cancel <- input$theme_cancel

    isolate({
      if (! is.null(cpt_valid) && ! is.null(cpt_cancel) && (cpt_valid > 0 || cpt_cancel > 0)) {
        if (cpt_valid > 0) {
          theme <- get_theme()

          theme$fontName <- input$theme_fontname
          theme$fontSize <- paste0(input$theme_fontsize, "em")
          theme$headerBackgroundColor <- input$theme_headerbgcolor
          theme$headerColor <- input$theme_headercolor
          theme$cellBackgroundColor <- input$theme_cellbgcolor
          theme$cellColor <- input$theme_cellcolor
          theme$outlineCellBackgroundColor <- input$theme_outlinecellbgcolor
          theme$outlineCellColor <- input$theme_outlinecellcolor
          theme$totalBackgroundColor <- input$theme_totalbgcolor
          theme$totalColor <- input$theme_totalcolor
          theme$borderColor <- input$theme_bordercolor

          get_theme(theme)
        }

        shiny::removeModal()
      }
    })
  })

  counter_pivottable <- reactiveVal(0)

  observe({
    input$go_table
    theme <- get_theme()
    trigger_initialization()

    isolate({
      counter_pivottable(counter_pivottable() + 1)
      output[[paste0("pivottable_", counter_pivottable())]] <- renderPivottabler({

        isolate({
          pt <- store_pt()

          if (! is.null(pt)) {
            if (! is.null(get_initialization())) {
              get_initialization(NULL)
            }

            pt$theme <- theme

            pt$renderPivot()

          } else {
            NULL
          }
        })
      })
    })
  })

  output$pivottable <- renderUI({
    div(pivottablerOutput(ns(paste0("pivottable_", counter_pivottable())), width = "100%", height = "100%"), style = "padding-top: 1.5%;")
  })

  output$is_pivottable <- reactive({
    ! is.null(store_pt())
  })
  outputOptions(output, "is_pivottable", suspendWhenHidden = FALSE)

  get_wb <- reactive({
    pt <- store_pt()

    isolate({
      if (! is.null(pt)) {
        shiny::withProgress(message = 'Preparing the export...', value = 0.5, {
          wb <- createWorkbook(creator = "Shiny pivot table")
          addWorksheet(wb, "Pivot table")

          pt$writeToExcelWorksheet(wb = wb, wsName = "Pivot table",
                                   topRowNumber = 1, leftMostColumnNumber = 1,
                                   outputValuesAs = "formattedValueAsNumber",
                                   applyStyles = get_export_styles(), mapStylesFromCSS = TRUE)
          wb
        })
      }
    })
  })

  output$export <- downloadHandler(
    filename = function() {
      paste0("pivot_table_", base::format(Sys.time(), format = "%Y%m%d_%H%M%S") ,".xlsx")
    },
    content = function(file) {
      saveWorkbook(get_wb(), file = file, overwrite = TRUE)
    }
  )
}



#' @import pivottabler shiny
#'
#' @export
#'
#' @rdname shiny_pivot_table
#'
shinypivottablerUI <- function(id,
                               app_colors = c("#59bb28", "#217346"),
                               app_linewidth = 8) {
  ns <- shiny::NS(id)

  fluidPage(
    conditionalPanel(condition = paste0("output['", ns("show_title"), "']"),
                     div(h2(HTML("<b>Shiny pivot table</b>")), style = paste0("color: ", app_colors[2], "; margin-left: 15px;"))
    ),

    br(),

    # tags
    singleton(tags$head(
      tags$script(src = "shiny_pivot_table/shinypivottable.js")
    )),
    singleton(tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "shiny_pivot_table/shinypivottable.css")
    )),
    tags$head(
      tags$style(HTML("
        div.combine_padding { padding-top: 35px; }
        "))),
    tags$head(
      tags$style("
        .btn { width: 100%; }
      ")
    ),

    conditionalPanel(condition = paste0("output['", ns("ui_have_data"), "']"),
                     fluidRow(style = "padding-left: 1%; padding-right: 1%;",
                              column(12, style = paste0("border-radius: 3px; border-top: ", app_linewidth, "px solid ", app_colors[1], "; border-bottom: ", app_linewidth, "px solid ", app_colors[2], "; border-left: ", app_linewidth, "px solid ", app_colors[1], "; border-right: ", app_linewidth, "px solid ", app_colors[2], ";"),

                                     br(),

                                     column(2,
                                            fluidRow(
                                              div(h4(HTML("<b>Selected indicators</b>")), align = "center", style = "padding-right: 12px;"),

                                              conditionalPanel(condition = paste0("output['", ns("is_idcs"), "']"),
                                                               div(uiOutput(ns("selected_indicators")), style = "margin-top: 15px; overflow-y: auto; height: 130px; overflow-x: hidden; margin-right: 10px;")
                                              ),
                                              conditionalPanel(condition = paste0("! output['", ns("is_idcs"), "']"),
                                                               div(h3("None"), align = "center", style = paste0("padding-top: 20px; padding-right: 12px; color: ", app_colors[2], ";"))
                                              )
                                            )
                                     ),

                                     column(10, style = paste0("margin-bottom: 15px; border-left: 2px solid ", app_colors[1], ";"),
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

                                                     div(hr(style = paste0("border: 1px solid ", app_colors[1], ";")), style = "margin-top: -10px;"),

                                                     fluidRow(
                                                       column(2,
                                                              div(id = ns("id_padding_1"), textInput(ns("label"), label = "Label", value = "Auto", width  = "100%"))
                                                       ),
                                                       column(6,
                                                              fluidRow(
                                                                column(8,
                                                                       selectInput(ns("target"), label = "Selected target",
                                                                                   choices = NULL, width = "100%")
                                                                ),
                                                                column(4,
                                                                       selectInput(ns("idc"), label = "Indicator",
                                                                                   choices = NULL, width = "100%")
                                                                )
                                                              ),
                                                              conditionalPanel(condition = paste0("input['", ns("combine"), "'] !== 'None'"),
                                                                               fluidRow(
                                                                                 column(8,
                                                                                        selectInput(ns("combine_target"), label = "Selected target",
                                                                                                    choices = NULL, width = "100%")
                                                                                 ),
                                                                                 column(4,
                                                                                        selectInput(ns("combine_idc"), label = "Indicator",
                                                                                                    NULL, width = "100%")
                                                                                 )
                                                                               )
                                                              )
                                                       ),
                                                       column(2,
                                                              div(id = ns("id_padding_2"), selectInput(ns("combine"), label = "Combine",
                                                                                                       NULL, width = "100%"))
                                                       ),
                                                       column(1,
                                                              div(id = ns("id_padding_3"), actionButton(ns("specify_format"), label = "", icon = icon("paint-brush"), width = "100%"), style = "margin-top: 25px")
                                                       ),
                                                       column(1,
                                                              div(id = ns("id_padding_4"), actionButton(ns("add_idc"), label = "", icon = icon("plus"), width = "100%"), align = "center", style = "margin-top: 25px")
                                                       )
                                                     )
                                            )
                                     )
                              )
                     ),

                     br(),
                     br(),

                     fluidRow(style = "padding-left: 1%; padding-right: 1%;",
                              column(12, style = paste0("padding: 2.5%; overflow-x: auto; overflow-y: auto; border-radius: 3px; border-top: ", app_linewidth, "px solid ", app_colors[1], "; border-bottom: ", app_linewidth, "px solid ", app_colors[2], "; border-left: ", app_linewidth, "px solid ", app_colors[1], "; border-right: ", app_linewidth, "px solid ", app_colors[2], ";"),
                                     fluidRow(
                                       column(4, offset = 1,
                                              div(actionButton(ns("go_table"), label = "Display table", width = "100%" ), align = "right")
                                       ),
                                       column(2,
                                              div(actionButton(ns("update_theme"), label = "Update theme", width = "100%" ), align = "right")
                                       ),
                                       column(4,
                                              div(actionButton(ns("reset_table"), label = "Reset table", width = "100%"), align = "left")
                                       )
                                     ),

                                     br(),

                                     conditionalPanel(condition = paste0("output['", ns("is_pivottable"), "']"),
                                                      uiOutput(ns("pivottable")),
                                                      br(),
                                                      column(6, offset = 3,
                                                             div(downloadButton(ns("export"), label = "Download table"), align = "center", style = "width: 100%;")
                                                      )
                                     ),
                                     conditionalPanel(condition = paste0("! output['", ns("is_pivottable"), "']"),
                                                      div(h3("No data to display"), align = "center", style = paste0("color: ", app_colors[2], ";"))
                                     )
                              )
                     )
    ),
    conditionalPanel(condition = paste0("output['", ns("ui_have_data"), "'] === false"),
                     div(h3("No data to display"), align = "center", style = paste0("color: ", app_colors[2], ";"))
    )
  )
}
