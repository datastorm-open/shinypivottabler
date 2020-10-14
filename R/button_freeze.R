#' Enable / Disable a Button
#'
#' @param session shiny session.
#' @param inputId Input's id to enable / disable.
#' @param type 'enable' or 'disable'.
#'
#' @noRd
toggleBtnSPivot <- function(session, inputId, type = "disable") {
  session$sendCustomMessage(
    type = "togglewidgetShinyPivot",
    message = list(inputId = inputId, type = type)
  )
}
