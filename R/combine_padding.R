#' Add / remove padding
#'
#' @param session shiny session.
#' @param inputId Input's id to add / remove.
#' @param type 'regular' or 'combine'.
#'
#' @noRd
combine_padding <- function(session, inputId, type = "regular") {
  session$sendCustomMessage(
    type = "method_combine_padding",
    message = list(inputId = inputId, type = type)
  )
}
