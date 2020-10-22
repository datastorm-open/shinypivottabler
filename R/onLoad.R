#' Adds the content of inst/assets/ to shinyfilesmanager/
#'
#' @importFrom shiny addResourcePath
#'
#' @noRd
#'
.onLoad <- function(...) {
  shiny::addResourcePath("shiny_pivot_table", system.file("www", package = "shinypivottabler"))
}
