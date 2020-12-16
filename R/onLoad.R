#' Adds the content of inst/assets/ to shinypivottabler/
#'
#' @importFrom shiny addResourcePath
#'
#' @noRd
#'
.onLoad <- function(...) {
  shiny::addResourcePath("shiny_pivot_table", system.file("demo_app/www", package = "shinypivottabler"))
}
