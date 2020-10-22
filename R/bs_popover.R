extract_createTooltipOrPopoverOnUI <- function(id, type, options) {

  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")

  shiny::tags$script(shiny::HTML(paste0("$(document).ready(function() {setTimeout(function() {extract_shinyBS.addTooltip('", id, "', '", type, "', ", options, ")}, 500)});")))

}
extract_buildTooltipOrPopoverOptionsList <- function(title, placement, trigger, options, content) {

  if(is.null(options)) {
    options = list()
  }

  if(!missing(content)) {
    content <- gsub("'", "&#39;", content, fixed = TRUE)
    if(is.null(options$content)) {
      options$content = shiny::HTML(content)
    }
  }

  if(is.null(options$placement)) {
    options$placement = placement
  }

  if(is.null(options$trigger)) {
    if(length(trigger) > 1) trigger = paste(trigger, collapse = " ")
    options$trigger = trigger
  }

  if(is.null(options$title)) {
    options$title = title
    options$title <- gsub("'", "&#39;", options$title, fixed = TRUE)
  }

  return(options)

}



#' extract from shinyBS (extract_bsPopover)
#'
#' @param id Input id.
#' @param title The title of the popover.
#' @param content The main content of the popover.
#' @param placement Where the popover should appear relative to its target (top, bottom, left, or right). Defaults to bottom.
#' @param trigger What action should cause the popover to appear? (hover, focus, click, or manual). Defaults to hover.
#' @param options A named list of additional options to be set on the popover.
#'
#' @noRd
extract_bsPopover <- function (id, title, content, placement = "bottom", trigger = "hover",
          options = NULL)
{
  options = extract_buildTooltipOrPopoverOptionsList(title, placement,
                                             trigger, options, content)
  extract_createTooltipOrPopoverOnUI(id, "popover", options)
}
