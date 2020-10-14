#'Tooltips and Popovers
#'
#'Tooltips and Popovers allow you to add additional information about controls
#'or outputs without cluttering up your user interface. You can add a tooltip to
#'a button that displays on hover and better explains what the button will do, or
#'you could add a popover to an output providing further analysis of that output.
#'
#'@section Components:
#'There are eight functions in the Tooltips and Popovers family:
#'  \describe{
#'    \item{\code{\link{bsTooltip}}}{Used in the UI to add a tooltip to an element
#'    in your UI.}
#'    \item{\code{\link{extract_bsPopover}}}{Used in the UI to add a popover to an element
#'    in your UI.}
#'    \item{\code{\link{tipify}}}{Wrap any UI element in \code{tipify} to add a
#'    tooltip to the wrapped element. Preferred for elemented created with
#'    \code{\link{renderUI}}.}
#'    \item{\code{\link{popify}}}{Wrap any UI element in \code{popify} to add a
#'    popover to the wrapped element. Preferred for elements created with
#'    \code{\link{renderUI}}.}
#'    \item{\code{\link{addTooltip}}}{Used in the Server logic to add a tooltip
#'    to an element in your UI.}
#'    \item{\code{\link{addPopover}}}{Used in the Server logic to add a popover
#'    to an element in your UI.}
#'    \item{\code{\link{removeTooltip}}}{Used in the Server logic to remove a
#'    tooltip from an element in your UI.}
#'    \item{\code{\link{removePopover}}}{Used in the Server logic to remove a
#'    popover from an element in your UI.}
#'  }
#'
#'@details
#'You can create tooltips and popovers from either the UI script or within the
#'Server logic. \code{\link{bsTooltip}} and \code{\link{extract_bsPopover}} are used in
#'the UI, and \code{\link{addTooltip}} and \code{\link{addPopover}} are used in
#'the Server logic. \code{\link{tipify}} and \code{\link{popify}} can be used
#'within the UI or from within a \code{\link{renderUI}} in the Server logic. They
#'also have the added advantage of not requiring that the UI element have an ID
#'attribute.

## These Functions are common to multiple tooltip and popover functions
# Shared functions with really long names...
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
