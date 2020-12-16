library(shiny)
library(shinypivottabler)
library(pivottabler)
library(data.table)


data <- data.table(bhmtrains)

data[, ArrivalDelta := as.numeric(difftime(ActualArrival, GbttArrival, units = "mins"))]
data[, DepartureDelta := as.numeric(difftime(ActualDeparture, GbttDeparture, units = "mins"))]

data[, Year := year(OriginGbttDeparture)]
data[, Month := month(OriginGbttDeparture)]

data <- data[!is.na(SchedSpeedMPH)]

data[, c("GbttArrival", "ActualArrival", "GbttDeparture", "ActualDeparture",
         "OriginGbttDeparture", "OriginActualDeparture", "DestinationGbttArrival", "DestinationActualArrival") := NULL]



theme <- list(
  fontName="arial",
  fontSize="1em",
  headerBackgroundColor = "#430838",
  headerColor = "rgb(255, 255, 255)",
  cellBackgroundColor = "rgb(255, 255, 255)",
  cellColor = "rgb(0, 0, 0)",
  outlineCellBackgroundColor = "rgb(192, 192, 192)",
  outlineCellColor = "rgb(0, 0, 0)",
  totalBackgroundColor = "#e6e6e6",
  totalColor = "rgb(0, 0, 0)",
  borderColor = "#000000"
)


additional_expr_num = list(
  "Median" = "paste0('median(', target, ', na.rm = TRUE)')"
)

getmode <- function(x) names(which.max(table(x)))

additional_expr_char = list(
  "Mode" = "paste0('getmode(', target, ')')"
)

additional_combine = c("Modulo" = "%%")


initialization <- list(
  "rows" = c("TOC", "Status"),
  "cols" = "TrainCategory",
  "target" = NULL,
  "idc" = NULL,
  "combine_target" = NULL,
  "combine_idc" = NULL,
  "combine" = NULL,
  "idcs" = c(
    list(
      # simple
      c("label" = "status", "target" = "ServiceId", "idc" = "Count")
    )
  )
)

