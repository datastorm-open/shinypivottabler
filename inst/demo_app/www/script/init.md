## Initialization

``pivot_cols`` and ``indicator_cols`` can be used to restrict possible column choices.

It's also possible to fully initialize the first pivot table with the argument ``initialization``, a named list with : 


- **rows** : names of selected rows or ``NULL``
- **cols** : names of selected columns or ``NULL``
- **target** : name of selected target or ``NULL``
- **idc** : name of selected indicator or ``NULL``
- **combine_target** : name of selected combine target or ``NULL``
- **combine_idc** : name of selected combine indicator or ``NULL``
- **combine** : combine operator or ``NULL``
- **idcs** : list of indicators to compute
    + label : mandatory
    + target : mandatory
    + idc : mandatory
    + nb_decimals : optionnal
    + sep_thousands : optionnal
    + sep_decimal : optionnal
    + prefix : optionnal
    + suffix : optionnal
    + combine : optionnal
    + combine_target : optionnal
    + combine_idc : optionnal


### Code

``` r
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

# ui
ui = shiny::fluidPage(
  shinypivottablerUI(id = "id", 
    app_colors = c("#e6e6e6", "#430838"),
    app_linewidth = 3
  )
)

# server
server = function(input, output, session) {
  shiny::callModule(module = shinypivottabler,
                    id = "id",
                    show_title = FALSE,
                    initialization = initialization,
                    data = data)
}
```
