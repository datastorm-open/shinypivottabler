## UI css

In ``shinypivottablerUI``, you can customize quickly the **UI** part, and more precisely the borders, using : 

- ``app_colors``, vector of two colors
- ``app_linewidth``, borders width

You can also remove module title using ``show_title`` in ``shinypivottabler``.

## Table theme

User can modify the table theme using the button ``update theme`` in **shiny**. But you can also set the default theme in ``shinypivottabler`` function 

the ``theme`` object must be a list with this parameters

- **fontName** (default "Courier New, Courier")
- **fontSize** ("1.2em")
- **headerBackgroundColor** ("#217346")
- **headerColor** ("#FFFFFF")
- **cellBackgroundColor** ("#FFFFFF")
- **cellColor** ("#000000")
- **outlineCellBackgroundColor** ("#C0C0C0")
- **outlineCellColor** ("#000000")
- **totalBackgroundColor** ("#59bb28")
- **totalColor** ("#000000")
- **borderColor** ("#404040)"


Colors can be in *RGB* or in *HEX*.

### Code

``` r
# new default theme
theme <- list(
  fontName="arial",
  fontSize="1em",
  headerBackgroundColor = "#430838",
  headerColor = "#FFFFFF",
  cellBackgroundColor = "#FFFFFF",
  cellColor = "#000000",
  outlineCellBackgroundColor = "#C0C0C0",
  outlineCellColor = "#000000",
  totalBackgroundColor = "#e6e6e6",
  totalColor = "#000000",
  borderColor = "#000000"
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
                    theme = theme,
                    data = data)
}
```
