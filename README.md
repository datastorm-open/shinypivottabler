# shinypivottabler


The great [pivottabler](http://www.pivottabler.org.uk/index.html) package enables pivot tables to be created with just a few lines of R.

The `pivottabler` package aims to:

-   Provide an easy way of creating pivot tables, without requiring the user to specify low-level layout logic.
-   Provide multiple ways of specifying calculation logic to cover both simple and more sophisticated requirements.
-   Provide styling options so the pivot tables can be themed/branded as needed.

All calculations for the pivot tables take place inside R, enabling the use of a wide-range of R functions in the calculation logic.

`shinypivottabler` just add a simple and usefull Shiny module to build, visualize, customize and export custom pivot table.

### Installation

You can install:

-   the latest development version from GitHub with

``` r
devtools::install_github("datastorm-open/shinypivottabler")
```

### Example

``` r
require(shinypivottabler)

# create artificial dataset
data <- data.frame("V1" = sample(c("A", "B", "C", "D"), size = 1000000,
                                 prob = rep(1, 4), replace = T),
                   "V2" = sample(c("E", "F", "G", "H"), size = 1000000,
                                 prob = rep(1, 4), replace = T),
                   "V3" = sample(c("I", "J", "K", "L"), size = 1000000,
                                 prob = rep(1, 4), replace = T),
                   "V4" = sample(c("M", "N", "O", "P"), size = 1000000,
                                 prob = rep(1, 4), replace = T),
                   "V5" = 1:1000000,
                   "V6" = 1000000:1)


server = function(input, output, session) {
  shiny::callModule(module = shinypivottabler,
                    id = "id",
                    data = data,
                    pivot_cols = c("V1", "V2", "V3", "V4"))
}

ui = shiny::fluidPage(
  shinypivottablerUI(id = "id")
)

shiny::shinyApp(ui = ui, server = server)


# more info in documentation : 
?shinypivottabler
```

- Define your pivot table

![](man/figures/init_module.png)

- Visualize & export

![](man/figures/view_table.png)

- Customize

![](man/figures/theme.png)
