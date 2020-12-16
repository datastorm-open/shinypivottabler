## Basic use

Two main functions in `shinypivottabler` :

- ``shinypivottablerUI``, the **UI** part of the module in **shiny** with at least the *id* argument
- ``shinypivottabler`` the **SERVER** part of the module in **shiny**, with at least the *data* argument. This function must be used with ``callModule`` function

By default, `shinypivottabler` initialize the pivot table inferface with : 

- **pivot choices** : all columns with less than ``max_n_pivot_cols = 100`` unique values
- **indicators choices** : all columns

### Code

``` r
# ui
ui = shiny::fluidPage(
  shinypivottablerUI(id = "id")
)

# server
server = function(input, output, session) {
  shiny::callModule(module = shinypivottabler,
                    id = "id",
                    data = data)
}

# more info in documentation : 
?shinypivottabler
```
