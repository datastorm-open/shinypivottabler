# New functions

By default, you can compute ``Sum``, ``Mean``, ``Min``, ``Max``, ``Standard Deviation``, ``Count`` and ``Count Distinct`` on quantitative variables and only ``Count`` and ``Count Distinct`` on qualitative variables.

And you can combine two indicatos using ``+``, ``-``, ``*``, and ``/``.

It's possible to add new functions with ``additional_expr_num`` (quantitative), ``additional_expr_char`` (qualitative) & ``additional_combine``

``additional_expr_num`` and ``additional_expr_char`` must be a named list with a specific definition in *character* using ``paste0``, the function and the key word ``, target,``.


### Code

``` r

additional_expr_num = list(
  "Median" = "paste0('median(', target, ', na.rm = TRUE)')"
)

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

additional_expr_char = list(
"Mode" = "paste0('getmode(', target, ')')"
)

additional_combine = c("Modulo" = "%%")

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
                    additional_expr_num = additional_expr_num,
                    additional_expr_char = additional_expr_char,
                    additional_combine = additional_combine,
                    data = data)
}

# more info in documentation : 
?shinypivottabler
```
