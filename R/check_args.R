.check_cols <- function(colnames, cols, nulltoquote = F) {
  
  col_res <- intersect(colnames, cols)
  if (length(col_res) == 0 & nulltoquote) col_res <- ""
  return(col_res)
}

.check_init <- function(colnames, initialization, nulltoquote = F) {
  
  if (is.null(initialization)) return(NULL)
  init_names <- names(initialization)
  init_cols <- intersect(init_names, c("rows", "cols", "target", "combine_target"))
  
  invisible(lapply(init_cols, function(col) {
    initialization[[col]] <<- .check_cols(
      colnames, initialization[[col]], nulltoquote = nulltoquote)
    }))
  if (length(initialization[["idcs"]]) > 0) {
    idcs_names <- c("target", "combine_target")
    invisible(lapply(1:length(initialization[["idcs"]]), function(elem) {
      lapply(idcs_names, function(col) {
        if (col %in% names(initialization[["idcs"]][[elem]])) {
          initialization[["idcs"]][[elem]][[col]] <<- .check_cols(
          colnames, initialization[["idcs"]][[elem]][[col]], nulltoquote = nulltoquote)
        }
      })
    }))
    invisible(lapply(init_cols, function(col) {
      initialization[[col]] <<- .check_cols(
        colnames, initialization[[col]], nulltoquote = nulltoquote)
    }))
  }
  return(initialization)
}
