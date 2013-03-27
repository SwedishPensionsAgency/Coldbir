#' Get all variables
#'
#' ...
#'
#' @param ... search_files() arguments
#' @param unique Return vector of unique variables
#' 
#' @export
list_variables <- function(..., unique = TRUE) {
    x <- basename(dirname(dirname(search_files(...))))
    if (unique) x <- unique(x)
    return(x)
}