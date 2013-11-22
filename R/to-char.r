#' Replace keys with values in vector
#'
#' Function to replace keys with values in a variable vector
#'
#' @param x Vector
#' @param ... See get_lookup().
#' @param factors If vector should be returned as factors, default is FALSE.
#'
#' @export
to_char <- function(x, ..., factors = FALSE) {
    
    df <- get_lookup(...)

    if (!is.null(df)) {
        x <- df[[2]][match(x, df[[1]])]
        if (!factors) {
            x <- as.character(x) 
        }
    }
    return(x)
}
