#' Replace keys with values in vector
#'
#' Function to replace keys with values in a variable vector
#'
#' @param x Vector
#' @param ... See get_lookup().
#' @param factors If vector should be returned as factors, default is FALSE.
#'
#' @export
#'
to_char <- function(x, ..., factors = FALSE) {  # The fnction seems to be absolete for the moment. 
                                                # The corresponding code has been moved to the cbd-class
    df <- get_lookup(...)

    if (!is.null(df)) {
        x <- df[[2]][match(x, df[[1]])]
        if (!factors) {
            x <- as.character(x) 
        }
    }
    
    return(x)
}
