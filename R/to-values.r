#' Replace keys with values in vector
#'
#' Function to replace keys with values in a variable vector
#'
#' @param v Vector
#' @param df Two-column data frame with keys and values
#' @param factors If vector should be returned as factors, default is FALSE.
#'
#' @export
#'
to_values <- function(v, df, factors = FALSE) {
    
    if (!is.data.frame(df) || ncol(df) != 2) 
        stop("Input must be a two-column data frame")
    
    v <- df[[2]][match(v, df[[1]])]
    if (!factors) 
        v <- as.character(v)
    return(v)
} 
