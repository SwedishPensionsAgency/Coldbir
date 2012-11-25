#' Replace keys with values in vector
#'
#' Function to replace keys with values in a variable vector
#'
#' @param v Vector
#' @param ... See get_keys().
#' @param factors If vector should be returned as factors, default is FALSE.
#'
#' @export
#'
to_values <- function(v, ..., factors = FALSE) {
    
    df <- get_dict(...)
    
    v <- df[[2]][match(v, df[[1]])]
    if (!factors) 
        v <- as.character(v)
    return(v)
} 
