#' Replace keys with values in vector
#'
#' Function to replace keys with values in a variable vector
#'
#' @param v vector
#' @param ht hash table
#' @param factors if vector should be returned as factors, default is FALSE.
#'
#' @export
#'
key_to_value <- function(v, ht, factors = FALSE) {
    v <- ht[[2]][match(v, ht[[1]])]
    if (!factors) 
        v <- as.character(v)
    return(v)
} 
