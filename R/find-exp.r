#' Find exponent
#'
#' ...
#'
#' @param x Number
#'
find_exp <- function(x) {
    F <- 1
    exponent <- 0L
    while (max(abs(x * F - round(x * F, 0)), na.rm = TRUE) > F * 51 * .Machine$double.eps && exponent <= 
        9) {
        exponent <- exponent + 1L
        F <- 10^exponent
    }
    return(exponent)
} 
