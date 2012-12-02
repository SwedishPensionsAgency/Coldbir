#' Print coldbir_db
#'
#' ...
#'
#' @param object Object
#'
#' @export
#'

print.coldbir_db <- function(object) {
    cat("This is a coldbir database object (perhaps list all available variables?)")
    cat(lsf.str(object))
} 
