#' Print coldbir_db
#'
#' ...
#'
#' @param object Object
#'
#' @export
#'

print.coldbir_db <- function(object) {
    cat("Variables (and their functions):\n")
    print(lsf.str(object))
} 
