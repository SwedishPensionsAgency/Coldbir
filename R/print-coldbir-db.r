#' Print coldbir_db
#'
#' ...
#'
#' @param object Object
#'
#' @export
#'

print.coldbir_db <- function(object) {
    vars <- attr(object, "variables")
    cat("Variables:\n", paste(b, collapse = ", "))
}
