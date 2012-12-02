#' Connect to a coldbir database
#'
#' ...
#'
#' @param path Database (absolute) path
#'
#' @export
#'
get_db <- function(path = getwd()) {
    db <- new.env(parent = parent.frame())
    vars <- list.files(path)
    for (name in vars) {
        if (is.v(name)) {
            
            fun <- function(na = NA) {
                sprintf("function(dims = NULL, na = %s) get_v(name = '%s', path = '%s', dims = dims, na = na)", na, 
                  name, path)
            }
            
            assign_fun <- function(name, fun) {
                assign(name, eval(parse(text = fun)), db)
                class(db[[name]]) <- "coldbir_v"
            }
            
            assign_fun(name, fun())
            assign_fun(sprintf("%s.0", name), fun(na = 0))
        }
    }
    class(db) <- "coldbir_db"
    return(db)
}

print.coldbir_db <- function(object) {
    cat("This is a coldbir database object (perhaps list all available variables?)")
    cat(lsf.str(object))
}

print.coldbir_v <- function(object) {
    cat("This is a brief documentation of the variable (it reads from markdown file)")
} 
