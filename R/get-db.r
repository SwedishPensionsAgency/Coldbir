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
                sprintf("function(dims = NULL, na = %s) get_v(name = '%s', path = '%s', dims = dims, na = na)", 
                  na, name, path)
            }
            
            assign_fun <- function(f_name, fun) {
                assign(f_name, eval(parse(text = fun)), db)
                class(db[[f_name]]) <- "cdb_variable"
            }
            
            assign_fun(name, fun())
            assign_fun(sprintf("%s.0", name), fun(na = 0))
        }
    }
    class(db) <- "cdb_database"
    return(db)
}

print.cdb_database <- function(object) {
    cat("This is a coldbir database object (perhaps list all available variables?)")
}

print.cdb_variable <- function(object) {
    cat("This is a brief documentation of the variable (it reads from markdown file)")
} 
