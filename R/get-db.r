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
    for (var in vars) {
        if (is.coldbir_v(var)) {
            fun <- sprintf("function(dims = NULL, na = NA) get_v(name = '%s', path = '%s', dims = dims, na = na)", 
                var, path)
            assign(var, eval(parse(text = fun)), db)
            class(db[[var]]) <- "cdb_variable"
        }
    }
    class(db) <- "cdb_database"
    return(db)
}

print.cdb_database <- function(object){
    cat("This is a coldbir database object (perhaps list all available variables?)")
}

print.cdb_variable <- function(object){
    cat("This is a brief documentation of the variable (it reads from markdown file)")
}
