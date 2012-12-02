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
            
            assign_fun <- function(name, fun, cl = TRUE) {
                assign(name, eval(parse(text = fun)), db)
                if (cl) 
                  class(db[[name]]) <- "coldbir_v"
            }
            
            assign_fun(name, fun())
            assign_fun(sprintf("%s.0", name), fun(na = 0))
            assign_fun(sprintf("%s.r", name), sprintf("function() get_readme(name = '%s', path = '%s', console = TRUE)", 
                name, path), cl = FALSE)
            assign_fun(sprintf("%s.l", name), sprintf("function() get_dict(name = '%s', path = '%s')", name, path), 
                cl = FALSE)
            # assign(sprintf('print.%s', name), eval(parse(text = sprintf( 'function(object) get_readme(name = '%s', path =
            # '%s', console = TRUE)' , name, path))), envir = .GlobalEnv)
        }
    }
    class(db) <- "coldbir_db"
    return(db)
} 
