#' Connect to a coldbir database
#'
#' ...
#'
#' @param path Database path
#'
#' @export
#'
get_db <- function(path = getwd()) {
    db <- new.env(parent = parent.frame())
    vars <- list.files()
    for (var in vars) {
        if (is.coldbir_v(var)) {
            fun <- sprintf("function(dims = NULL, na = NA) get_v(name = '%s', path = '%s', dims = dims, na = na)", 
                var, path)
            assign(var, eval(parse(text = fun)), db)
        }
    }
    return(db)
}

# TODO: Use S4 (or S3) classes to change print()-function to show brief documentation instead (e.g.
# '>db$Exer') 
