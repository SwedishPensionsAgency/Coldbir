setClass(
    Class = "cdb",
    representation = representation(path = "character"),
    validity = function(object){
        #if(!RJSONIO:::isValidJSON(toJSON(object@form), TRUE)) {
        #    stop("The argument is not a valid JSON list")
        #} 
        #if(length(object@form$schema$properties) != length(object@form$options$fields)) {
        #    stop("The number of element schemas and options must match")
        #}
        return(TRUE)
    }
)

#' Class initializor
#' 
#' Method that runs when a new coldbir database is initialized.
#' 
setMethod (
    f = "initialize",
    signature = "cdb",
    definition = function(.Object, path = getwd()) {
        .Object@path <- path
        return(.Object)
    }
)

#' Assign new (or existing) coldbir database
#' 
#' Method to assign either a new or existing coldbir database to an R object.
#' The current working directory is set as the default path.
#' 
#' @param path Database path (the location of the coldbir database)
#' 
#' @examples db <- cdb()
#' @export
#' 
cdb <- function(...) {
    new(Class = "cdb", ...)
}

#' Get path of coldbir database
#' 
#' Method to get the path of a coldbir database object.
#' 
#' @examples get_path(db)
#' 
setGeneric("get_path", function(object){ standardGeneric("get_path") })
setMethod(
    f = "get_path", 
    signature = "cdb",
    definition = function(object){
        return(object@path)
    }
)

#' "["-method (overloading)
#' 
setMethod(
    f = "[",
    signature = "cdb",
    definition = function(x, i, j){
        if (missing(j)) j <- NULL
        v <- get_variable(name = i, path = get_path(x), dims = j)
        
        # Add some attributes
        attr(v, "cdb_path") <- get_path(x)
        attr(v, "cdb_name") <- i
        attr(v, "cdb_dims") <- j

        return(v)
    }
)

#' "[<-"-method (overloading)
#' 
setMethod(
    f = "[<-",
    signature = "cdb",
    definition = function(x, i, j, value){
        if (missing(j)) j <- NULL
        put_variable(x = value, name = i, dims = j, path = get_path(x))
        return(x)
    }
)

