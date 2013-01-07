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
#' Method that runs when a new coldbir object is initialized.
setMethod (
    f = "initialize",
    signature = "cdb",
    definition = function(.Object, path = getwd()) {
        .Object@path <- path
        return(.Object)
    }
)

#' Create new coldbir object
#' 
#' Use this to create a new coldbir object.
#' @export
#' @param path Path
#' @examples
#' db <- cdb()
coldbir <- function(...) {
    new(Class = "cdb", ...)
}

#' Get path from cdb object
#' 
#' Method to get the path of a cdb object.
#' @export
#' @docType methods
#' @rdname get_path
#' @examples
#' get_path(db)
setGeneric("get_path", function(object){ standardGeneric("get_path") })

#' @rdname get_path
setMethod(
    f = "get_path", 
    signature = "cdb",
    definition = function(object){
        return(object@path)
    }
)

#' "["-method (overloading)
#' 
#' @export
setMethod(
    f = "[",
    signature = "cdb",
    definition = function(x, i, j){
        if (missing(j)) j <- NULL
        data <- get_variable(name = i, path = get_path(x), dims = j)
        return(data)
    }
)

#' "[<-"-method (overloading)
#' 
#' @export
setMethod(
    f = "[<-",
    signature = "cdb",
    definition = function(x, i, j, value){
        if (missing(j)) j <- NULL
        put_variable(x = value, name = i, dims = j, path = get_path(x))
        return(x)
    }
)
