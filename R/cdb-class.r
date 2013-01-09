setClass(
    Class = "cdb",
    representation = representation(path = "character", type = "character", na = "numeric"),
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
    definition = function(.Object, path = getwd(), type = "numeric", na = NA_real_) {
        .Object@path <- path
        .Object@type <- type
        .Object@na <- na
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
        
        v <- get_variable(name = i, path = x@path, dims = j, na = x@na)
    
        # Convert to character or factor (if requested)
        if (x@type %in% c("character", "factor")) {
            factors <- if (x@type == "factor") TRUE else FALSE
            v <- to_char(x = v, name = i, path = x@path, factors = factors)
        }

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
        put_variable(x = value, name = i, dims = j, path = x@path)
        return(x)
    }
)

