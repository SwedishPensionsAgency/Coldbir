setClass(
    Class = "cdb",
    representation = representation(path = "character", type = "character", na = "numeric", md = "logical"),
    validity = function(object){
        types <- c("c", "f", "n")
        if (!(object@type %in% types)) {
            stop("wrong type argument; only '", paste(types, collapse = "'/'"), "' are allowed")
        }
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
    definition = function(.Object, path = getwd(), type = "n", na = NA_real_, md = TRUE) {
        .Object@path <- path
        .Object@type <- type
        .Object@na <- na
        .Object@md <- md
        validObject(.Object)
        return(.Object)
    }
)

#' Assign new (or existing) coldbir database
#' 
#' Method to assign either a new or existing coldbir database to an R object.
#' The current working directory is set as the default path.
#' 
#' @param path Database path (the location of the coldbir database)
#' @param type Return type of variable. Possible values: 'c' = character, 'f' = factor and 'n' = numeric (default).
#' Character conversion might be a bit slow; hence numeric or factor is recommended.
#' @param na Value representing missing values (default: NA_real_)
#' @param md If markdown readme should be added (in addition to a json-file)
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
#' @examples 
#' db <- cdb()
#' get_path(db)
#' @exportMethod
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
        if (x@type %in% c("c", "f")) {
            factors <- if (x@type == "f") TRUE else FALSE
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
        
        if (class(value) == "readme") {
            
            # Create readme.json
            put_readme(x = to_json(value), name = i, path = x@path, file_ext = "json")

            if (x@md) {
                # Create readme.md
                put_readme(x = to_markdown(value), name = i, path = x@path, file_ext = "md")
            }
            
        } else {
            put_variable(x = value, name = i, dims = j, path = x@path)
        }
        return(x)
    }
)

#' Get variable documentation
#' 
#' ...
#' 
#' @exportMethod
#' 
setGeneric("doc", function(object, x = "character"){ standardGeneric("doc") })
setMethod(
    f = "doc",
    signature = "cdb",
    definition = function(object, x){
        readme <- get_readme(name = x, path = object@path, file_ext = "json")
        readme <- fromJSON(readme, simplifyWithNames = FALSE)
        return(readme)
    }
)