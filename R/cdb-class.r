setClass(
    Class = "cdb",
    representation = representation(path = "character", type = "character", na = "numeric", md = "logical", log_level = "numeric"),
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
    definition = function(.Object, path = getwd(), type = "n", na = NA_real_, md = TRUE, log_level = 4) {
        .Object@path <- path
        .Object@type <- type
        .Object@na <- na
        .Object@md <- md
        .Object@log_level <- log_level
        validObject(.Object)
        
        # Set log level
        flog.threshold(log_level)
        
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
#' @param md If markdown documentation should be added (in addition to a json-file)
#' @param log_level futile.logger level (default: 4). Available levels: 1-9.
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
#' @export
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
        
        if (all(class(value) == "doc")) {
            
            # Create readme.json
            put_variable_doc(x = to_json(value), name = i, path = x@path, file_name = .doc_json)

            if (x@md) {
                # Create readme.md
                put_variable_doc(x = to_markdown(value), name = i, path = x@path, file_name = .doc_md)
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
#' @param object Cdb object
#' @param name Variable nane
#' @export
#' 
setGeneric("get_doc", function(object, name = "character"){ standardGeneric("get_doc") })
setMethod(
    f = "get_doc",
    signature = "cdb",
    definition = function(object, name){
        d <- get_variable_doc(name = name, path = object@path, file_name = .doc_json)
        d <- RJSONIO::fromJSON(d, simplifyWithNames = FALSE)
        return(d)
    }
)

#' Help method: a?"foo"
#' 
#' @export
setMethod("?",  c("cdb", "character"), function(e1, e2) {
    x <- get_doc(e1, e2)
    cat(list_to_md(x))
    # Why does it return an S4 object instead??
    # TODO: if perfect match then return readme, otherwise return suggested results by search matching
})

#' Help method: ?a
#' 
#' @export
setMethod("?",  "cdb", function(e1) {
    cat("Database documenation/help ...")
    # returns NULL
})

#' List all variables in database
#' 
#' ...
#' 
#' @param object Cdb object
#' @export
#' 
setGeneric("vars", function(object){ standardGeneric("vars") })
setMethod(
    f = "vars",
    signature = "cdb",
    definition = function(object){
        x <- search_files(object@path)
        cat(x, sep = "\n")
    }
)

