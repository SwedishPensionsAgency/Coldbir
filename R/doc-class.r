#' Coldbir doc class
#' 
#' Coldbir documentation class
#' 
#' @slot lst contains a list
#' @docType methods
#' @rdname doc-methods
#' @aliases doc
setClass(
  Class = "doc",
  representation = representation(lst = "list"),
  validity = function(object){
    return(TRUE)
  }
)

#' Class initializor
#' 
#' Method that runs when a new doc object is initialized.
#' 
#' @rdname doc-methods
setMethod(
  f = "initialize",
  signature = "doc",
  definition = function(.Object, ...) {
    .Object@lst <- list(...)
    validObject(.Object)
    return(.Object)
  }
)

#' Create new doc
#' 
#' Function to create a new Coldbir documentation object
#' 
#' @param ... objects passed on to a list(...)
#' 
#' @examples r <- doc(title = "GDP", description = "Gross domestic product")
#' @rdname doc-methods
#' @export
doc <- function(...) {
  new(Class = "doc", ...)
}

#' Convert doc to json
#' 
#' Method to convert a docs object to a json character string
#' @rdname doc-methods
setGeneric("to_json", function(object){ standardGeneric("to_json") })
setMethod(
    f = "to_json",
    signature = "doc",
    definition = function(object) {
        RJSONIO::toJSON(object@lst, pretty = T, asIs = T)
    }
)