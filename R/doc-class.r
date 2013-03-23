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
setMethod (
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
#' ...
#' 
#' @param x Documentation list
#' 
#' @examples r <- doc(title = "GDP", description = "Gross domestic product ...")
#' @export
#' 
doc <- function(...) {
    new(Class = "doc", ...)
}

#' Convert doc to markdown
#'
setGeneric("to_markdown", function(object){ standardGeneric("to_markdown") })
setMethod(
    f = "to_markdown",
    signature = "doc",
    definition = function(object) {
        list_to_md(object@lst)
    }
)

#' Convert doc to json
#' 
setGeneric("to_json", function(object){ standardGeneric("to_json") })
setMethod(
    f = "to_json",
    signature = "doc",
    definition = function(object) {
        toJSON(object@lst, pretty = TRUE)
    }
)