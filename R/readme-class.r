setClass(
    Class = "readme",
    representation = representation(lst = "list"),
    validity = function(object){
        return(TRUE)
    }
)

#' Class initializor
#' 
#' Method that runs when a new readme object is initialized.
#' 
setMethod (
    f = "initialize",
    signature = "readme",
    definition = function(.Object, ...) {
        .Object@lst <- list(...)
        validObject(.Object)
        return(.Object)
    }
)

#' Create new readme
#' 
#' ...
#' 
#' @param x Readme list
#' 
#' @examples r <- readme()
#' @export
#' 
readme <- function(...) {
    new(Class = "readme", ...)
}

#' Convert readme to markdown
#'
setGeneric("to_markdown", function(object){ standardGeneric("to_markdown") })
setMethod(
    f = "to_markdown",
    signature = "readme",
    definition = function(object) {
        list_to_md(object@lst)
    }
)

#' Convert readme to json
#' 
setGeneric("to_json", function(object){ standardGeneric("to_json") })
setMethod(
    f = "to_json",
    signature = "readme",
    definition = function(object) {
        toJSON(object@lst, pretty = TRUE)
    }
)