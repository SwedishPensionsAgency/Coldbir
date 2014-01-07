#' Documentation class
#' 
#' Create a documentation list. Used for variable documentation
#' that is saved as a json-file in the variable folder.
#' 
#' @param ... Documentation list
#' @export
doc <- setRefClass(
  "doc",
  fields = list(
    lst = "list"
  ),
  methods = list(
    initialize = function(...) {
      .self$lst <- get_args(...)
    },
    
    # Convert documentation object to yaml
    to_json = function() {
      RJSONIO::toJSON(.self$lst, pretty = T, asIs = T)
    }
  )
)
