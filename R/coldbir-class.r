db <- setRefClass(
    "coldbir",
    fields = list(path = "character"),
    methods = list(
        initialize = function(path = NULL) {
            path <<- if (is.null(path)) getwd() else path
        },
        put_v = function(...) put_variable(path = path, ...),
        get_v = function(...) get_variable(path = path, ...),
        
        put_r = function(...) put_readme(path = path, ...),
        get_r = function(...) get_readme(path = path, ...),
        
        put_l = function(...) put_lookup(path = path, ...),
        get_l = function(...) get_lookup(path = path, ...)
    )
)