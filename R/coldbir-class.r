db <- setRefClass(
    "coldbir",
    fields = list(path = "character"),
    methods = list(
        initialize = function(path = NULL) {
            path <<- if (is.null(path)) getwd() else path
        },
        put = function(x, name = NULL ,...) {
            if (is.vector(x)) {
                if (is.null(name)) {
                    name <- deparse(substitute(x))
                }
                put_v(x = x, path = path, name = name, ...)
            } else if (is.data.frame(x)) {
                sapply(names(x), function(i) {
                    put_v(x = x[[i]], name = i, ...)
                })
            }
        },
        get = function(...) get_v(path = path, ...)
    )
)
