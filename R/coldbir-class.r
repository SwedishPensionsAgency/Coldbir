# Create coldbir database object with ReferenceClasses
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

# Overloading subscript operator with get_v (S3)
"[.coldbir" = "[[.coldbir" = function(obj, x, y = NULL) {
    get("obj")$get_v(name = x, dims = y)  
}

# Overloading subscript+assign operator with put_v (S3)
"[<-.coldbir" = "[[<-.coldbir" = function(obj, x, y = NULL, value) {
    get("obj")$put_v(x = value, name = x, dims = y)
    return(obj)   
}

# TODO: Write everything in S4 instead?
