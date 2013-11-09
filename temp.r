# Define parent class
setRefClass(
  "Coldbir",
  methods = list(
    show = function() {
      print("Coldbir Database")
    }
  )
)

cdbTest <- function(path, vars) {
  # Generate a list of functions (specific for each variable)
  var_methods <- lapply(vars, function(i){
    fun <- function(var = i) paste("This is", var)
    formals(fun)$var <- i
    return(fun)
  })
  names(var_methods) <- vars
  
  # Return class object
  mclass <- setRefClass(
    Class = paste0("cdb<", path, ">"),
    contains = "Coldbir",
    fields = list(
      vars = "character"
    ),
    methods = list(
      initialize = function(vars) {
        vars <<- vars
      }
    )
  )
  
  mclass$methods(var_methods)
  mclass$new(vars)
}

# Overload dollar names
.DollarNames.Coldbir <- function(x, pattern){
  grep(pattern, x$vars, value = TRUE)	
}

a <- cdbTest("C:/path 1", c("var1", "var2"))
b <- cdbTest("C:/path 2", c("hej", "tjo"))
