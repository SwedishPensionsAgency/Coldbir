#' Assign new (or existing) coldbir database
#' 
#' Method to assign either a new or existing coldbir database to an R object.
#' The current working directory is set as the default path.
#' 
#' @param path Database path (the location of the coldbir database)
#' @param dims Set default dimensions
#' @param type Return type of variable. Possible values: 'c' = character, 'f' = factor and 'n' = numeric (default).
#' Character conversion might be a bit slow; hence numeric or factor is recommended.
#' @param na Value representing missing values (default: NA_real_)
#' @param log_level Log level (default: 4). Available levels: 1-9.
#' @param log_file Log file. As default log messages will be written to console.
#' 
#' @examples db <- cdb()
#' @export
#'
cdb2 <- setRefClass(
  "cdb2",
  fields = list(
    path = "character",
    type = "character",
    na = "numeric",
    log_level = "numeric",
    log_file = "character",
    dims = "ANY"
  ),
  methods = list(
    initialize = function(
        path = getwd(),
        type = "f",
        na = NA_real_, 
        log_level = 4,
        log_file = "",
        dims = NULL
      ) {
      
      # Validate
      types <- c("c", "f", "n")
      if (!(type %in% types)) {
        stop("wrong type argument; only '", paste(types, collapse = "'/'"), "' are allowed")
      }
      
      # Set parameters
      .self$path <- path
      .self$type <- type
      .self$na <- na
      .self$log_level <- log_level
      .self$log_file <- log_file
      .self$dims <- dims
      
      # Set futile.logger options
      flog.threshold(log_level)
      
      if (log_file != "") {
        flog.appender(appender.file(log_file))
      } else {
        flog.appender(appender.console())
      }
    }
  )
)

#' "["-method (overloading)
#' 
setMethod(
  f = "[",
  signature = "cdb2",
  definition = function(x, i, j){
    
    if (missing(j)) j <- x$dims
    
    if (missing(i) || is.vector(i) && length(i) > 1){
      if (missing(i)){
        vars <- get_vars(a, dims = T)
        fun <- function(x) isTRUE(all.equal(x, as.character(j)))
        i <- vars[sapply(vars$dims, fun)]$variable
      }
      
      # Create data.table with first variable
      v <- data.table(first = x[i[1], j])
      setnames(v, "first", i[1])
      
      # Add all other variables
      if (length(i) > 1) {
        for(var in i[2:length(i)]){
          v[ , var := x[var, j], with = F]
        }
      }
      
    } else {
      v <- get_variable(name = i, path = x$path, dims = j, na = x$na)
      
      # Convert to character or factor (if requested)
      if (x$type %in% c("c", "f")) {
        factors <- if (x$type == "f") TRUE else FALSE
        v <- to_char(x = v, name = i, path = x$path, factors = factors)
      }
    }
    
    return(v)
  }
)

#' "[<-"-method (overloading)
#' 
setMethod(
  f = "[<-",
  signature = "cdb2",
  definition = function(x, i, j, value){
    if (missing(j)) j <- x$dims
    
    if (all(class(value) == "doc")) {
      
      # Create readme.json
      put_variable_doc(x = to_yaml(value), name = i, path = x$path, file_name = .doc_file)
      
    } else {
      put_variable(x = value, name = i, dims = j, path = x$path)
    }
    
    return(x)
  }
)


