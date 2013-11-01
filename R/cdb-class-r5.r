cdb2 <- setRefClass(
  "cdb2",
  fields = list(
    path = "character",
    type = "character",
    na = "numeric",
    log_level = "numeric",
    log_file = "character"
  ),
  methods = list(
    initialize = function(
        path = getwd(),
        type = "f",
        na = NA_real_, 
        log_level = 4,
        log_file = ""
      ) {
      
      # Validate
      types <- c("c", "f", "n")
      if (!(type %in% types)) {
        stop("wrong type argument; only '", paste(types, collapse = "'/'"), "' are allowed")
      }
      
      # Set parameters
      path <<- path
      type <<- type
      na <<- na
      log_level <<- log_level
      log_file <<- log_file
      
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
    
    if (missing(j)) j <- NULL
    
    if (missing(i) || is.vector(i) && length(i) > 1){
      if (missing(i)){
        i <- get_vars(x, dims = FALSE)
      }
      
      # Create data.table with first variable
      v <- data.table(first = x[i[1], j])
      setnames(v, "first", i[1])
      
      # Add all other variables
      for(var in i[2:length(i)]){
        v[ , var := x[var, j], with = F]
      }
      
    } else {
      
      v <- get_variable(name = i, path = x$path, dims = j, na = x$na)
      
      # Convert to character or factor (if requested)
      if (x$type %in% c("c", "f")) {
        v <- to_char(x = v, name = i, path = x$path, factors = x$type == "f")
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
    
    if (missing(j)) j <- NULL
    
    if (all(class(value) == "doc")) {
      
      # Create readme.json
      put_variable_doc(x = to_yaml(value), name = i, path = x$path, file_name = .doc_file)
      
    } else {
      put_variable(x = value, name = i, dims = j, path = x$path)
    }
    
    return(x)
  }
)
