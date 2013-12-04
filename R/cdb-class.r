#' Assign new (or existing) coldbir database
#' 
#' Method to assign either a new or existing coldbir database to an R object.
#' The current working directory is set as the default path.
#' 
#' @param path Database path (the location of the coldbir database)
#' @param log_level Log level (default: 4). Available levels: 1-9.
#' @param log_file Log file. As default log messages will be written to console.
#' @param compress file compression level
#' @param read_only read only (default: T)
#' 
#' @examples db <- cdb()
#' @export
cdb <- setRefClass(
  "cdb",
  fields = list(
    path = "character",
    log_level = "numeric",
    log_file = "character",
    compress = "numeric",
    read_only = "logical"
  ),
  methods = list(
    initialize = function(
      path = getwd(),
      log_level = 4,
      log_file = "",
      compress = 5,
      read_only = T
    ) {
        
      # Set parameters
      .self$path <- path
      .self$log_level <- log_level
      .self$log_file <- log_file
      .self$compress <- compress
      .self$read_only <- read_only
      
      # Set futile.logger options
      flog.threshold(log_level)
      
      if (log_file != "") {
        flog.appender(appender.file(log_file))
      } else {
        flog.appender(appender.console())
      }
    },
    
    # Get variable documentation
    get_doc = function(name) {
      d <- get_variable_doc(name = name, path = .self$path, file_name = .doc_file)
      d <- yaml::yaml.load(d)
      return(d)
    },
    
    # List all variables
    get_vars = function(dims = F) {
      list_variables(path = .self$path, dims = dims)
    },
    
    # Get variable dimensions
    get_dims = function(name) {
      x <- list_variables(path = .self$path, dims = T)
      x <- subset(x, variable == name)
      return(x$dims)
    },
    
    # Get variable data
    get_variable = function(name, dims = NULL, na = NA) {
  
      # Get file path
      cdb <- file_path(name, .self$path, dims, ext = c("cdb.gz", "cdb"), create_dir = FALSE)
      
      # Connect to compressed/uncompressed file
      if (file.exists(cdb[1])) {
        bin_file <- gzfile(cdb[1], "rb")
          
      } else if (file.exists(cdb[2])) {
        bin_file <- file(cdb[2], "rb")
          
      } else {
        flog.error("%s - file does not exist", name)
        stop()
      }
      
      header_len <- readBin(bin_file, integer(), n = 1, size = 8)
      header_str <- rawToChar(readBin(bin_file, raw(), n = header_len))
      header <- fromJSON(header_str, simplifyWithNames = FALSE)
      
      vector_len <- readBin(bin_file, integer(), n = 1, size = 8)
      
      if (header$bytes <= 4) {
        x <- readBin(bin_file, integer(), n = vector_len, size = header$bytes)
      } else {
        x <- readBin(bin_file, double(), n = vector_len)
      }
      
      close(bin_file)
      
      # Check if using an old version of colbir
      if (header$db_ver != as.integer(.database_version)) {
        flog.error("%s - version of coldbir package and file format does not match", name)
        stop()
      }
  
      # Prepare data depending on vector type
      
      ## integer or factor
      if (header$type %in% c("integer", "factor")) {
        if (!is.na(na)) 
          x[is.na(x)] <- as.integer(na)
        
        if (header$type == "factor") {
          x <- to_char(x = x, name = name, path = .self$path, factors = T)
        }
      
      ## double
      } else if (header$type == "double") {
        if (!is.null(header$exponent)) 
          x <- x / 10^header$exponent
        if (!is.na(na))
          x[is.na(x)] <- as.double(na)
        
      ## logical
      } else if (header$type == "logical") {
        # NA's are stored as -1, thus they are replaced with NA
        x[x == -1L] <- NA
        
        # Replace 0/1 with TRUE/FALSE
        x <- (x > 0L)
        
        if (!is.na(na))
          x[is.na(x)] <- as.logical(na)
          
      ## Date
      } else if (header$type == "Date") {
        origin <- "1970-01-01"
        x <- as.Date(x, origin = origin)
        
      ## POSIXt
      } else if (header$type %in% c("POSIXct", "POSIXlt")) {
        origin <- as.POSIXct("1970-01-01 00:00:00", tz = .tzone)
        x <- as.POSIXct(x, tz = .tzone, origin = origin)
      }
      
      # Add attributes to vector
      if (!is.null(header$attributes)) {
        attributes(x) <- c(attributes(x), header$attributes)
      }
      
      return(x)
    },
    
    # Put variable data
    put_variable = function(x, name = NULL, dims = NULL, attrib = NULL, lookup = TRUE) {
      
      if (read_only) stop("You're only allowed to read data, to change this use cdb(..., read_only = F)")
      
      # If x is a data frame it will recursively run put_variable over all columns
      if (is.data.frame(x)) {
        sapply(names(x), function(i) {
          put_variable(
            x = x[[i]],
            name = i,
            dims = dims,
            attrib = attrib,
            lookup = lookup
          )
        })
        return(TRUE)
        
      } else {
  
        # Read object name if name argument is missing
        if (is.null(name)) name <- deparse(substitute(x))
        
        # if null => exit
        if (is.null(x)) {
          flog.warn("%s - variable is NULL; nothing to write", name)
          return(FALSE)
        }
        
        # Info
        if (all(is.na(x))) {
          flog.info("%s - all values are missing", name)
        }
          
        # Create empty header
        header <- list()
          
        # Check/set vector type and number of bytes
          
        if (is.numeric(x)) {
              
          if (is.integer(x)) {
            header$type <- "integer"
            header$bytes <- 4L  # H_itemSize, note: NA for integers is already -2147483648 in R
                    
          } else if (is.double(x)) {
            header$type <- "double"
            header$exponent <- find_exp(x)
            
            if (header$exponent <= 9L) {
              x <- round(x * 10^header$exponent, 0)
              header$bytes <- check_repr(x)
              if (header$bytes <= 4L) {
                header$bytes <- 4L
              }
            } else {
              header$bytes <- 8L
              header$exponent <- 0L
            }
          }
  
        } else if (is.logical(x)) {
          header$type <- "logical"
          header$bytes <- 1L
          
          # Replace integer value with NA,
          # unless -2147483648 is in the range
          x[is.na(x)] <- -1L
          
        } else if ("POSIXt" %in% class(x)) {  # OBS: must be checked before is.double
          header$type <- "POSIXct"
          header$bytes <- 8L  # save as double
          
          x <- lubridate::force_tz(x, .tzone)  # convert to GMT
          x <- as.double(x)  # convert to double
          
        } else if ("Date" %in% class(x)) {
          header$type <- "Date"
          header$bytes <- 8L
          
        } else if (is.factor(x) || is.character(x)) {
          if (is.character(x)) {
            x <- as.factor(x)
            flog.warn("%s - character converted to factor", name)
          }
          
          if (lookup) {
            values <- levels(x)
            lt <- data.frame(key = 1:length(values), value = values)
            put_lookup(lt, name = name, path = path)
          }
          
          header$type <- "factor"
          header$bytes <- 4L
              
        } else {
          flog.error("%s - data type is not supported", name)
          stop()
        }
        
        ext <- if (.self$compress > 0) "cdb.gz" else "cdb"
        
        # Construct file path
        cdb <- file_path(
          name = name,
          path = .self$path,
          dims = dims,
          ext = ext,
          create_dir = T
        )
        
        # File header
        header$db_ver <- as.integer(.database_version)
          
        # Add attributes
        header$attributes <- attrib
          
        header_raw <- charToRaw(toJSON(header, digits = 50))
        header_len <- length(header_raw)
        
        # Removes attributes from vector
        if (header$bytes == 8) {
          x <- as.double(x)
        } else {
          x <- as.integer(x)
        }
        
        # Create temporary file
        tmp <- create_temp_file(cdb)
  
        # Try to write file to disk
        tryCatch({
          
          # Create file and add file extension
          if (.self$compress > 0) {
            bin_file <- gzfile(tmp, open = "wb", compression = .self$compress)
          } else {
            bin_file <- file(tmp, "wb")
          }
            
          # Write binary file
          writeBin(header_len, bin_file, size = 8)
          writeBin(header_raw, bin_file)
          writeBin(length(x), bin_file, size = 8)
          writeBin(x, bin_file, size = header$bytes)  # write each vector element to bin_file
          close(bin_file)
  
          # Rename temporary variable to real name (overwrite)
          file.copy(tmp, cdb, overwrite = TRUE)
              
          },
          finally = file.remove(tmp),
          error = function(e) {
            flog.fatal("%s - writing failed; rollback! (%s)", name, e)
          }
        )
            
        # Return TRUE and message if variable is successfully written
        flog.info(cdb)
        return(TRUE)
      }
    }
  )
)

#' Extract content from variable
#' 
#' Function to extract data content from a Coldbir variable.
#' Overloads the `[`-method.
#' 
#' @param x cdb object
#' @param i variable name
#' @param j variable dims
#' @param na NA value (default: NA)
#' 
#' @name `[`
#' @docType methods
#' @rdname extract-methods
setMethod(
  f = "[",
  signature = "cdb",
  definition = function(x, i, j, na = NA){
    
    if (missing(j)) j <- NULL
    
    if (missing(i) || is.vector(i) && length(i) > 1){
      if (missing(i)){
        vars <- x$get_vars(dims = T)
        fun <- function(x) isTRUE(all.equal(x, as.character(j)))
        i <- vars[sapply(vars$dims, fun)]$variable
      }
      
      # Create data.table with first variable
      v <- data.table(first = x[i[1], j, na = na])
      setnames(v, "first", i[1])
      
      # Add all other variables
      if (length(i) > 1) {
        for(var in i[2:length(i)]){
          # Use function call below since the data.table (`:=`) otherwise
          # interprets `x` as a column name, if it exists (see issue 49).
          read_var <- function() x[var, j, na = na]
          v[ , var := read_var(), with = F]
        }
      }
      
    } else {
      v <- x$get_variable(name = i, dims = j, na = na)
    }
    
    return(v)
  }
)

#' Assign content to variable 
#' 
#' Function to assign content, either data or documentation,
#' to a Coldbir variable. Overloads the `[<-`-method.
#' 
#' @param x cdb object
#' @param i variable name
#' @param j variable dims
#' @param value value
#'
#' @name `[<-`
#' @docType methods
#' @rdname replace-methods
setMethod(
  f = "[<-",
  signature = "cdb",
  definition = function(x, i, j, value){
    if (missing(j)) j <- NULL
    if (x$read_only) stop("You're only allowed to read data, to change this use cdb(..., read_only = F)")
    
    if (all(class(value) == "doc")) {
      
      # Create readme.json
      put_variable_doc(x = to_yaml(value), name = i, path = x$path, file_name = .doc_file)
      
    } else {
      x$put_variable(x = value, name = i, dims = j)
    }
    
    return(x)
  }
)
