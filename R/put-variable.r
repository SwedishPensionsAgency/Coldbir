#'
#' Takes a vector (or data frame) and save its content to a file in the correct cdb.gz-format.
#' 
#' @param x A data frame or vector
#' @param name Variable name
#' @param path Directory of where the file are to be created
#' @param dims A numeric or character vector specifying the dimension of the data (e.g. year and month)
#' @param attrib List of vector attributes
#' @param compress Degree of compression in .gz file (size/speed - trade off). Zero compression gives most speed.
#' @param lookup If lookup table should be added. It will be saved as a seperate file in the folder of the variable.
#'
#' @importFrom RJSONIO toJSON
#' @export
#'
put_variable <- function(x, name = NULL, path = getwd(), dims = NULL, attrib = NULL, lookup = TRUE, compress = 5) {
    
    # If x is a data frame it will recursively run put_variable over all columns
    if (is.data.frame(x)) {
        sapply(names(x), function(i) {
            put_variable(x = x[[i]], name = i, path = path, dims = dims, 
                attrib = attrib, lookup = lookup, compress = compress)
        })
        return(TRUE)
    } else {

        # Read object name if name argument is missing
        if (is.null(name)) name <- deparse(substitute(x))

        # Errors and warnings
        if (is.null(x)) stop(name, " - variable is NULL")
        if (all(is.na(x))) warning(name, " - all values are missing")
        
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
            if (any(is.na(x))) {
                warning(name, " - logical vector; NA is converted to FALSE")
            }
            
        } else if ("POSIXt" %in% class(x)) {  # OBS: must be checked before is.double
            header$type <- if ("POSIXct" %in% class(x)) "POSIXct" else "POSIXlt"
            header$bytes <- 8L  # save as double
            header$timezone <- format(x, format = "%Z")
            x <- as.double(x)  # convert to double
            
        } else if ("Date" %in% class(x)) {
                header$type <- "Date"
                header$bytes <- 8L

        } else if (is.factor(x) || is.character(x)) {
            if (is.character(x)) {
                x <- as.factor(x)
                warning(name, " - character converted to factor")
            }
            
            if (lookup) {
                values <- levels(x)
                lt <- data.frame(key = 1:length(values), value = values)
                put_lookup(lt, name = name, path = path)
            }
            
            header$type <- "factor"
            header$bytes <- 4L
            
        } else {
            stop(name, " - data type is not supported")
        }
        
        ext <- if (compress > 0) "cdb.gz" else "cdb"
        
        # Construct file path
        cdb <- file_path(name, path, dims, ext, create_dir = TRUE)
        
        # Create file and add file extension
        if (compress > 0) {
            bin_file <- gzfile(cdb, open = "wb", compression = compress)
        } else {
            bin_file <- file(cdb, "wb")
        }
        
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
        
        # Write binary file
        writeBin(header_len, bin_file, size = 8)
        writeBin(header_raw, bin_file)
        writeBin(length(x), bin_file, size = 8)
        writeBin(x, bin_file, size = header$bytes)  # write each vector element to bin_file
        
        close(bin_file)
        
        message(name, " - successfully written to disk")
        return(TRUE)

    }
} 
