#' Write vector to disk
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
            if (any(is.na(x))) {
                flog.warn("%s - logical vector; NA is converted to FALSE", name)
            }
            
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
        
        ext <- if (compress > 0) "cdb.gz" else "cdb"
        
        # Construct file path
        cdb <- file_path(name, path, dims, ext, create_dir = TRUE)
        
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
        
        write_variable <- function() {
            # Create file and add file extension
            if (compress > 0) {
                bin_file <- gzfile(tmp, open = "wb", compression = compress)
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
        }
        
        # Create temporary file
        tmp <- create_temp_file(cdb)

        # Try to write file to disk
        tryCatch(
            write_variable(),
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
