#' Save vector to disk
#'
#' Takes a numeric vector and saves its content to a file in the correct cdb.gz-format.
#'
#' @param x A factor or numeric vector, NOT a character vector
#' @param name Variable name
#' @param path Directory of where the file are to be created
#' @param dims A numeric or character vector specifying the dimension of the data (e.g. year and month)
#' @param attrib List of vector attributes
#' @param compress Degree of compression in .gz file (size/speed - trade off). Zero compression gives most speed.
#'
#' @export
#'
put_v <- function(x, name, path = getwd(), dims = NULL, attrib = NULL, compress = 5) {
    
    # Errors and warnings
    if (is.null(x)) 
        stop("Vector is NULL")
    if (all(is.na(x))) 
        warning("All values are missing")
    
    # Check/set vector type and number of bytes
    if (is.integer(x)) {
        type <- charToRaw("i")
        bytes <- 4L  # H_itemSize, note: NA for integers is already -2147483648 in R
        exponent <- 0L
        
    } else if (is.double(x)) {
        type <- charToRaw("d")
        exponent <- find_exp(x)
        
        if (exponent <= 9L) {
            x <- round(x * 10^exponent, 0)
            bytes <- check_repr(x)
            if (bytes <= 4L) {
                bytes <- 4L
            }
        } else {
            # Save as double
            bytes <- 8L
            exponent <- 0L
        }
        
    } else if (is.logical(x)) {
        type <- charToRaw("l")
        bytes <- 1L
        exponent <- 0L
        
    } else if (is.factor(x)) {
        type <- charToRaw("f")
        bytes <- 4L
        exponent <- 0L
        
    } else {
        stop("Wrong data type; only allows integer, double, factor or logical")
    }
    
    ext <- if (compress > 0) 
        "cdb.gz" else "cdb"
    
    # Construct file path
    cdb <- file_path(name, path, dims, ext, create_dir = TRUE)
    
    # Create file and add file extension
    if (compress > 0) {
        bin_file <- gzfile(cdb, open = "wb", compression = compress)
    } else {
        bin_file <- file(cdb, "wb")
    }
    
    # File header
    db_ver <- get_db_ver()
    
    attr_raw <- charToRaw(if (!is.null(attrib)) {
        RJSONIO:::toJSON(attrib, digits = 50)
    } else "")
    attr_len <- length(attr_raw)
    
    vector_len <- length(x)
    
    # Removes attributes from vector
    if (bytes == 8) {
        x <- as.double(x)
    } else {
        x <- as.integer(x)
    }
    
    # Write binary file
    writeBin(type, bin_file, size = 1)
    writeBin(as.raw(bytes), bin_file, size = 1)
    writeBin(as.raw(exponent), bin_file, size = 1)
    writeBin(db_ver, bin_file, size = 8)
    
    writeBin(attr_len, bin_file, size = 8)
    writeBin(attr_raw, bin_file)
    
    writeBin(vector_len, bin_file, size = 8)
    writeBin(x, bin_file, size = bytes)  # write each vector element to bin_file
    
    close(bin_file)
    
    message("File was successfully written to disk")
    return(TRUE)
} 
