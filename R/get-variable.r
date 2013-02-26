#' Read vector from disk
#'
#' Reads a numeric vector from disk
#'
#' @param name Variable name
#' @param path Directory of where the variable is located
#' @param dims A numeric or character vector specifying the dimension of the data (e.g. year and month)
#' @param na Specification of how missing values should be coded
#' 
#' @importFrom RJSONIO fromJSON
#' @export
#'
get_variable <- function(name, path = getwd(), dims = NULL, na = NA) {

    # Get file path
    cdb <- file_path(name, path, dims, ext = c("cdb.gz", "cdb"), create_dir = FALSE)
    
    # Connect to compressed/uncompressed file
    if (file.exists(cdb[1])) {
        bin_file <- gzfile(cdb[1], "rb")
        
    } else if (file.exists(cdb[2])) {
        bin_file <- file(cdb[2], "rb")
        
    } else {
        stop(name, " - file does not exist")
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
    if (header$db_ver != as.integer(.database_version))
        stop(name, " - version of coldbir package and file format does not match")

    # Prepare data depending on vector type
    
    ## integer or factor
    if (header$type %in% c("integer", "factor")) {
        if (!is.na(na)) 
            x[is.na(x)] <- as.integer(na)
    
    ## double
    } else if (header$type == "double") {
        if (!is.null(header$exponent)) 
            x <- x/10^header$exponent
        if (!is.na(na))
            x[is.na(x)] <- as.double(na)

    ## logical
    } else if (header$type == "logical") {
        x <- (x > 0L)
        if (!is.na(na)) 
            x[is.na(x)] <- as.logical(na)
        
    ## Date
    } else if (header$type == "Date") {
        origin <- "1970-01-01"
        x <- as.Date(x, origin = origin)
        
    ## POSIXt
    } else if (header$type %in% c("POSIXct", "POSIXlt")) {
        origin <- as.POSIXct("1970-01-01 00:00:00", tz = "GMT")
        x <- as.POSIXct(x, origin = origin)  # slow => time * 1.5
        attributes(x)$tzone <- header$timezone
        
        if (header$type == "POSIXlt") x <- as.POSIXlt(x)  # very slow => time * 10
    }
    
    # Add attributes to vector
    if (!is.null(header$attributes)) {
        attributes(x) <- c(attributes(x), header$attributes)
    }
    
    return(x)
} 
