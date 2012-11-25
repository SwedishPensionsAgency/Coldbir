#' Read vector from disk
#'
#' Reads a numeric vector from disk
#'
#' @param name Variable name
#' @param path Directory of where the variable is located
#' @param dims A numeric or character vector specifying the dimension of the data (e.g. year and month)
#' @param na Specification of how missing values should be coded
#'
#' @export
#'
get_v <- function(name, path = getwd(), dims = NULL, na = NA) {
    
    # Get file path
    cdb <- file_path(name, path, dims, ext = c("cdb.gz", "cdb"), create_dir = FALSE)
    
    # Connect to compressed/uncompressed file
    if (file.exists(cdb[1])) {
        bin_file <- gzfile(cdb[1], "rb")
        
    } else if (file.exists(cdb[2])) {
        bin_file <- file(cdb[2], "rb")
        
    } else {
        stop("File does not exist")
    }
    
    type <- rawToChar(readBin(bin_file, raw(), n = 1, size = 1, signed = FALSE))
    bytes <- readBin(bin_file, integer(), n = 1, size = 1, signed = FALSE)
    exponent <- readBin(bin_file, integer(), n = 1, size = 1, signed = FALSE)
    db_ver <- readBin(bin_file, integer(), n = 1, size = 8)
    
    attr_len <- readBin(bin_file, integer(), n = 1, size = 8)
    attr_str <- rawToChar(readBin(bin_file, raw(), n = attr_len))
    
    vector_len <- readBin(bin_file, integer(), n = 1, size = 8)
    
    if (bytes <= 4) {
        x <- readBin(bin_file, integer(), n = vector_len, size = bytes)
        # if (H_itemSize == 1) data[data == -128] <- as.integer(NA) if (H_itemSize == 2) data[data ==
        # -32768] <- as.integer(NA)
    } else {
        x <- readBin(bin_file, double(), n = vector_len)
    }
    
    close(bin_file)
    
    # Check if using an old version of colbir
    if (db_ver != get_db_ver()) 
        stop("Version of coldbir package and file format does not match")
    
    # Prepare data depending on vector type
    if (type == "i") {
        if (!is.na(na)) 
            x[is.na(x)] <- as.integer(na)
    } else if (type == "d") {
        if (exponent > 0) 
            x <- x/10^exponent
        if (!is.na(na)) 
            x[is.na(x)] <- as.double(na)
    } else if (type == "l") {
        x <- (x > 0L)
        if (!is.na(na)) 
            x[is.na(x)] <- as.logical(na)
    }
    
    # Add attributes to vector
    attributes(x) <- if (attr_str != "") {
        as.list(RJSONIO:::fromJSON(attr_str))
    } else NULL
    
    return(x)
} 
