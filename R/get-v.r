#' Read vector from disk
#' 
#' Reads a numeric vector from disk
#' 
#' @param name Variable name
#' @param path Directory of where the variable is located
#' @param dims A numeric or character vector specifying the dimension of the data (e.g. year and month)
#' 
#' @export
#'
get_v <- function(name, path = getwd(), dims = NULL) {
  
  v_name <- file_name(name, dims)
  v_dir <- file_dir(name, path, create_dir = FALSE)
  v_path <- file.path(v_dir, v_name)  

  path_compressed <- paste(v_path, "cdb.gz", sep = ".")
  path_uncompressed <- paste(v_path, "cdb", sep = ".")
  
  if (file.exists(path_compressed)) {
    bin_file <- gzfile(path_compressed, "rb")
  } else if (file.exists(path_uncompressed)) {
    bin_file <- file(path_uncompressed, "rb")
  } else {
    cat("File does not exist\n")
    return(NULL)
  }
  
}