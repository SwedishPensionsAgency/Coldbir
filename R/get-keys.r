#' Read keys from disk
#'
#' Read keys that represents variable data from disk.
#'
#' @param name Variable name
#' @param path Directory of where the file is located
#'
#' @export
#'
get_keys <- function(name, path = getwd()) {
    folder_path <- file_path(name, path, create_dir = FALSE, file_name = FALSE)
    
    df <- read.table(file = file.path(folder_path, "keys.txt"), header = TRUE, quote = "", sep = "\t")
    if (!is.data.frame(df) || ncol(df) != 2) 
        stop("File must be a two-column data frame")
    
    return(df)
} 
