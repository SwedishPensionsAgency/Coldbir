#' Read dictionary from disk
#'
#' Read dictionary that represents variable data from disk.
#'
#' @param name Variable name
#' @param path Directory of where the file is located
#'
#' @export
#'
get_dict <- function(name, path = getwd()) {
    folder_path <- file_path(name, path, create_dir = FALSE, file_name = FALSE)
    
    df <- read.table(file = file.path(folder_path, "LOOKUP.txt"), header = TRUE, quote = "", sep = "\t")
    if (!is.data.frame(df) || ncol(df) != 2) 
        stop("Dictionary must be a two-column data frame")
    
    return(df)
} 
