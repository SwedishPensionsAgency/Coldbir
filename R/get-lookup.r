#' Read dictionary from disk
#'
#' Read dictionary that represents variable data from disk.
#'
#' @param name Variable name
#' @param path Directory of where the file is located
#' 
#' @export
#'
get_lookup <- function(name, path = getwd()) {
    folder_path <- file_path(name, path, create_dir = FALSE, file_name = FALSE, data_folder = FALSE)
    
    df <- read.table(file = file.path(folder_path, "lookup.txt"), header = TRUE, quote = "", sep = "\t")
    if (!is.data.frame(df) || ncol(df) != 2) 
        stop("Lookup must be a two-column data frame")
    
    return(df)
} 
