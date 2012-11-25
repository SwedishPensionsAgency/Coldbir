#' Read hash table from disk
#'
#' Read a hash table (dictionary) that represents variable data to disk.
#'
#' @param name Variable name
#' @param path Directory of where the file is located
#'
#' @export
#'
get_ht <- function(name, path = getwd()) {
    folder_path <- file_path(name, path, create_dir = FALSE, file_name = FALSE)
    df <- read.table(file = file.path(folder_path, "dict.txt"), header = TRUE, quote = "", sep = "\t")
    return(df)
} 
