#' Save hash table to disk
#'
#' Used to save a hash table (dictionary) that represents variable data to disk.
#'
#' @param name Variable name
#' @param path Directory of where the file are to be created
#' @param key Key vector
#' @param value Value vector
#'
#' @export
#'
put_ht <- function(name, path = getwd(), key, value, create_dir = TRUE) {
    
    folder_path <- file_path(name, path, create_dir = create_dir, file_name = FALSE)
    
    df <- data.frame(key = key, value = value)
    write.table(df, file = file.path(folder_path, "dictionary.tsv"), quote = FALSE, row.names = FALSE, sep = "\t")
    
    message("Hash table was successfully written to disk")
    return(TRUE)
} 
