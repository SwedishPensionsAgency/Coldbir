#' Write hash table to disk
#'
#' Write a hash table (dictionary) that represents variable data to disk.
#'
#' @param ht hash table; a two-column data frame with keys and values
#' @param name Variable name
#' @param path Directory of where the file are to be created
#'
#' @export
#'
put_ht <- function(ht, name, path = getwd(), create_dir = TRUE) {
    
    if (!is.data.frame(ht) || ncol(ht) != 2) 
        stop("Hash table must be a two-column data frame")
    
    folder_path <- file_path(name, path, create_dir = create_dir, file_name = FALSE)
    
    colnames(ht) <- c("key", "value")
    write.table(ht, file = file.path(folder_path, "dict.txt"), quote = FALSE, row.names = FALSE, sep = "\t")
    
    message("Hash table was successfully written to disk")
    return(TRUE)
} 
