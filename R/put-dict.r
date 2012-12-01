#' Write dictionary to disk
#'
#' Write dictionary that represents variable data to disk.
#'
#' @param df Two-column data frame with keys and values
#' @param name Variable name
#' @param path Directory of where the file are to be created
#' @param create_dir If folder should be created when missing
#'
#' @export
#'
put_dict <- function(df, name, path = getwd(), create_dir = TRUE) {
    
    if (!is.data.frame(df) || ncol(df) != 2) 
        stop("Input must be a two-column data frame")
    
    folder_path <- file_path(name, path, create_dir = create_dir, file_name = FALSE)
    
    colnames(df) <- c("key", "value")
    write.table(df, file = file.path(folder_path, "LOOKUP.txt"), quote = FALSE, row.names = FALSE, sep = "\t")
    
    message("Dictionary was successfully written to disk")
    return(TRUE)
} 
