#' Get variable documentation from disk
#'
#' Read documentation of a variable from disk.
#'
#' @param name Variable name
#' @param path Directory of where the file are to be created
#' @param console TRUE if print to console instead of to character string
#' @param file_name Documentation file name
#' 
#' @export
#'
get_variable_doc <- function(name, path, console = FALSE, file_name = .doc_json) {
    
    folder_path <- file_path(name, path, create_dir = FALSE, file_name = FALSE, data_folder = FALSE)
    
    con <- file(file.path(folder_path, file_name), "r")
    lns <- readLines(con, n = -1, warn = FALSE)
    close(con)
    
    if (console) {
        limit <- 10
        cat(lns[1:limit], sep = "\n")
        if (limit < length(lns)) 
            cat("... (see doc for more information) ...\n")
    } else {
        str <- paste(lns, collapse = "\n")
        return(str)
    }
} 
