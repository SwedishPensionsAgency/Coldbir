#' Get readme from disk
#'
#' Read readme of a variable from disk.
#'
#' @param name Variable name
#' @param path Directory of where the file are to be created
#' @param console TRUE if print to console instead of to character string
#'
get_readme <- function(name, path, console = FALSE) {
    
    folder_path <- file_path(name, path, create_dir = FALSE, file_name = FALSE, data_folder = FALSE)
    
    con <- file(file.path(folder_path, "readme.md"), "r")
    lns <- readLines(con, n = -1)
    close(con)
    
    if (console) {
        limit <- 10
        cat(lns[1:limit], sep = "\n")
        if (limit < length(lns)) 
            cat("... (see readme for more information) ...\n")
    } else {
        str <- paste(lns, collapse = "\n")
        return(str)
    }
} 
