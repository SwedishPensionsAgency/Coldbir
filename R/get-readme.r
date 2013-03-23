#' Get readme from disk
#'
#' Read readme of a variable from disk.
#'
#' @param name Variable name
#' @param path Directory of where the file are to be created
#' @param console TRUE if print to console instead of to character string
#' @param file_ext File extension
#' 
#' @export
#'
get_readme <- function(name, path, console = FALSE, file_ext = "json") {
    
    folder_path <- file_path(name, path, create_dir = FALSE, file_name = FALSE, data_folder = FALSE)
    
    con <- file(file.path(folder_path, paste(.readme_filename, file_ext, sep = ".")), "r")
    lns <- readLines(con, n = -1, warn = FALSE)
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
