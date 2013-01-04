#' Put readme to disk
#'
#' Write readme of a variable to disk.
#'
#' @param name Variable name
#' @param path Directory of where the file are to be created
#' @param create_dir If folder should be created when missing
#' @param ... list_to_md(...)
#'
#' @export
#'
put_readme <- function(name, path = getwd(), create_dir = TRUE, ...) {
    
    folder_path <- file_path(name, path, create_dir = create_dir, file_name = FALSE, data_folder = FALSE)
    
    md <- list_to_md(...)
    
    sink(file.path(folder_path, "readme.md"))
    cat(md)
    sink()
    
    message(name, ": readme was successfully written to disk")
    return(TRUE)
} 
