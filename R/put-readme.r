#' Put readme to disk
#'
#' Write readme of a variable to disk.
#'
#' @param x Character string
#' @param name Variable name
#' @param path Directory of where the file are to be created
#' @param create_dir If folder should be created when missing
#' 
#' @export
#'
put_readme <- function(x, name, path = getwd(), create_dir = TRUE) {
    
    folder_path <- file_path(name, path, create_dir = create_dir, file_name = FALSE, data_folder = FALSE)

    sink(file.path(folder_path, .readme_filename))
    cat(x)
    sink()
    
    message(name, ": readme was successfully written to disk")
    return(TRUE)
} 
