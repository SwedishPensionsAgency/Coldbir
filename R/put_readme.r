#' Put readme to disk
#'
#' Write readme of a variable to disk.
#'
#' @param lst Readme list
#' @param name Variable name
#' @param path Directory of where the file are to be created
#' @param create_dir If folder should be created when missing
#'
#' @export
#'
put_readme <- function(lst, name, path = getwd(), create_dir = TRUE) {
    
    folder_path <- file_path(name, path, create_dir = create_dir, file_name = FALSE, data_folder = FALSE)
    
    md <- list_to_md(lst)
    
    fl <- file(file.path(folder_path, "readme.txt"))
    writeLines(md, fl)
    close(fl)
    
    message("Readme was successfully written to disk")
    return(TRUE)
} 
