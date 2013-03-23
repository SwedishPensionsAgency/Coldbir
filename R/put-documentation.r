#' Put documentation to disk
#'
#' Write documentation of a variable to disk.
#'
#' @param x Character string
#' @param name Variable name
#' @param path Directory of where the file are to be created
#' @param create_dir If folder should be created when missing
#' @param file_name Documentation file name
#' 
#' @export
#'
put_documentation <- function(x, name, path = getwd(), create_dir = TRUE, file_name = .doc_json) {
    
    folder_path <- file_path(name, path, create_dir = create_dir, file_name = FALSE, data_folder = FALSE)

    sink(file.path(folder_path, file_name))
    cat(x)
    sink()
    
    message(name, ": documentation was successfully written to disk")
    return(TRUE)
} 
