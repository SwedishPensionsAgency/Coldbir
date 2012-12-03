#' Generate coldbir file path
#'
#' ...
#'
#' @param name Variable name
#' @param path Variable path
#' @param dims Vector specifying the dimensions
#' @param ext File extension
#' @param create_dir If folder should be created when missing
#' @param file_name If file name should be included in return path
#' @param data_folder If data folder should be added
#'
file_path <- function(name, path, dims = NULL, ext, create_dir = FALSE, file_name = TRUE, data_folder = TRUE) {
    
    folder_path <- if (data_folder) {
        file.path(path, name, "data")
    } else {
        file.path(path, name)
    }
    
    if (is.na(file.info(folder_path)$isdir)) {
        if (create_dir) {
            dir.create(folder_path, recursive = TRUE)
        } else {
            stop("Variable folder does not exist")
        }
    }
    
    if (file_name) {
        file_path <- file.path(folder_path, file_name(name, dims, ext))
    } else {
        file_path <- folder_path
    }
    
    return(file_path)
} 
