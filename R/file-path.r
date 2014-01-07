#' Generate coldbir file path
#'
#' Function that constructs a cdb file path (and creates the folder)
#'
#' @param name Variable name
#' @param path Variable path
#' @param dims Vector specifying the dimensions
#' @param ext File extension
#' @param create_dir If folder should be created when missing
#' @param file_name If file name should be included in return path
#' @param data_folder If data folder should be added
#' 
#' @export
#'
file_path <- function(name, path, dims = NULL, ext = NULL, create_dir = F, file_name = T, data_folder = T) {
    
    folder_path <- if (data_folder) {
        file.path(path, name, "data")
    } else {
        file.path(path, name)
    }
    
    if (is.na(file.info(folder_path)$isdir)) {
        if (create_dir) {
            dir.create(folder_path, recursive = TRUE)
        } else {
            return(NA_character_)
        }
    }
    
    if (file_name) {
        file_path <- file.path(folder_path, file_name(name, dims, ext))
    } else {
        file_path <- folder_path
    }
    
    return(file_path)
} 
