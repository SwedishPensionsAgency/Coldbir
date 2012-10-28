#' Generate coldbir file path
#' 
#' ...
#' 
#' @param name Variable name
#' @param path Variable path
#' @param dims Vector specifying the dimensions
#' @param ext File extension
#' @param create_dir 
#' 
file_path <- function(name, path, dims = NULL, ext, create_dir = FALSE) {

  folder_path <- file.path(path, name)
  
  if(create_dir && is.na(file.info(folder_path)$isdir)) {
    dir.create(folder_path, recursive = TRUE)
  }
  
  file_path <- file.path(folder_path, file_name(name, dims, ext))

  return(file_path)
}
