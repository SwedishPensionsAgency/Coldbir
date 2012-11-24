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
file_path <- function(name, path, dims = NULL, ext, create_dir = FALSE, file_name = TRUE) {

  folder_path <- file.path(path, name)  
  
  if (is.na(file.info(folder_path)$isdir)) {
    if (create_dir) {
      dir.create(folder_path, recursive = TRUE)
    } else {
      stop("Variable folder does not exist")
    }
  
  
  if (file_name) {
    file_path <- file.path(folder_path, file_name(name, dims, ext))
  } else {
    file_path <- folder_path
  }

  return(file_path)
}
