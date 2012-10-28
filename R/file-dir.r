#' Get or create file directory
#' 
#' ...
#' 
#' @param name Variable name
#' @param path Path
#' @param create_dir Create dir if missing
#'
file_dir <- function(name, path, create_dir = FALSE) {
  
  folder_path <- file.path(path, name)
  
  if(create_dir && is.na(file.info(folder_path)$isdir)) {
    dir.create(folder_path, recursive = TRUE) 
  }

  return(folder_path)
}
