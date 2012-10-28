#' Create file directory
#' 
#' ...
#' 
#' @param name Variable name
#' @param path Path
#'
create_dir <- function(name, path) {
  
  folder_path <- file.path(path, name)
  
  if(is.na(file.info(folder_path)$isdir)) {
    dir.create(folder_path, recursive = TRUE) 
  }

  return(folder_path)
}
