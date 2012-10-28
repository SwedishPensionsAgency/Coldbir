#' Create file name
#' 
#' ...
#' 
#' @param name Variable name
#' @param dims Vector specifying the dimensions
#' 
create_file_name <- function(name, dims = NULL) {

  brackets <- c("[", "]")

  label <- if (!is.null(dims) && length(dims) != 0) {
     paste(brackets[1], dims, brackets[2], sep="", collapse="")
  } else ""
 
  file_name <- paste(name, label, sep="")	

  return(file_name)
}
