#' Generate coldbir file name
#' 
#' ...
#' 
#' @param name Variable name
#' @param dims Vector specifying the dimensions
#' 
file_name <- function(name, dims = NULL) {

  brackets <- c("[", "]")

  label <- if (!is.null(dims) && length(dims) != 0) {
     paste(brackets[1], dims, brackets[2], sep="", collapse="")
  } else ""
 
  v_name <- paste(name, label, sep="")	

  return(v_name)
}
