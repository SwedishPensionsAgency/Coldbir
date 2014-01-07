#' Generate coldbir file name
#'
#' Function that constructs a cdb file name
#'
#' @param name Variable name
#' @param dims Vector specifying the dimensions
#' @param ext File extension
#' 
#' @export
file_name <- function(name, dims = NULL, ext) {
    
    brackets <- c("[", "]")
    
    label <- if (!is.null(dims) && length(dims) != 0) {
        paste(brackets[1], dims, brackets[2], sep = "", collapse = "")
    } else ""
    
    file_name <- paste("d", label, ".", ext, sep = "")
    
    return(file_name)
} 
