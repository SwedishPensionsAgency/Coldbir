#' Create temporary file
#'
#' ...
#'
#' @param p Directory of where the file is to be located
#' 
#' @export
create_temp_file <- function(p) {
    tmp <- tempfile(tmpdir = "")
    tmp <- paste(p, substring(tmp, 2, nchar(tmp)), sep = "_")
}