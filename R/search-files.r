#' Search files
#'
#' ...
#'
#' @param path Search path
#' @param ext Allowed extensions
#'
search_files <- function(path = getwd(), ext = "gz") {
    require(tools)
    files <- list.files(path, recursive = TRUE, full.names = TRUE)
    Filter(function(x) tolower(file_ext(x)) %in% ext, files)
}