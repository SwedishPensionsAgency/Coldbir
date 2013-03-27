#' Start Coldbir help server
#'
#' ... shiny ...
#'
#' @param path Database path
#' @param search Search phrase
#'
#' @export
#'
run_help <- function(path, search = NULL) {
    .help_args <<- list("path" = tools::file_path_as_absolute(path), "search" = search)
    shiny::runApp(file.path(system.file(package = "Coldbir"), "help-app"), launch.browser = TRUE)
}
