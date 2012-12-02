#' Write data frame columns to disk
#'
#' Function to write all columns of a data frame to disk.
#'
#' @param df Data frame
#' @param path Database path
#' @param lookup If lookup should be created
#'
#' @export
#'
put_db <- function(df, path = getwd(), lookup = TRUE) {
    
    suppressMessages(sapply(names(df), function(x) {
        put_v(x = df[[x]], name = x, path = path, lookup = lookup)  # write vector
    }))
    
    message("Files were successfully written to disk")
    return(TRUE)
} 
