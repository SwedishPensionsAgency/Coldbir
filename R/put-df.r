#' Write data frame columns to disk
#'
#' Function to write all columns of a data frame to disk.
#'
#' @param df Data frame
#' @param path Database path
#'
#' @export
#'
put_df <- function(df, path = getwd()) {
    
    suppressMessages(
        sapply(
            names(df), 
            function(x) put_v(x = df[[x]], name = x, path = path)
        )
    )
    
    message("Files were successfully written to disk")
    return(TRUE)
}
