#' Write data frame columns to disk
#'
#' Function to write all columns of a data frame to disk.
#'
#' @param df Data frame
#' @param path Database path
#' @param dicts If dictionaries should be created
#'
#' @export
#'
put_db <- function(df, path = getwd(), dicts = TRUE) {
    
    add_dict <- function(v, name) {
        if (is.character(v)) 
            v <- as.factor(v)
        if (is.factor(v)) {
            values <- levels(v)
            dict <- data.frame(key = 1:length(values), value = values)
            put_dict(dict, name = name, path = path)
        }
    }
    
    suppressMessages(sapply(names(df), function(x) {
        put_v(x = df[[x]], name = x, path = path)  # write vector
        if (dicts) 
            add_dict(df[[x]], x)  # write dictionary
    }))
    
    message("Files were successfully written to disk")
    return(TRUE)
} 
