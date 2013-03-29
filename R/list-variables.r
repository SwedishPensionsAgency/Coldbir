#' Get all variables
#'
#' ...
#'
#' @param path Path
#' @param dims If dims should be included (returns a data table)
#' @param ... search_files() arguments
#' 
#' @export
list_variables <- function(path = getwd(), dims = FALSE, ...) {
    files <- search_files(path = path, ...)
    
    # Extract variable names
    vars = basename(dirname(dirname(files)))
    
    if (dims) {
        
        # Extract dims
        var_dims <- str_extract_all(files, "\\[(\\w*)\\]")
        
        # Remove square brackets
        var_dims <- sapply(var_dims, function(x) {
            x <- gsub("\\[", "", x)
            x <- gsub("\\]", "", x)
            
            #if(length(x) == 0) x <- NULL
            return(x)
        })
        
        x <- data.table(
            variable = vars,
            dims = var_dims
        )
        
    } else {
        x <- vars
    }
    
    if (dims == FALSE) x <- unique(x)
    return(x)
}