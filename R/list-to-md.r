#' Convert list to markdown
#'
#' Function to convert a list to a character string containing markdown syntax
#'
#' @param lst List to convert
#' @param str Return string
#' @param h Header start level
#'
list_to_md <- function(lst, str = "", h = 1) {
    lst_names <- names(lst)
    for (i in 1:length(lst_names)) {
        if (lst_names[[i]] != "") {
            str <- sprintf("%s%s %s\n\n", str, paste(rep("#", h), collapse = ""), lst_names[[i]])
        }
        str <- if (is.list(lst[[i]])) {
            list_to_md(lst[[i]], str, h + 1)
        } else {
            sprintf("%s%s\n\n", str, lst[[i]])
        }
    }
    return(str)
} 
