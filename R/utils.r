#' Escape characters
#' 
#' Use this to escape character strings.
#' Many thanks to Duncan Murdoch (http://goo.gl/1G1php)!
#' 
#' @param x character string
escape_char <- function(x) {
  gsub("[\x01-\x1f\x7f-\xff]", "", x)
}

#' Get function arguments
#' 
#' Especially useful in the doc class, where the provided
#' arguments are anything or a list (with anything).
#' 
#' @param ... documentation (provided as a list or as parameters)
get_args <- function(...) {
  args <- list(...)
  
  if (length(args) == 1 && is.list(args[[1]]) && is.null(names(args))) {
    return(args[[1]])
  } else {
    return(args)
  }
}

#' Create new time stamp
#' 
#' Creates a time stamp of the current system time.
new_time_stamp <- function(){
  return(as.double(lubridate::force_tz(Sys.time(), .tzone)))
}


#' Create recursive list from vector
#' 
#' A helper function to convert variable file names to a list representation
#' 
#' @param x character vector
#' @param val endpoint value
#' 
#' @examples \dontrun{
#' x <- c("a", "b", "c")
#' recursive_list(x)
#' }
recursive_list <- function(x, val) {
  r <- list()
  r[[as.character(x[1])]] <- if (length(x[-1]) != 0) recursive_list(x[-1], val) else val
  return(r)
}

#' Sorted modify list
#' 
#' Almost the same as utils::modifyList,
#' but also sorts the resulting list
#' 
#' @param x list
#' @param val list
#' 
#' @examples \dontrun{
#' x <- list(a = list(b = list(c = 1, e = 1), g = 1, h = 1))
#' y <- list(a = list(b = list(c = NULL, d = 1), f = 1, g = NULL))
#' sorted_modify_list(x, y)
#' }
sorted_modify_list <- function (x, val) {
  stopifnot(is.list(x), is.list(val))
  for (v in names(val)) {
    x[[v]] <- if (v %in% names(x) && is.list(x[[v]]) && is.list(val[[v]])) {
      sorted_modify_list(x[[v]], val[[v]])
    } else val[[v]]
  }
  
  # mixedorder works a bit strange if length == 1
  if (length(names(x)) > 1) {
    x <- x[gtools::mixedorder(names(x))]
  }
  
  return(x)
}

#' Clear empty branches
#' 
#' Clear all empty branches in a nested list
#' 
#' @param x list
clear_branch <- function (x) {
  for (i in names(x)) {
    if (sum(unlist(x[[i]])) == 0) {
      x[[i]] <- NULL
    } else {
      x[[i]] <- if (is.list(x[[i]])) clear_branch(x[[i]]) else x[[i]]
    }
  }
  return(x)
}
