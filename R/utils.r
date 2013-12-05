#' Escape characters
#' 
#' Use this to escape character strings.
#' Many thanks to Duncan Murdoch (http://goo.gl/1G1php)!
#' 
#' @param x character string
escape_char <- function(x) {
  gsub("[\x01-\x1f\x7f-\xff]", "", x)
}
