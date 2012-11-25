#' Convert list to markdown
#'
#' Function to convert a list to a character string containing markdown syntax
#'
#' @param lst list
#'
#' @export
#'
list_to_md <- function(lst) {
  # E.g.
  # lst <- list("a1" = list("b1" = "some text", "b2" = "more text"), "a2" = "additional text")
  # list_to_md(lst)
  # Output:
  # # a1
  # 
  # ## b1
  # some text
  #
  # ## b2
  # more text
  #
  # # a2
  # additional text
  #
}

