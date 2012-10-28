#' Check representation
#' 
#' ...
#' 
#' @param x Number
#'
check_repr <- function(x) {
  
  xmax <- max(x, na.rm = TRUE)
  xmin <- min(x, na.rm = TRUE)
  
  if (xmax <= 127L && xmin >= -127L) bytes <- 1L
  else if (xmax <= 32767L && xmin >= -32767L)	bytes	<- 2L
  else if (xmax <= 2147483647L && xmin >= -2147483647L)	bytes	<- 4L
  else bytes <- 8L	# save as double
  
  return(bytes)
}