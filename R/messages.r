#' Log messages
#' 
#' Handlers for showing messages, warnings and errors.
#' These functions are useful as many of the log messages
#' are re-used in the code. `msg()` is used for ordinary messages,
#' `wrn()` for warnings and `err()` for throwing an error and
#' interrupting the code from further evaulation.
#' 
#' @param n message code. The number used must exist in table available in the code.
#' @param ... arguments passed on to `sprintf()`.
#' 
#' @examples
#' \dontrun{
#' msg(3, "foo")
#' }
#' 
#' @rdname messages
msg <- function(n, ...) {
  
  # Message codes are available in the table below;
  # some numbers in between have been removed since they aren't used anymore.
  str <- sprintf(switch(
    as.character(n),  # obs: convert to char since int otherwise refers to index
    "0"   = "Error message is not defined!",
    "1"   = "Config file updated, although `db$read_only` is set to TRUE",
    "2"   = "%s - writing failed; rollback! (%s)",
    "3"   = "%s - no such data base",
    "4"   = "%s : %s - no documantation for this variable",
    "6"   = "%s - file does not exist",
    "7"   = "%s - version of coldbir package and file format does not match",
    "8"   = "You're only allowed to read data, to change this use db$read_only <- F",
    "9"   = "%s - variable is NULL; nothing to write",
    "11"  = "%s - length of variable doesn't match the size of the other columns; nothing will be written",
    "13"  = "%s - data type is not supported",
    "14"  = "%s - writing failed; rollback! (%s)",
    "16"  = "input must be a two-column data frame",
    "17"  = "the database %s is empty",
    "18"  = "reading with an empty vector of variable names",
    "19"  = "the database variables doesn't match the required: %s",
    "20"  = "%s not found",
    "21"  = "nothing to get, probably missmatching dimensions, %s",
    "22"  = "The 'value' argument in ']<-' should be a vector, data.table, doc or NULL. It is a %s.",
    "23"  = "Attemption to remove non existing data %s.",
    "24"  = "The argument `i` is not used. The column names of the data.frame are used instead",
    "25"  = "%s wasn't added since character '.' isn't supported as variable name",
    "26"  = "All column names must be unique",
    "27"  = "No variable name provided. Expecting a name string during attpmtion to save vector to database",
    "28"  = "Time limit exceeded. The database was blocked by another precess."
    
  ), ...)
  
  if (is.null(str)) err(0)
  
  switch(
    as.character(match.call()[[1]]),  # name of calling function
    "msg" = message(str),
    "wrn" = warning(str),
    "err" = stop(str)
  )
}

#' @rdname messages
wrn <- msg

#' @rdname messages
err <- msg
