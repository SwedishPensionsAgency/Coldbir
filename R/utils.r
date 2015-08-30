
# a couple of convinient aliases for the package environment 
is._           <- is.na # ._ <- NA in 0-constants.r

# a copy of "." definition in plyr by Hadley Wickham 
# (at least in version 0.4.2 on Cran or https://github.com/hadley/dplyr)
.              <- function (..., .env = parent.frame()) 
{structure(as.list(match.call()[-1]), env = .env, class = "quoted")}
# mainly to get conistance with data.table's handling of .() as list()
# ti's the simple way of avoiding conflicts with dplyr and still use 
# thr convinient dot-notation from data.table without any hard-maintained pieces of code
# the dot-function is exported as it is in dplyr

# we need the corresponding function for translation the do-notation into list-represantation
unpackDots <- function(x){
  if(class(x) == "quoted") {
    return(lapply(x, function(z) unpackDots(eval(z))))
  } else {
    if(is.call(x))  return(lapply(x, unpackDots)) else return(x)
  }
}
# example: x <- .(.(1:4,7:12),.(1999, 2011:2012), .("A", "B"))
# unpackDots(x)
# unpackDots(list())
# unpackDots(NULL)


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

#' Help function for better readability of the code
#' 
#' All is codet as numeric value .all
#' 
isALL <- function(x) {
  if(is.numeric(x)){ 
    return(any(x == .all))
  } else if(is.list(x)) {
    return(any(sapply(x,function(x){is.numeric(x) && x == .all})))
  } else return (FALSE)
}




#' Rephrasing of the actual data request
#' 
#' A helper function, never accessed directly by user
#' @param x   - variables to fetch - character vector or missing or .all (defined in 0-constants)
#' @param y   - dimensions to fetch - vector, list, missing, first element of vector or .all (-"-)
#' @param ... - remeining dimension
#' 
#' @return list of two elements
#'           vector of variables names or constnt .all
#'           data.table with all requested observation regardless if they exist or not
#' 
#' @examples \dontrun{
#' rephraseDataRequest()                            # empty => take all
#' rephraseDataRequest("a")                         # missing dimensions => take all observations
#' rephraseDataRequest(NULL)                        # any cases, even strange request -> inerpretation "empty list"
#' rephraseDataRequest("a", NULL)                   # any cases, even strange request -> inerpretation "empty list"
#' rephraseDataRequest(,2001,2,._)                  # wild card => take all abservetions in form (2001,2,...)
#'                                                  # all arguments are on the same level
#' rephraseDataRequest (c("a","b"),.(3:6,._, 1:2, c("d","e")))  # wild card + expand-grid 
#' rephraseDataRequest("a", c(3,4))                 # dimension represented as a vector
#' rephraseDataRequest(.("hej","a"), .(3,"4",NA))   # both variables and dimensions are expressed as lists
#' rephraseDataRequest("a",1,2,"3")                 # no essuptions about the data type of dimentions
#' rephraseDataRequest("a",.all,.all,"b",.all)      # translate .all into ._
#' rephraseDataRequest("a",.all,.all,._,.all)       # .all instead for data.table in the resulting list
#' rephraseDataRequest(.("a","b",._),1,2,3)         # + few other cases ...
#' rephraseDataRequest("*Var*", .("test1", .(2001, 2002:2003),.(1:3))) # handle unnecessary complex expressions
#' rephraseDataRequest("*Var*", .("test1", 2001:2003,1:3)) # the same as above, simplyfied
#' 
#' }
#' 
rephraseDataRequest <- function(x, y ,...) {
  
  # the first argument - named variables 
  #       vector or list or missing (or value == .all)
  #       list-notation . == list is allowed
  #       result:  a vector of variable names or value == .all

  if(missing(x)){ 
    x <- .all
  } else{
      x <- unpackDots(x)
      if(is.null(x)){
        return(list())
      } else {
        if(is.list(x)) x <- unlist(x)
      }
  }
  
  # solving problem with ambiguous .all or ._ in y
  if(any(is.na(x)) || any(x ==.all)) x <- .all 
  
  
  # the second argument (and eventually following args) - requested dimensions
  #       vector or list or missing or first element of a vector (or value == .all)
  #       list-notation . == list is enablad
  #       when using list (or .()): elements can specified as vectors and used for "grid expansion"
  #       wildcards ._ are allowed
  #       result: a data.table with requsted dimensions
  if(missing(y)){
    y <- .all 
  } else {
    y <- unpackDots(y)
    if(is.null(y)) {
       return(list())
    } else {
       if(!is.list(y)) y <- as.list(y)
       y <- lapply(y, unlist) # move up lists on second level
    }
  }
  
  #case with y as a vector
  if(!is.list(y)) y <- as.list(y)
  
  # case with y as just first element of a longer list
  y_rest <- list(...)
  if(length(y_rest) > 0L) y <- c(y, y_rest)
  
  # case with y as just first element of a longer list
  
  # translate any .all or ._ into ._ for the total dimension
  y <- lapply(y ,FUN=function(x){if(length(intersect(x,c(._ , .all)))>0) return(._) else return(x)})
  
  if( all(is.na(unlist(y)))){ 
    #  we don't need any data.table when all observations are requested
    y = .all
  } else {
    y = expand.grid(y,stringsAsFactors=FALSE)
    y = as.data.table(lapply(y, unlist ))
    ns <- names(y)
    setnames(y,ns, paste("V",1:length(ns),sep=""))
  }
  
  
  return (list(VAR = x, DIMS = y))
  
}

create_colname <- function(nm, dms) {
  if (length(dms) > 0) dms <- dms[!is.na(dms)]
  
  if (length(dms) > 0) # there are still !NA dimenstions left
   return(paste(nm, paste(dms, collapse = .col_sep$text), sep = .col_sep$text))  else return(nm)
}

attach.CB<- function(db)  #detta gör att alla gamla skipt kan köras som vanligt
{
  varTAB <- list_variables(db$path,dims=T) 
  
  vnames <- unique(varTAB$variable)
  for(v in vnames) 
  {
    f <- function(){}
    formals(f) <- alist(...=)
    class(f) <- "cb.data.as.function"
    assign(toupper(v), f,envir= .GlobalEnv) #MIDAS had upper case convention
  }
  
  invisible(NULL)
  
}

deatach.CB <- function(db)
{
  varTAB <- list_variables(db$path,dims=T) 
  vnames <- unique(varTAB$variable)
  vnames <- toupper(vnames)       #MIDAS had upper case convention
  rm(list = vnames, envir = .GlobalEnv) 
}


