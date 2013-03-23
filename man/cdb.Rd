\name{cdb}
\alias{cdb}
\title{Assign new (or existing) coldbir database}
\usage{
  cdb(...)
}
\arguments{
  \item{path}{Database path (the location of the coldbir
  database)}

  \item{type}{Return type of variable. Possible values: 'c'
  = character, 'f' = factor and 'n' = numeric (default).
  Character conversion might be a bit slow; hence numeric
  or factor is recommended.}

  \item{na}{Value representing missing values (default:
  NA_real_)}

  \item{md}{If markdown documentation should be added (in
  addition to a json-file)}
}
\description{
  Method to assign either a new or existing coldbir
  database to an R object. The current working directory is
  set as the default path.
}
\examples{
db <- cdb()
}

