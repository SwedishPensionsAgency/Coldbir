\name{get_variable}
\alias{get_variable}
\title{Read vector from disk}
\usage{
  get_variable(name, path = getwd(), dims = NULL, na = NA)
}
\arguments{
  \item{name}{Variable name}

  \item{path}{Directory of where the variable is located}

  \item{dims}{A numeric or character vector specifying the
  dimension of the data (e.g. year and month)}

  \item{na}{Specification of how missing values should be
  coded #}

  \item{format}{Format of return vector ('n' = numeric, 'f'
  = factor, 'c' = character)}
}
\description{
  Reads a numeric vector from disk
}

