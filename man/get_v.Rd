\name{get_v}
\alias{get_v}
\title{Read vector from disk}
\usage{
  get_v(name, path = getwd(), dims = NULL, na = NA)
}
\arguments{
  \item{name}{Variable name}

  \item{path}{Directory of where the variable is located}

  \item{dims}{A numeric or character vector specifying the
  dimension of the data (e.g. year and month)}

  \item{na}{Specification of how missing values should be
  coded}
}
\description{
  Reads a numeric vector from disk
}

