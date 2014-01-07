\name{cdb}
\alias{cdb}
\title{Assign new (or existing) coldbir database}
\usage{
  cdb(...)
}
\arguments{
  \item{path}{database path (the location of the coldbir
  database)}

  \item{compress}{file compression level}

  \item{encoding}{set documentation encoding (default:
  UTF-8)}

  \item{read_only}{read only (default: T)}

  \item{db_version}{instance-attribute for sycronisation of
  current list of variables}

  \item{n_row}{the common length of vecrors in the
  database}
}
\description{
  Method to assign either a new or existing coldbir
  database to an R object. The current working directory is
  set as the default path.
}
\examples{
db <- cdb()
}

