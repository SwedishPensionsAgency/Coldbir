\name{cdb}
\alias{cdb}
\title{Assign new (or existing) coldbir database}
\usage{
  cdb(...)
}
\arguments{
  \item{path}{database path; the location of your new or
  existing Coldbir database, where the last folder name of
  the path is the name of the database, e.g. `a <-
  cdb('data/MyCDB')` (default: `tempfile()`)}

  \item{compress}{file compression level}

  \item{encoding}{set documentation encoding (default:
  UTF-8)}

  \item{read_only}{read only (default: T)}
}
\description{
  Method to assign either a new or existing coldbir
  database to an R object. The current working directory is
  set as the default path.
}
\examples{
a <- cdb()
}

