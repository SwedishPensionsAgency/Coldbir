\name{cdb}
\alias{cdb}
\title{Assign new (or existing) coldbir database}
\usage{
  cdb(...)
}
\arguments{
  \item{path}{Database path (the location of the coldbir
  database)}

  \item{log_level}{Log level (default: 4). Available
  levels: 1-9.}

  \item{log_file}{Log file. As default log messages will be
  written to console.}

  \item{compress}{file compression level}

  \item{read_only}{read only (default: T)}
}
\description{
  Method to assign either a new or existing coldbir
  database to an R object. The current working directory is
  set as the default path.
}
\examples{
db <- cdb()
}

