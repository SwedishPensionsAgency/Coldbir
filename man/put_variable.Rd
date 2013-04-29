\name{put_variable}
\alias{put_variable}
\title{Write vector to disk}
\usage{
  put_variable(x, name = NULL, path = getwd(), dims = NULL,
    attrib = NULL, lookup = TRUE, compress = 5)
}
\arguments{
  \item{x}{A data frame or vector}

  \item{name}{Variable name}

  \item{path}{Directory of where the file are to be
  created}

  \item{dims}{A numeric or character vector specifying the
  dimension of the data (e.g. year and month)}

  \item{attrib}{List of vector attributes}

  \item{compress}{Degree of compression in .gz file
  (size/speed - trade off). Zero compression gives most
  speed.}

  \item{lookup}{If lookup table should be added. It will be
  saved as a seperate file in the folder of the variable.}
}
\description{
  Takes a vector (or data frame) and save its content to a
  file in the correct cdb.gz-format.
}

