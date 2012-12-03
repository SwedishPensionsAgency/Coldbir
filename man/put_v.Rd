\name{put_v}
\alias{put_v}
\title{Save vector to disk}
\usage{
  put_v(x, name, path = getwd(), dims = NULL,
    attrib = NULL, lookup = TRUE, compress = 5)
}
\arguments{
  \item{x}{A factor or numeric vector, NOT a character
  vector}

  \item{name}{Variable name}

  \item{path}{Directory of where the file are to be
  created}

  \item{dims}{A numeric or character vector specifying the
  dimension of the data (e.g. year and month)}

  \item{attrib}{List of vector attributes}

  \item{compress}{Degree of compression in .gz file
  (size/speed - trade off). Zero compression gives most
  speed.}

  \item{lookup}{If lookup table should be added}
}
\description{
  Takes a numeric vector and saves its content to a file in
  the correct cdb.gz-format.
}

