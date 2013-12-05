\name{put_lookup}
\alias{put_lookup}
\title{Write lookup table to disk}
\usage{
  put_lookup(df, name, path = getwd(), create_dir = T)
}
\arguments{
  \item{df}{Two-column data frame with keys and values}

  \item{name}{Variable name}

  \item{path}{Directory of where the file are to be
  created}

  \item{create_dir}{If folder should be created when
  missing}
}
\description{
  Write lookup table that represents variable data to disk.
}

