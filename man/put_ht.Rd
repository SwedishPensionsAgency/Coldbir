\name{put_ht}
\alias{put_ht}
\title{Write hash table to disk}
\usage{
  put_ht(ht, name, path = getwd(), create_dir = TRUE)
}
\arguments{
  \item{ht}{hash table; a two-column data frame with keys
  and values}

  \item{name}{Variable name}

  \item{path}{Directory of where the file are to be
  created}

  \item{create_dir}{If folder should be created when
  missing}
}
\description{
  Write a hash table (dictionary) that represents variable
  data to disk.
}

