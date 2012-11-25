\name{put_ht}
\alias{put_ht}
\title{Write hash table to disk}
\usage{
  put_ht(key, value, name, path = getwd(),
    create_dir = TRUE)
}
\arguments{
  \item{name}{Variable name}

  \item{path}{Directory of where the file are to be
  created}

  \item{key}{Key vector}

  \item{value}{Value vector}
}
\description{
  Write a hash table (dictionary) that represents variable
  data to disk.
}

