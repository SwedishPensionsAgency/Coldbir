\name{put_ht}
\alias{put_ht}
\title{Save hash table to disk}
\usage{
  put_ht(name, path = getwd(), key, value,
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
  Used to save a hash table (dictionary) that represents
  variable data to disk.
}

