\name{put_documentation}
\alias{put_documentation}
\title{Put documentation to disk}
\usage{
  put_documentation(x, name, path = getwd(),
    create_dir = TRUE, file_name = .doc_json)
}
\arguments{
  \item{x}{Character string}

  \item{name}{Variable name}

  \item{path}{Directory of where the file are to be
  created}

  \item{create_dir}{If folder should be created when
  missing}

  \item{file_name}{Documentation file name}
}
\description{
  Write documentation of a variable to disk.
}

