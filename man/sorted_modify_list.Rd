\name{sorted_modify_list}
\alias{sorted_modify_list}
\title{Sorted modify list}
\usage{
  sorted_modify_list(x, val)
}
\arguments{
  \item{x}{list}

  \item{val}{list}
}
\description{
  Almost the same as utils::modifyList, but also sorts the
  resulting list
}
\examples{
\dontrun{
x <- list(a = list(b = list(c = 1, e = 1), g = 1, h = 1))
y <- list(a = list(b = list(c = NULL, d = 1), f = 1, g = NULL))
sorted_modify_list(x, y)
}
}

