\name{msg}
\alias{err}
\alias{msg}
\alias{wrn}
\title{Log messages}
\usage{
  msg(n, ...)

  wrn(n, ...)

  err(n, ...)
}
\arguments{
  \item{n}{message code. The number used must exist in
  table available in the code.}

  \item{...}{arguments passed on to `sprintf()`.}
}
\description{
  Handlers for showing messages, warnings and errors. These
  functions are useful as many of the log messages are
  re-used in the code. `msg()` is used for ordinary
  messages, `wrn()` for warnings and `err()` for throwing
  an error and interrupting the code from further
  evaulation.
}
\examples{
\dontrun{
msg(3, "foo")
}
}

