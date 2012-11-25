# BUILD SCRIPT - run before pushing to github

# RUN ROXYGEN
require("roxygen2")
roxygenize("../coldbir")

# RUN FORMATR
require("formatR")
suppressMessages(tidy.dir("R"))

# RUN TESTS
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {         
    source(file.path(path, nm), ...)
  }
}
sourceDir("R")

require("testthat")
test_file("inst/tests/1.r")