path <- tempfile()
size <- 1e3
db <- cdb(path, log_level = 1, read_only = F)

context("INITIALIZE DATABASE")
##############################
test_that("init cdb", {
    expect_equal(path, db$path)
})

context("VARIABLE TYPES")
#########################
x <- sample(c(T, F), size, replace = T)
db["boolean"] <- x
test_that("boolean", {
    expect_equal(x, db["boolean"])
})

x <- sample(c(T, F, NA), size, replace = T)
db["boolean_NA"] <- x
test_that("boolean_NA", {
    expect_equal(x, db["boolean_NA"])
})

x <- sample(c(0, 1, 10000, .Machine$integer.max, NA), size, replace = T)
db["integer"] <- x
test_that("integer", {
    expect_equal(x, db["integer"])
})

x <- sample(c(-100, -50, 0, 50, 100, NA), size, replace = T)
db["double"] <- x
test_that("double", {
    expect_equal(x, db["double"])
})

x <- sample(LETTERS, size, replace = T)
db["char"] <- x
test_that("char", {
  expect_equal(as.factor(x), db["char"])
})

# Test if escape characters works
x <- c("a\n", "\tc\v\n", "d\a\vx\ry\f\tz")
db["escape_char"] <- x
test_that("escape_char", {
  expect_equal(as.factor(escape_char(x)), db["escape_char"])
})

x <- .POSIXct(runif(size) * unclass(Sys.time()))
db["POSIXct"] <- x
test_that("POSIXct", {
    expect_equal(as.character(x), as.character(db["POSIXct"]))
})

test_that("non-existing", {
    expect_error(db["non-existing"])
})

context("VARIABLE DOCUMENTATION")
#################################
x <- list(a = "text", b = list(c = 1, d = 2))
db["x"] <- doc(x)
test_that("get documentation", {
    expect_equal(list(x), db$get_doc("x"))
})

context("VARIABLE DIMENSIONS")
##############################
x <- sample(1:5, size, replace = T)
dims <- c(2012, "test")
db["x", dims] <- x
test_that("put/get variable with dimensions", {
  expect_error(db["non-existing", dims])
  expect_equal(x, db["x", dims])
  expect_true(file.exists(file.path(db$path, "x", "data", "d[2012][test].cdb.gz")))
})

x <- sample(1:5, size, replace = T)
dims <- NULL
db["x", dims] <- x

test_that("put/get variable with dims = NULL", {
  expect_equal(x, db["x", dims])
  expect_true(file.exists(file.path(db$path, "x", "data", "d.cdb.gz")))
})

test_that("non-existing dimensions", {
  expect_error(db["non-existing", dims])
})

context("DATASETS")
###################
x <- data.table(MASS::survey)

# In addition we change the column names
# to specifically test issue 49.
setnames(x, c("Wr.Hnd", "NW.Hnd", "Pulse"), c("var", "x", "z"))

db[, "survey"] <- x

# Order columns by name since the folders in the database
setcolorder(x, sort(names(x)))

test_that("get dataset", {
  expect_equal(x, db[, "survey"])
})

context("READ ONLY")
####################
db$read_only <- T
test_that("put variable", {
  expect_error({ db["read_only"] <- 1:10})
})
test_that("put docs", {
  expect_error({ db["read_only"] <- doc(a = 1, b = 2) })
})
db$read_only <- F

# CLEAN UP
unlink(path, recursive = T)
