path <- tempfile()
size <- 1e3
db <- cdb(path, read_only = F)

context("INITIALIZE DATABASE")
##############################
test_that("initialize database", {
  expect_equal(path, db$path)
})

db[] <- MASS::survey
db2 <- cdb(path, read_only = F)

test_that("re-initialize database", {
  expect_equal(db[], db2[])
})

db$clean()
db2 <- NULL

context("DATABASE CONFIG")
##########################
db$put_config()
rds <- file.path(db$path, Coldbir:::.config_filename)
test_that("create config file in an empty database", {
  expect_true(file.exists(rds))
  expect_identical(readRDS(rds), list(
    read_only = F,
    db_version = NA_real_,
    n_row = NA_integer_
  ))
})
db$clean()

context("DATABASE VARIABLE LENGTH")
###################################
test_that("nrow when database is empty", {
  expect_true(is.na(db$db_nrow()))
})

db["x"] <- sample(1:5, size, replace = T)
test_that("nrow when database contains one variable without any dimentions", {
  expect_equal(db$db_nrow(), size)
})
db$clean()

db["x"] <- 1:3
test_that("throw error if assigning non-allowed variable length", {
  expect_error({db["x"] <- 1:2})
  expect_error({db["y"] <- 1:2})
})
db$clean()

context("VARIABLE TYPES")
#########################
x <- sample(c(T, F), size, replace = T)
db["x"] <- x
test_that("logical", {
  expect_equal(x, db["x"][[1]])
})
db$clean()

x <- sample(c(T, F, NA), size, replace = T)
db["x"] <- x
test_that("logical na", {
  expect_equal(x, db["x"][[1]])
})
db$clean()

x <- sample(c(0, 1, 10000, .Machine$integer.max, NA), size, replace = T)
db["x"] <- x
test_that("integer", {
  expect_equal(x, db["x"][[1]])
})
db$clean()

x <- sample(c(-100, -50, 0, 50, 100, NA), size, replace = T)
db["x"] <- x
test_that("double", {
  expect_equal(x, db["x"][[1]])
})
db$clean()

x <- sample(LETTERS, size, replace = T)
db["x"] <- x
test_that("character", {
  expect_equal(x, db["x"][[1]])
})
db$clean()

db["x"] <- x <- as.factor(c(NA, NA))
test_that("factor with only na", {
  expect_equal(x, db["x"][[1]])
})
db$clean()

db["x"] <- x <- as.character(c("a", NA, "b", NA))
test_that("character with na", {
  expect_equal(x, db["x"][[1]])
})
db$clean()

# Test if escape characters works for factor variables
# Only factor variables (lookup tables) are escaped,
# character variables aren't (as it's not necessary).
db["x"] <- x <- as.factor(c("a\n", "\tc\v\n", "d\a\vx\ry\f\tz"))
test_that("escape_char", {
  expect_equal(as.character(escape_char(x)), as.character(db["x"][[1]]))
})
db$clean()

db["x"] <- x <- .POSIXct(runif(size) * unclass(Sys.time()))
test_that("POSIXct", {
  expect_equal(as.character(x), as.character(db["x"][[1]]))
})
db$clean()

test_that("non-existing", {
  expect_true(is.null(db["non-existing"]))
})

context("VARIABLE DOCUMENTATION")
#################################
db["x"] <- doc(a = 1, b = "c")
test_that("add docs as parameters", {
  expect_equal(list(a = 1, b = "c"), db$get_doc("x"))
})
db$clean()

x <- list(a = "text", b = list(c = 1:3, d = 4), c = "åäö")
db["x"] <- doc(x)
test_that("add docs as a list", {
  expect_equal(x, db$get_doc("x"))
})
db$clean()

x <- list(b = 1, c = 2)
db["x"] <- doc(a = x)  # special case
test_that("add docs as one parameter that includes a list", {
  expect_equal(list(a = x), db$get_doc("x"))
})
db$clean()

context("VARIABLE DIMENSIONS")
##############################
dims <- c(2012, "a")
db["x", dims] <- x <- sample(1:5, size, replace = T)
test_that("put/get variable with dimensions", {
  expect_equal(x, db["x", dims][[1]])
  expect_true(file.exists(file.path(db$path, "x", "data", "d[2012][a].cdb.gz")))
})
db$clean()

dims <- NULL
db["x", dims] <- x <- sample(1:5, size, replace = T)
test_that("put/get variable with dims = NULL", {
  expect_equal(x, db["x", dims][[1]])
  expect_true(file.exists(file.path(db$path, "x", "data", "d.cdb.gz")))
})
db$clean()

test_that("non-existing dimensions", {
  expect_true(is.null(db["non-existing", dims]))
})

context("REPLACE NA")
#####################
db["x"] <- x <- c(T, F, NA, F, T)
test_that("logical replaces NA", {
  expect_equal(sum(x, na.rm = T), sum(db["x", na = F][[1]]))
})
db$clean()

db["x"] <- x <- c(1, NA, 3)
test_that("integer replaces NA", {
  expect_equal(1:3, db["x", na = 2][[1]])
})
db$clean()

db["x"] <- x <- c("a", NA)
test_that("character replaces NA", {
  expect_equal(as.factor(c("a", "b")), db["x", na = "b"][[1]])
})
db$clean()

context("DATASETS")
###################
x <- data.table(MASS::survey)[, list(
  Sex, Fold, Pulse, Clap, Exer, Smoke, Height, Age
)]

# In addition we change the column names
# to specifically test issue 49.
setnames(x, c("Smoke", "Height", "Age"), c("var", "x", "z"))

db[] <- x

setcolorder(x, sort(names(x)))

test_that("get dataset", {
  expect_equal(x, db[])
})

test_that("select multiple variables", {
  expect_equal(x[, list(Clap, Pulse)], db[c("Clap", "Pulse")])
})

db$clean()

x1 <- cdb(tempfile())
x2 <- cdb(tempfile())
x1[] <- MASS::survey
x1[, 2012] <- MASS::survey
x1[, c(2012,12)] <- MASS::survey
x2[] <- x1[]

test_that("Copy database", {
  expect_equal(x1[], x2[])
})

x1$clean()
x2$clean()

context("READ ONLY")
####################
db$read_only <- T

test_that("put variable", {
  expect_error({ db["x"] <- 1:10})
})

test_that("put docs", {
  expect_error({ db["x"] <- doc(a = 1, b = 2) })
})

test_that("put config", {
  expect_warning(db$put_config())
})

test_that("clean", {
  expect_error(db$clean())
})

db$read_only <- F
db$clean()

context("LOOKUP TABLES")
########################
db["x", "a"] <- a <- as.factor(c("b", "c", "c", "b"))
db["x", "b"] <- b <- as.factor(c("a", "b", NA, NA))
db["x", "c"] <- c <- as.factor(c("d", "c", NA, "c"))
db["x", "d"] <- d <- as.factor(rep("c", 4))
db["x", "e"] <- e <- as.factor(rep(as.character(NA), 4))

# Since it's the same variable but with several dimensions,
# it share the same lookup table, therefore we convert
# them to character before comparing.
test_that("Different lookup tables between dimensions", {
  expect_equal(as.character(a), as.character(db["x", "a"][[1]]))
  expect_equal(as.character(b), as.character(db["x", "b"][[1]]))
  expect_equal(as.character(c), as.character(db["x", "c"][[1]]))
  expect_equal(as.character(d), as.character(db["x", "d"][[1]]))
  expect_equal(as.character(e), as.character(db["x", "e"][[1]]))
})

db$clean()
