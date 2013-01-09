# devtools::test(".")

path <- "temp"
size <- 100

context("init database")
    db <- cdb(path)
    test_that("init cdb", {
        expect_equal(path, get_path(db))
    })

context("variable (integer)")
    x <- sample(c(0, 1, 10000, .Machine$integer.max, NA), size, replace = TRUE)
    test_that("put variable", {
        expect_message(db["x"] <- x)
    })
    test_that("get variable", {
        expect_error(db["non-existing"])
        expect_equal(x, db["x"])
    })

context("variable (double)")
    x <- sample(c(-100, -50, 0, 50, 100, NA), size, replace = TRUE)
    test_that("put variable", {
        expect_message(db["x"] <- x)
    })
    test_that("get variable", {
        expect_error(db["non-existing"])
        expect_equal(x, db["x"])
    })

# Remove all temporary test files
system(sprintf("rm -r %s", path))
