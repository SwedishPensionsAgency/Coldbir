# devtools::test(".")

path <- tempfile()
size <- 100

context("init database")
    db <- cdb(path, log_level = 9)
    test_that("init cdb", {
        expect_equal(path, get_path(db))
    })

context("variable (integer)")
    x <- sample(c(0, 1, 10000, .Machine$integer.max, NA), size, replace = TRUE)
    db["x"] <- x
    test_that("get variable", {
        expect_error(db["non-existing"])
        expect_equal(x, db["x"])
    })

context("variable (double)")
    x <- sample(c(-100, -50, 0, 50, 100, NA), size, replace = TRUE)
    db["x"] <- x
    test_that("get variable", {
        expect_error(db["non-existing"])
        expect_equal(x, db["x"])
    })

context("documentation")
    x <- list(a = "text", b = list(c = 1, d = 2))
    db["x"] <- doc(x)
    test_that("get variable", {
        expect_error(db["non-existing"])
        expect_equal(list(x), get_doc(db, "x"))
    })

# Remove all temporary test files
system(sprintf("rm -r %s", path))
