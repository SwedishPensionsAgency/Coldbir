# devtools::test(".")

path <- tempfile()
size <- 100

context("init database")
    db <- cdb(path)
    test_that("init cdb", {
        expect_equal(path, get_path(db))
    })

context("variable (integer)")
    x <- sample(c(0, 1, 10000, .Machine$integer.max, NA), size, replace = TRUE)
    db["integer"] <- x

    test_that("get variable", {
        expect_error(db["non-existing"])
        expect_equal(x, db["integer"])
    })

context("variable (double)")
    x <- sample(c(-100, -50, 0, 50, 100, NA), size, replace = TRUE)
    db["double"] <- x

    test_that("get variable", {
        expect_equal(x, db["double"])
    })

context("variable (POSIXct)")
    ts <- as.character(.POSIXct(runif(1e3) * unclass(Sys.time())))
    # ts <- as.character(as.POSIXct(runif(1e4) * unclass(Sys.time()), origin = "1970-01-01"))

    #attr(x, "tzone") <- "GMT"
    x <- as.POSIXct(ts)
    #x_chr <- as.character(x)
    db["POSIXct"] <- x
    test_that("get variable", {
        expect_equal(as.character(x), as.character(db["POSIXct"]))
    })

# Remove all temporary test files
system(sprintf("rm -r %s", path))
