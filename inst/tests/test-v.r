context("Vector tests")

v <- sample(c(0:1, NA), 100, replace = TRUE)

test_that("Write vector", {
  expect_error(put_v(v))
  expect_true(put_v(v, "test_variable"))
})

test_that("Read vector", {
  expect_error(get_v("wrong_dir"))
  w <- get_v("test_variable")
  attributes(w) <- NULL
  expect_identical(v, w)
})

system("rm -r test_variable")
