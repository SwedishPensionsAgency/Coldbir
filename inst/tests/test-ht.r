context("Hash table tests")

df <- data.frame(key = c(1, 2, 3), value = c("a", "b", "c"))

test_that("Write hash table", {
  expect_error(put_ht(df$key, df$value))
  expect_true(put_ht(df$key, df$value, "test_variable"))
})

test_that("Read vector", {
  expect_error(get_ht("wrong_dir"))
  expect_equal(df, get_ht("test_variable"))
})

system("rm -r test_variable")