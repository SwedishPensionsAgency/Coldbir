context("Key-value tests")

df <- data.frame(key = c(1, 2, 3), value = c("a", "b", "c"))

test_that("Write keys", {
  expect_error(put_keys(df))
  expect_true(put_keys(df, "test_variable"))
})

test_that("Read keys", {
  expect_error(get_keys("wrong_dir"))
  expect_equal(df, get_keys("test_variable"))
})

system("rm -r test_variable")