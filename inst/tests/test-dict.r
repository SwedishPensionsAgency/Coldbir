context("Dictionary tests")

df <- data.frame(key = c(1, 2, 3), value = c("a", "b", "c"))

test_that("Write dictionary", {
    expect_error(put_dict(df))
    expect_true(put_dict(df, "test_variable"))
})

test_that("Read dictionary", {
    expect_error(get_dict("wrong_dir"))
    expect_equal(df, get_dict("test_variable"))
})

test_that("Keys -> Values", {
    expect_equal(c("c", "a"), to_values(c(3, 1), "test_variable"))
})

system("rm -r test_variable")
