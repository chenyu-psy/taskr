test_that("hello_taskr returns expected greeting", {
  msg <- hello_taskr("lab")
  expect_equal(msg, "Hello, lab! Welcome to taskr.")
})

test_that("hello_taskr validates input", {
  expect_error(hello_taskr(123), "must be a single non-missing character")
  expect_error(hello_taskr(c("a", "b")), "must be a single non-missing character")
  expect_error(hello_taskr(NA_character_), "must be a single non-missing character")
})
