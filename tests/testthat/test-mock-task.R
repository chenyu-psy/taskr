test_that("MockTask satisfies core contract fields and methods", {
  task <- MockTask$new(id = "mock_001")

  expect_equal(task$id, "mock_001")
  expect_s3_class(task$created_at, "POSIXct")
  expect_equal(task$status(), "running")
  expect_true(task$is_alive())
  expect_true(is.function(task$read_output))
  expect_true(is.function(task$read_error))
  expect_true(is.function(task$progress))
  expect_true(is.function(task$kill))
  expect_true(is.function(task$elapsed))
})

test_that("MockTask supports incremental output and error reads", {
  task <- MockTask$new(
    id = "mock_002",
    output_queue = c("out1\n", "out2\n"),
    error_queue = c("err1\n")
  )

  expect_identical(task$read_output(), "out1\n")
  expect_identical(task$read_output(), "out2\n")
  expect_identical(task$read_output(), "")
  expect_identical(task$stdout_buffer, "out1\nout2\n")

  expect_identical(task$read_error(), "err1\n")
  expect_identical(task$read_error(), "")
  expect_identical(task$stderr_buffer, "err1\n")
})

test_that("MockTask supports progress and terminal transitions", {
  task <- MockTask$new(id = "mock_003")

  expect_null(task$progress())

  task$set_progress(0.5, "halfway")
  prog <- task$progress()
  expect_equal(prog$fraction, 0.5)
  expect_equal(prog$message, "halfway")
  expect_s3_class(prog$updated_at, "POSIXct")

  task$set_status("completed")
  expect_equal(task$status(), "completed")
  expect_false(task$is_alive())

  task$kill()
  expect_equal(task$status(), "cancelled")
  expect_false(task$is_alive())
  expect_type(task$elapsed(), "double")
})
