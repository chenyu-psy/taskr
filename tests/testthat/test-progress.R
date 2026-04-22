test_that("report_progress emits a tagged JSON progress line", {
  output <- capture.output(result <- taskr::report_progress(0.5, "halfway"))

  expect_null(result)
  expect_length(output, 1)
  expect_match(output, "^##TASKR_PROGRESS##")
  expect_match(output, "##/TASKR_PROGRESS##$")

  json_text <- sub("^##TASKR_PROGRESS##", "", output)
  json_text <- sub("##/TASKR_PROGRESS##$", "", json_text)
  payload <- jsonlite::fromJSON(json_text)

  expect_equal(payload$fraction, 0.5)
  expect_equal(payload$message, "halfway")
  expect_match(payload$ts, "Z$")
})

test_that("report_progress clamps fraction to the valid range", {
  low_output <- capture.output(taskr::report_progress(-0.2, "low"))
  high_output <- capture.output(taskr::report_progress(2, "high"))

  low_json <- sub("^##TASKR_PROGRESS##", "", low_output)
  low_json <- sub("##/TASKR_PROGRESS##$", "", low_json)
  high_json <- sub("^##TASKR_PROGRESS##", "", high_output)
  high_json <- sub("##/TASKR_PROGRESS##$", "", high_json)

  expect_equal(jsonlite::fromJSON(low_json)$fraction, 0)
  expect_equal(jsonlite::fromJSON(high_json)$fraction, 1)
})

test_that("report_progress validates inputs", {
  expect_error(taskr::report_progress(NA_real_), "single finite numeric value")
  expect_error(taskr::report_progress(c(0.1, 0.2)), "single finite numeric value")
  expect_error(taskr::report_progress(0.5, NA_character_), "single non-missing character string")
})
