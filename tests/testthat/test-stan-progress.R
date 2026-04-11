test_that("extract_stan_progress_rows parses cmdstan lines and keeps latest per chain", {
  text <- paste(
    "Chain 1 Iteration: 100 / 1000 [ 10%] (Warmup)",
    "Chain 2 Iteration: 200 / 1000 [ 20%] (Warmup)",
    "Chain 1 Iteration: 300 / 1000 [ 30%] (Sampling)",
    sep = "\n"
  )

  tab <- taskr:::extract_stan_progress_rows(text)

  expect_equal(nrow(tab), 2)
  expect_identical(tab$chain, c(1L, 2L))
  expect_equal(round(tab$progress, 2), c(0.30, 0.20))
  expect_identical(tab$phase[1], "Sampling")
})

test_that("extract_stan_progress_rows parses rstan fallback lines", {
  text <- paste(
    "Chain 1: 40% warmup",
    "Chain 2: 75% sampling",
    sep = "\n"
  )

  tab <- taskr:::extract_stan_progress_rows(text)
  expect_equal(nrow(tab), 2)
  expect_identical(tab$chain, c(1L, 2L))
  expect_equal(round(tab$progress, 2), c(0.40, 0.75))
})

test_that("extract_stan_progress_rows parses colon style chain iteration lines", {
  text <- paste(
    "Chain 1: Iteration: 10 / 100 [ 10%] (Warmup)",
    "Chain 2: Iteration: 60 / 100 [ 60%] (Sampling)",
    sep = "\n"
  )

  tab <- taskr:::extract_stan_progress_rows(text)
  expect_equal(nrow(tab), 2)
  expect_identical(tab$chain, c(1L, 2L))
  expect_equal(round(tab$progress, 2), c(0.10, 0.60))
  expect_identical(tab$phase, c("Warmup", "Sampling"))
})

test_that("extract_stan_progress_rows parses single-chain iteration lines", {
  text <- paste(
    "Iteration: 100 / 1000 [ 10%] (Warmup)",
    "Iteration: 900 / 1000 [ 90%] (Sampling)",
    sep = "\n"
  )

  tab <- taskr:::extract_stan_progress_rows(text)
  expect_equal(nrow(tab), 1)
  expect_identical(tab$chain, 1L)
  expect_equal(round(tab$progress, 2), 0.90)
  expect_identical(tab$phase, "Sampling")
})

test_that("extract_stan_progress_rows parses print-wrapped and prefixed lines", {
  text <- paste(
    '[1] "Chain 1 Iteration: 300 / 1000 [ 30%] (Warmup)"',
    "INFO Chain 2: Iteration: 800 / 1000 [ 80%] (Sampling)",
    sep = "\n"
  )

  tab <- taskr:::extract_stan_progress_rows(text)
  expect_equal(nrow(tab), 2)
  expect_identical(tab$chain, c(1L, 2L))
  expect_equal(round(tab$progress, 2), c(0.30, 0.80))
})

test_that("extract_stan_progress_rows handles carriage-return and ANSI logs", {
  text <- paste0(
    "\u001b[32mChain 1 Iteration: 250 / 1000 [ 25%] (Warmup)\u001b[39m\r",
    "Chain 2 Iteration: 700 / 1000 [ 70%] (Sampling)\r"
  )

  tab <- taskr:::extract_stan_progress_rows(text)
  expect_equal(nrow(tab), 2)
  expect_identical(tab$chain, c(1L, 2L))
  expect_equal(round(tab$progress, 2), c(0.25, 0.70))
})

test_that("extract_stan_progress_rows parses jags text progress bars", {
  text <- paste(
    "|+++++++++++++                     |  40%",
    "|+++++++++++++++++++++++++++++++   |  95%",
    sep = "\n"
  )

  tab <- taskr:::extract_stan_progress_rows(text)
  expect_equal(nrow(tab), 1)
  expect_identical(tab$chain, 1L)
  expect_equal(round(tab$progress, 2), 0.95)
})
