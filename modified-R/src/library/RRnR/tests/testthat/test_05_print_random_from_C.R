library(RRnR)

test_that("call to R from C works when recording",
{
  func <- function()
  {
    RRnR:::print_random()
  }
  
  # reset test counter and seed
  RRnR:::reset_print_random(5330)

  # capture the print into a file
  sink("res1.log", split = FALSE)
  rec <- record(func())
  sink()

  # read the file
  con <- file("res1.log", "r")
  res1 <- readLines(con, n = 1)
  close(con)

  file.remove("res1.log")

  # compare the result with expected result for the given seed
  expect_equal(res1, "[1] 513662")
})

test_that("C code is not executed when replaying",
{
  func <- function()
  {
    RRnR:::print_random()
  }
  
  # reset test counter and seed
  RRnR:::reset_print_random(5330)

  # record the call and capture the output
  sink("res1.log", split = FALSE)
  rec <- record(func())
  sink()
  file.remove("res1.log")
  
  # read the C call counter
  cnt <- RRnR:::get_print_random_counter()
  expect_equal(cnt, 1)
  
  # replay the call and capture the output
  sink("res1.log", split = FALSE)
  replay(rec)
  sink()
  file.remove("res1.log")
  
  # read the C call counter again, it should not increase
  cnt <- RRnR:::get_print_random_counter()
  expect_equal(cnt, 1)
})

test_that("print_random gives the same result when replayed",
{
  func <- function()
  {
    RRnR:::print_random()
  }
  
  # reset test counter and seed
  RRnR:::reset_print_random(5330)

  sink("res1.log", split = FALSE)
  rec <- record(func())
  sink()

  sink("res2.log", split = FALSE)
  replay(rec)
  sink()

  con <- file("res1.log", "r")
  res1 <- readLines(con, n = 1)
  close(con)

  con <- file("res2.log", "r")
  res2 <- readLines(con, n = 1)
  close(con)

  file.remove("res1.log")
  file.remove("res2.log")

  # compare the result with expected result for the given seed
  expect_equal(res1, "[1] 513662")
  expect_equal(res1, res2)
})
