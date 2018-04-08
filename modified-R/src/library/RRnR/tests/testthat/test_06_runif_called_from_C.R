library(RRnR)

test_that("runif called from C is recorded and replayed",
{
  func <- function(seed)
  {
    RRnR:::call_runif(seed)
  }
  
  sink("res1.log", split = FALSE)
  rec <- record(func(5330))
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
  expect_equal(res1, "[1] 0.6408431")
  expect_equal(res1, res2)
})
