library(RRnR)

test_that("it records and replays runif",
{
  res1 <- 0
  
  func <- function()
  {
    res1 <<- runif(1)
  }
  
  rec <- record(func())
  res2 <- replay(rec)
  
  expect_equal(res1, res2)
})
