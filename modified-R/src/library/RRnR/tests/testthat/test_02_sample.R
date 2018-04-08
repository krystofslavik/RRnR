library(RRnR)

test_that("it records and replays sample",
{
  res1 <- 0
  
  func <- function()
  {
    res1 <<- sample(10, 1)
  }
  
  rec <- record(func())
  res2 <- replay(rec)
  
  expect_equal(res1, res2)
})
