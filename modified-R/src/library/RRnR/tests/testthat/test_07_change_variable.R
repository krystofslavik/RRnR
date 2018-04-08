library(RRnR)

test_that("it is not affected by changes in the environment",
{
  var <- 1234
  
  func <- function()
  {
    var
  }
  
  rec <- record(func())
  var <- 5678
  res <- replay(rec)
  
  expect_equal(1234, res)
})
