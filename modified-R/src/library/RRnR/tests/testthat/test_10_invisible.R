library(RRnR)

test_that("replay does not return when replaying invisible function",
{
  func <- function()
  {
    invisible(5)
  }
  
  rec <- record(func())
  res <- withVisible(replay(rec))
  
  expect_equal(res$visible, FALSE)
})

test_that("replay does return when replaying visible function",
{
  func <- function()
  {
    return(5)
  }
  
  rec <- record(func())
  res <- withVisible(replay(rec))
  
  expect_equal(res$visible, TRUE)
})
