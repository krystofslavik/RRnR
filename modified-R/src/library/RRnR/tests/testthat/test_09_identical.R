library(RRnR)

test_that("identity is preserved when assigning into environment",
{
  func <- function()
  {
    e <- new.env()
    v <- c(1, 2, 3)
    e$v <- v
    
    return(identical(v, e$v))
  }
  
  rec <- record(func())
  res <- replay(rec)
  
  expect_equal(res, TRUE)
})

test_that("identity is preserved when returning vector from C",
{
  func <- function()
  {
    v <- c(1, 2, 3)
    v2 <- RRnR:::simple_return(v)
    
    return(identical(v, v2))
  }
  
  rec <- record(func())
  res <- replay(rec)
  
  expect_equal(res, TRUE)
})

test_that("identity is preserved when returning env from C",
{
  func <- function()
  {
    e <- new.env()
    e2 <- RRnR:::simple_return(e)
    
    return(identical(e, e2))
  }
  
  rec <- record(func())
  res <- replay(rec)
  
  expect_equal(res, TRUE)
})
