library(RRnR)

test_that("new.env() returns empty env",
{
  # in replay new.env() always returns the same env
  # but we need to test if it is empty
  func <- function()
  {
    a <- new.env() # create new env
    res <- exists("foo", envir=a) # test if there is foo
    a$foo <- "bar" # insert foo
    
    return(res)
  }

  rec <- record(func())
  res <- replay(rec) # foo should not be in there after the replayed new.env()

  # compare the result
  expect_equal(res, FALSE)
})

test_that("new.env() result is replaced in the trace with new value",
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

test_that("environment() returns env with original contents",
{
  func <- function()
  {
    a <- environment()
    res <- exists("foo", envir=a) # test if there is foo
    a$foo <- "bar" # insert foo
    
    return(res)
  }

  rec <- record(func())
  res <- replay(rec) # foo should not be in there after the replayed environment()

  # compare the result
  expect_equal(res, FALSE)
})

test_that("environment() returns current environment",
{
  func <- function()
  {
    a <- environment()
    foo <- "bar"
    res <- exists("foo", envir=a) # a and current env should be identical
    
    return(res)
  }

  rec <- record(func())
  res <- replay(rec) # foo should be in there after the replayed environment()

  # compare the result
  expect_equal(res, TRUE)
})

test_that("environment() result is replaced in the trace with new value",
{
  func <- function()
  {
    e <- environment()
    e2 <- RRnR:::simple_return(e)
    
    return(identical(e, e2))
  }
  
  rec <- record(func())
  res <- replay(rec)
  
  expect_equal(res, TRUE)
})

test_that("parent.frame() returns env with original contents",
{
  func <- function()
  {
    f <- function()
    {
      a <- parent.frame()
      res <- exists("foo", envir=a) # test if there is foo
      a$foo <- "bar" # insert foo
      
      return(res)
    }
    
    f()
  }

  rec <- record(func())
  res <- replay(rec) # foo should not be in there after the replayed parent.frame()

  # compare the result
  expect_equal(res, FALSE)
})

test_that("parent.frame() result is replaced in the trace with new value",
{
  func <- function()
  {
    f <- function()
    {
      e <- parent.frame()
      e2 <- RRnR:::simple_return(e)
      
      return(identical(e, e2))
    }
    
    f()
  }
  
  rec <- record(func())
  res <- replay(rec)
  
  expect_equal(res, TRUE)
})
