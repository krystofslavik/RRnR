library(RRnR)

test_that("system function works while recording",
{
  func <- function()
  {
    system("date", intern = TRUE)
  }
  
  res1 <- func()
  rec <- record(func())
  res2 <- rec$result
  
  expect_equal(res1, res2)
})

test_that("system function does not execute during replaying",
{
  func <- function()
  {
    system("date > tmp.file", intern = TRUE)
  }
  
  capture.output(rec <- record(func()))
  file.remove("tmp.file")
  
  capture.output(replay(rec))
  
  expect_equal(file.exists("tmp.file"), FALSE)
})

test_that("system function outputs the same when replaying",
{
  func <- function()
  {
    system("date", intern = TRUE)
  }
  
  rec <- record(func())
  res1 <- rec$result
  res2 <- replay(rec)
  
  expect_equal(res1, res2)
})

test_that("Sys.time returns the original time",
{
  func <- function()
  {
    op <- options(digits.secs = 6)
    x <- Sys.time()
    options(op)
    
    as.character(x)
  }
  
  rec <- record(func())
  res1 <- rec$result
  res2 <- replay(rec)
  
  expect_equal(res1, res2)
})

test_that("download.file works during recording",
{
  func <- function(fn)
  {
    download.file("https://cran.r-project.org/", fn, quiet = TRUE)
  }
  
  func("res1.log")
  rec <- record(func("res2.log"))
  
  a <- unname(md5sum("res1.log"))
  b <- unname(md5sum("res2.log"))
  
  expect_equal(a, b)
  
  file.remove("res1.log")
  file.remove("res2.log")
})

test_that("download.file does not create a file when replaying",
{
  func <- function()
  {
    download.file("https://cran.r-project.org/", "tmp.file", quiet = TRUE)
  }
  
  rec <- record(func())
  file.remove("tmp.file")
  
  replay(rec)
  
  expect_equal(file.exists("tmp.file"), FALSE)
})
