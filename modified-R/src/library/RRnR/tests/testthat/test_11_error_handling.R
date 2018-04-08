library(RRnR)

test_that("stop inside recorded function does not break the recording",
{
  func <- function()
  {
    stop("err")
  }
  
  # this might break the whole session if not handled properly
  capture.output(rec <- tryCatch(record(func()), error = function(e) { NULL }))
  
  expect_false(is.null(rec))
})

test_that("stop inside recorded function does not break the replaying",
{
  func <- function()
  {
    stop("err")
  }
  
  # this might break the whole session if not handled properly
  capture.output(rec <- record(func()))
  
  res <- TRUE
  capture.output(tryCatch(replay(rec), error = function(e) { res <<- FALSE }))
  
  expect_equal(res, TRUE)
})

test_that("stop inside recorded function does not change the output",
{
  func <- function()
  {
    print(runif(1))
    stop("err")
  }
  
  # this might break the whole session if not handled properly
  capture.output(rec <- record(func()), file = "res1.log")
  capture.output(replay(rec), file = "res2.log")
  
  con <- file("res1.log", "r")
  res1 <- readLines(con)
  close(con)

  con <- file("res2.log", "r")
  res2 <- readLines(con)
  close(con)
  
  file.remove("res1.log")
  file.remove("res2.log")
  
  expect_equal(res1, res2)
})

test_that("errors inside C code are captured and replayed",
{
  func <- function()
  {
    RRnR::throw_an_error()
  }
  
  # this might break the whole session if not handled properly
  capture.output(rec <- record(func()))
  capture.output(res <- replay(rec))
  
  expect_equal(rec$result[[1]], res[[1]])
})

test_that("errors inside C code called from a callback are captured and replayed",
{
  func <- function()
  {
    RRnR::call_throw_an_error()
  }
  
  # this might break the whole session if not handled properly
  capture.output(rec <- record(func()))
  capture.output(res <- replay(rec))
  
  expect_equal(rec$result[[1]], res[[1]])
})
