library(RRnR)

test_that("file creates a file when recording",
{
  func <- function(fn)
  {
    f <- file(fn)
    writeLines("test", f)
    close(f)
  }
  
  if(file.exists("res1.log"))
    file.remove("res1.log")
  
  rec <- record(func("res1.log"))
  
  expect_equal(file.exists("res1.log"), TRUE)
  file.remove("res1.log")
})

test_that("file does not create a file when replaying",
{
  func <- function(fn)
  {
    f <- file(fn)
    writeLines("test", f)
    close(f)
  }
  
  rec <- record(func("res1.log"))
  file.remove("res1.log")
  
  replay(rec)
  
  expect_equal(file.exists("res1.log"), FALSE)
})

test_that("file DOES create a file when replaying with allow_connections = TRUE",
{
  func <- function(fn)
  {
    f <- file(fn)
    writeLines("test", f)
    close(f)
  }
  
  rec <- record(func("res1.log"), list(allow_connections = TRUE))
  file.remove("res1.log")
  
  replay(rec)
  
  expect_equal(file.exists("res1.log"), TRUE)
  file.remove("res1.log")
})

# write.dcf uses file and writeLines internally
test_that("write.dcf creates a file when recording",
{
  func <- function(fn)
  {
    write.dcf("test", fn)
  }
  
  if(file.exists("res1.log"))
    file.remove("res1.log")
  
  rec <- record(func("res1.log"))
  
  expect_equal(file.exists("res1.log"), TRUE)
  file.remove("res1.log")
})

test_that("write.dcf does not create a file when replaying",
{
  func <- function(fn)
  {
    write.dcf("test", fn)
  }
  
  rec <- record(func("res1.log"))
  file.remove("res1.log")
  
  replay(rec)
  
  expect_equal(file.exists("res1.log"), FALSE)
})

test_that("write.dcf DOES create a file when replaying with allow_connections = TRUE",
{
  func <- function(fn)
  {
    write.dcf("test", fn)
  }
  
  rec <- record(func("res1.log"), list(allow_connections = TRUE))
  file.remove("res1.log")
  
  replay(rec)
  
  expect_equal(file.exists("res1.log"), TRUE)
  file.remove("res1.log")
})

test_that("write.dcf writes expected content while being recorded",
{
  func <- function(x, fn)
  {
    write.dcf(x, fn)
  }
  
  # create a test vector
  x <- c(1, 2, 3)
  
  # save the vector using non-recorded write and read it
  write.dcf(x, "res1.log")
  orig <- read.dcf("res1.log")
  
  file.remove("res1.log")

  # save the vector while recording and read it
  rec <- record(func(x, "res1.log"))
  test <- read.dcf("res1.log")
  
  file.remove("res1.log")

  # verify that the contents are the same
  expect_equal(orig, test)
})

test_that("write.dcf writes expected content while being replayed with allow_connections = TRUE",
{
  func <- function(x, fn)
  {
    write.dcf(x, fn)
  }
  
  # create a test vector
  x <- c(1, 2, 3)
  
  # save the vector using non-recorded write and read it
  write.dcf(x, "res1.log")
  orig <- read.dcf("res1.log")
  
  file.remove("res1.log")

  # remove the file created when recording
  rec <- record(func(x, "res1.log"), list(allow_connections = TRUE))
  file.remove("res1.log")
  
  # save the vector while replaying and read it
  replay(rec)
  test <- read.dcf("res1.log")
  
  file.remove("res1.log")

  # verify that the contents are the same
  expect_equal(orig, test)
})

test_that("cat to stdout works during replay with different connection numbers",
{
  func <- function()
  {
    cat("test\n")
  }
  
  # increase capture.output's connection number by creating a dummy connection
  dummy <- textConnection(NULL, "w")
  capture.output(rec <- record(func()), file = "res1.log")
  
  # decrease capture.output's connection number by closing the dummy connection
  close(dummy)
  capture.output(replay(rec), file = "res2.log")
  
  a <- unname(md5sum("res1.log"))
  b <- unname(md5sum("res2.log"))
  
  expect_equal(a, b)
  
  file.remove("res1.log")
  file.remove("res2.log")
})
