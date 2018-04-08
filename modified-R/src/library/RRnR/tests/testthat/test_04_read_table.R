library(RRnR)

test_that("read.table reads a file when being recorded",
{
  func <- function()
  {
    read.table("test.file")
  }

  # create a vector and save it to a file
  x <- c(1,2,3)
  write.table(x, "test.file")

  # read the file without recording
  orig <- read.table("test.file")

  # read the file with recording
  rec <- record(func())
  
  file.remove("test.file")

  # compare the read contents
  expect_equal(orig, rec$result)
})

test_that("read.table reads a removed file when being replayed",
{
  func <- function()
  {
    read.table("test.file")
  }

  # create a vector and save it to a file
  x <- c(1,2,3)
  write.table(x, "test.file")

  # read the file without recording
  orig <- read.table("test.file")

  # read the file with recording
  rec <- record(func())
  
  # remove the file
  file.remove("test.file")
  
  # replay the read
  test <- replay(rec)

  # compare the read contents
  expect_equal(orig, test)
})

test_that("read.table still reads the original contents of a modified file when being replayed",
{
  func <- function()
  {
    read.table("test.file")
  }

  # create a vector and save it to file
  x <- c(1,2,3)
  write.table(x, "test.file")

  # read the original file
  r1 <- read.table("test.file")

  # record the read
  rec <- record(func())

  # modify the file
  x <- c(5,6,7)
  write.table(x, "test.file")

  # read the modified file
  r2 <- read.table("test.file")

  # replay the original read, it should return the unchanged table
  r3 <- replay(rec)

  file.remove("test.file")

  # the replayed read should return the original table
  expect_equal(r1, r3)

  # the replayed read should not return the changed table
  expect_false(isTRUE(all.equal(r2, r3)))
})

# read.csv uses read.table internally
test_that("read.csv reads a removed file when being replayed",
{
  func <- function()
  {
    read.csv("test.file")
  }

  # create a vector and save it to a file
  x <- c(1,2,3)
  write.csv(x, "test.file")

  # read the file without recording
  orig <- read.csv("test.file")

  # read the file with recording
  rec <- record(func())
  
  # remove the file
  file.remove("test.file")
  
  # replay the read
  test <- replay(rec)

  # compare the read contents
  expect_equal(orig, test)
})

test_that("read.csv still reads the original contents of a modified file when being replayed",
{
  func <- function()
  {
    read.csv("test.file")
  }

  # create a vector and save it to file
  x <- c(1,2,3)
  write.csv(x, "test.file")

  # read the original file
  r1 <- read.csv("test.file")

  # record the read
  rec <- record(func())

  # modify the file
  x <- c(5,6,7)
  write.csv(x, "test.file")

  # read the modified file
  r2 <- read.csv("test.file")

  # replay the original read, it should return the unchanged table
  r3 <- replay(rec)

  file.remove("test.file")

  # the replayed read should return the original table
  expect_equal(r1, r3)

  # the replayed read should not return the changed table
  expect_false(isTRUE(all.equal(r2, r3)))
})
