library(RRnR)

test_that("write.table creates file while being recorded",
{
  func <- function(x)
  {
    write.table(x, "test.file")
  }
  
  # if there already is the test.file then remove it
  if(file.exists("test.file"))
    file.remove("test.file")

  # create a vector and save it to file while recording
  x <- c(1,2,3)
  rec <- record(func(x))

  # verify that the file has been created
  expect_equal(file.exists("test.file"), TRUE)
  
  file.remove("test.file")
})

test_that("write.table writes expected content while being recorded",
{
  func <- function(x)
  {
    write.table(x, "test.file")
  }
  
  # create a test vector
  x <- c(1, 2, 3)
  
  # save the vector using non-recorded write and read it
  write.table(x, "test.file")
  orig <- read.table("test.file")
  
  file.remove("test.file")

  # save the vector while recording and read it
  rec <- record(func(x))
  test <- read.table("test.file")
  
  file.remove("test.file")

  # verify that the contents are the same
  expect_equal(orig, test)
})

test_that("write.table does not create a file when being replayed",
{
  func <- function(x)
  {
    write.table(x, "test.file")
  }

  # create a vector and save it to file while recording
  x <- c(1,2,3)
  rec <- record(func(x))

  # remove the created file
  file.remove("test.file")
  
  # now replay
  replay(rec)
  
  # there should be no file
  expect_equal(file.exists("test.file"), FALSE)
})

test_that("write.table does not change contents of a file when being replayed",
{
  func <- function(x)
  {
    write.table(x, "test.file")
  }

  # create a vector and save it to file while recording
  x <- c(1,2,3)
  rec <- record(func(x))

  # verify that the file has been created
  r1 <- read.table("test.file")

  # modify the file
  x <- c(5,6,7)
  write.table(x, "test.file")

  # read the modified file
  r2 <- read.table("test.file")

  # replay the first write, the file should remain unchanged
  replay(rec)

  # read the file again, it should not be changed
  r3 <- read.table("test.file")

  file.remove("test.file")

  # the replayed write should not change the file
  expect_equal(r2, r3)

  # the file should contain the second vector
  expect_false(isTRUE(all.equal(r1, r2)))
})

# write.csv uses write.table internally
test_that("write.csv writes expected content while being recorded",
{
  func <- function(x)
  {
    write.csv(x, "test.file")
  }
  
  # create a test vector
  x <- c(1, 2, 3)
  
  # save the vector using non-recorded write and read it
  write.csv(x, "test.file")
  orig <- read.csv("test.file")
  
  file.remove("test.file")

  # save the vector while recording and read it
  rec <- record(func(x))
  test <- read.csv("test.file")
  
  file.remove("test.file")

  # verify that the contents are the same
  expect_equal(orig, test)
})

test_that("write.csv does not create a file when being replayed",
{
  func <- function(x)
  {
    write.csv(x, "test.file")
  }

  # create a vector and save it to file while recording
  x <- c(1,2,3)
  rec <- record(func(x))

  # remove the created file
  file.remove("test.file")
  
  # now replay
  replay(rec)
  
  # there should be no file
  expect_equal(file.exists("test.file"), FALSE)
})
