library(RRnR)
library(tools)

test_that("cat to stdout works even during replay",
{
  func <- function()
  {
    cat("test\n")
  }
  
  capture.output(rec <- record(func()), file = "res1.log")
  capture.output(replay(rec), file = "res2.log")
  
  a <- unname(md5sum("res1.log"))
  b <- unname(md5sum("res2.log"))
  
  expect_equal(a, b)
  
  file.remove("res1.log")
  file.remove("res2.log")
})

test_that("cat to stdout does NOT work during replay with allow_prints = FALSE",
{
  func <- function()
  {
    cat("test\n")
  }
  
  capture.output(rec <- record(func(), list(allow_prints = FALSE)), file = "res1.log")
  capture.output(replay(rec), file = "res2.log")
  
  expect_equal(file.info("res2.log")$size, 0)
  
  file.remove("res1.log")
  file.remove("res2.log")
})

test_that("cat to a file does not create the file during replay",
{
  func <- function()
  {
    cat("test\n", file = "tmp.log")
  }
  
  rec <- record(func())
  expect_equal(TRUE, file.exists("tmp.log"))
  file.remove("tmp.log")
  
  replay(rec)
  expect_equal(FALSE, file.exists("tmp.log"))
})

test_that("dump to a file does not create the file during replay",
{
  func <- function()
  {
    tst <- c(1, 2, 3)
    dump(c("tst"), file="tmp.log")
  }
  
  rec <- record(func())
  expect_equal(TRUE, file.exists("tmp.log"))
  file.remove("tmp.log")
  
  replay(rec)
  expect_equal(FALSE, file.exists("tmp.log"))
})

test_that("cat to a file does not create the file during replay with allow_prints = FALSE",
{
  func <- function()
  {
    cat("test\n", file = "tmp.log")
  }
  
  rec <- record(func(), list(allow_prints = FALSE))
  expect_equal(TRUE, file.exists("tmp.log"))
  file.remove("tmp.log")
  
  replay(rec)
  expect_equal(FALSE, file.exists("tmp.log"))
})

test_that("cat to a file DOES create the file during replay with allow_connections = TRUE",
{
  func <- function()
  {
    cat("test\n", file = "tmp.log")
  }
  
  rec <- record(func(), list(allow_connections = TRUE))
  expect_equal(TRUE, file.exists("tmp.log"))
  file.remove("tmp.log")
  
  replay(rec)
  expect_equal(TRUE, file.exists("tmp.log"))
  file.remove("tmp.log")
})

test_that("cat to a file is not replayed to stdout",
{
  func <- function()
  {
    cat("test\n", file = "tmp.log")
  }
  
  capture.output(rec <- record(func()), file = "res1.log")
  capture.output(replay(rec), file = "res2.log")
  
  expect_equal(file.info("res2.log")$size, 0)
  a <- unname(md5sum("res1.log"))
  b <- unname(md5sum("res2.log"))
  
  expect_equal(a, b)
  
  file.remove("tmp.log")
  file.remove("res1.log")
  file.remove("res2.log")
})

test_that("dump to a file is not replayed to stdout",
{
  func <- function()
  {
    tst <- c(1, 2, 3)
    dump(c("tst"), file="tmp.log")
  }
  
  capture.output(rec <- record(func()), file = "res1.log")
  capture.output(replay(rec), file = "res2.log")
  
  expect_equal(file.info("res2.log")$size, 0)
  a <- unname(md5sum("res1.log"))
  b <- unname(md5sum("res2.log"))
  
  expect_equal(a, b)
  
  file.remove("tmp.log")
  file.remove("res1.log")
  file.remove("res2.log")
})

test_that("cat to a file is not replayed to stdout with allow_prints = FALSE",
{
  func <- function()
  {
    cat("test\n", file = "tmp.log")
  }
  
  capture.output(rec <- record(func(), list(allow_prints = FALSE)), file = "res1.log")
  capture.output(replay(rec), file = "res2.log")
  
  a <- unname(md5sum("res1.log"))
  b <- unname(md5sum("res2.log"))
  
  expect_equal(file.info("res2.log")$size, 0)
  expect_equal(a, b)
  
  file.remove("tmp.log")
  file.remove("res1.log")
  file.remove("res2.log")
})

test_that("dump to a file is not replayed to stdout with allow_prints = FALSE",
{
  func <- function()
  {
    tst <- c(1, 2, 3)
    dump(c("tst"), file="tmp.log")
  }
  
  capture.output(rec <- record(func(), list(allow_prints = FALSE)), file = "res1.log")
  capture.output(replay(rec), file = "res2.log")
  
  a <- unname(md5sum("res1.log"))
  b <- unname(md5sum("res2.log"))
  
  expect_equal(file.info("res2.log")$size, 0)
  expect_equal(a, b)
  
  file.remove("tmp.log")
  file.remove("res1.log")
  file.remove("res2.log")
})

test_that("sink to a file does not create the file during replay",
{
  func <- function()
  {
    sink("tmp.log")
    cat("test\n")
    sink(NULL)
  }
  
  rec <- record(func())
  expect_equal(TRUE, file.exists("tmp.log"))
  file.remove("tmp.log")
  
  replay(rec)
  expect_equal(FALSE, file.exists("tmp.log"))
})

test_that("sink to a file does not create the file during replay with allow_prints = FALSE",
{
  func <- function()
  {
    sink("tmp.log")
    cat("test\n")
    sink(NULL)
  }
  
  rec <- record(func(), list(allow_prints = FALSE))
  expect_equal(TRUE, file.exists("tmp.log"))
  file.remove("tmp.log")
  
  replay(rec)
  expect_equal(FALSE, file.exists("tmp.log"))
})

test_that("sink to a file DOES create the file during replay with allow_connections = TRUE",
{
  func <- function()
  {
    sink("tmp.log")
    cat("test\n")
    sink(NULL)
  }
  
  rec <- record(func(), list(allow_connections = TRUE))
  expect_equal(TRUE, file.exists("tmp.log"))
  file.remove("tmp.log")
  
  replay(rec)
  expect_equal(TRUE, file.exists("tmp.log"))
  file.remove("tmp.log")
})

test_that("cat sinked to a file is not replayed to stdout",
{
  func <- function()
  {
    sink("tmp.log")
    cat("test\n")
    sink(NULL)
  }
  
  capture.output(rec <- record(func()), file = "res1.log")
  capture.output(replay(rec), file = "res2.log")
  
  expect_equal(file.info("res2.log")$size, 0)
  a <- unname(md5sum("res1.log"))
  b <- unname(md5sum("res2.log"))
  
  expect_equal(a, b)
  
  file.remove("tmp.log")
  file.remove("res1.log")
  file.remove("res2.log")
})

test_that("cat sinked to a file is not replayed to stdout with allow_prints = FALSE",
{
  func <- function()
  {
    sink("tmp.log")
    cat("test\n")
    sink(NULL)
  }
  
  capture.output(rec <- record(func()), file = "res1.log")
  capture.output(replay(rec), file = "res2.log")
  
  expect_equal(file.info("res2.log")$size, 0)
  a <- unname(md5sum("res1.log"))
  b <- unname(md5sum("res2.log"))
  
  expect_equal(a, b)
  
  file.remove("tmp.log")
  file.remove("res1.log")
  file.remove("res2.log")
})

test_that("writeLines sinked to a file is not replayed to stdout",
{
  func <- function()
  {
    sink("tmp.log")
    writeLines("test")
    sink(NULL)
  }
  
  capture.output(rec <- record(func()), file = "res1.log")
  capture.output(replay(rec), file = "res2.log")
  
  expect_equal(file.info("res2.log")$size, 0)
  a <- unname(md5sum("res1.log"))
  b <- unname(md5sum("res2.log"))
  
  expect_equal(a, b)
  
  file.remove("tmp.log")
  file.remove("res1.log")
  file.remove("res2.log")
})

test_that("writeLines sinked to a file is not replayed to stdout with allow_prints = FALSE",
{
  func <- function()
  {
    sink("tmp.log")
    writeLines("test")
    sink(NULL)
  }
  
  capture.output(rec <- record(func()), file = "res1.log")
  capture.output(replay(rec), file = "res2.log")
  
  expect_equal(file.info("res2.log")$size, 0)
  a <- unname(md5sum("res1.log"))
  b <- unname(md5sum("res2.log"))
  
  expect_equal(a, b)
  
  file.remove("tmp.log")
  file.remove("res1.log")
  file.remove("res2.log")
})

test_that("dump sinked to a file is not replayed to stdout",
{
  func <- function()
  {
    sink("tmp.log")
    tst <- c(1, 2, 3)
    dump(c("tst"), file="")
    sink(NULL)
  }
  
  capture.output(rec <- record(func()), file = "res1.log")
  capture.output(replay(rec), file = "res2.log")
  
  expect_equal(file.info("res2.log")$size, 0)
  a <- unname(md5sum("res1.log"))
  b <- unname(md5sum("res2.log"))
  
  expect_equal(a, b)
  
  file.remove("tmp.log")
  file.remove("res1.log")
  file.remove("res2.log")
})

test_that("dump sinked to a file is not replayed to stdout with allow_prints = FALSE",
{
  func <- function()
  {
    sink("tmp.log")
    tst <- c(1, 2, 3)
    dump(c("tst"), file="")
    sink(NULL)
  }
  
  capture.output(rec <- record(func()), file = "res1.log")
  capture.output(replay(rec), file = "res2.log")
  
  expect_equal(file.info("res2.log")$size, 0)
  a <- unname(md5sum("res1.log"))
  b <- unname(md5sum("res2.log"))
  
  expect_equal(a, b)
  
  file.remove("tmp.log")
  file.remove("res1.log")
  file.remove("res2.log")
})

test_that("print of a string to stdout works even during replay",
{
  func <- function()
  {
    print("test")
  }
  
  capture.output(rec <- record(func()), file = "res1.log")
  capture.output(replay(rec), file = "res2.log")
  
  a <- unname(md5sum("res1.log"))
  b <- unname(md5sum("res2.log"))
  
  expect_equal(a, b)
  
  file.remove("res1.log")
  file.remove("res2.log")
})

test_that("print of a string sinked to a file is not replayed to stdout",
{
  func <- function()
  {
    sink("tmp.log")
    print("test")
    sink(NULL)
  }
  
  capture.output(rec <- record(func()), file = "res1.log")
  capture.output(replay(rec), file = "res2.log")
  
  a <- unname(md5sum("res1.log"))
  b <- unname(md5sum("res2.log"))
  
  expect_equal(a, b)
  
  file.remove("tmp.log")
  file.remove("res1.log")
  file.remove("res2.log")
})

test_that("print of a function to stdout works even during replay",
{
  func <- function()
  {
    print(func)
  }
  
  capture.output(rec <- record(func()), file = "res1.log")
  capture.output(replay(rec), file = "res2.log")
  
  a <- unname(md5sum("res1.log"))
  b <- unname(md5sum("res2.log"))
  
  expect_equal(a, b)
  
  file.remove("res1.log")
  file.remove("res2.log")
})

test_that("print of a function sinked to a file is not replayed to stdout",
{
  func <- function()
  {
    sink("tmp.log")
    print(func)
    sink(NULL)
  }
  
  capture.output(rec <- record(func()), file = "res1.log")
  capture.output(replay(rec), file = "res2.log")
  
  a <- unname(md5sum("res1.log"))
  b <- unname(md5sum("res2.log"))
  
  expect_equal(a, b)
  
  file.remove("tmp.log")
  file.remove("res1.log")
  file.remove("res2.log")
})

test_that("print of a string to stdout does NOT work during replay with allow_prints = FALSE",
{
  func <- function()
  {
    print("test")
  }
  
  capture.output(rec <- record(func(), list(allow_prints = FALSE)), file = "res1.log")
  capture.output(replay(rec), file = "res2.log")
  
  expect_equal(file.info("res2.log")$size, 0)
  
  file.remove("res1.log")
  file.remove("res2.log")
})

test_that("print of a string sinked to a file is not replayed to stdout with allow_prints = FALSE",
{
  func <- function()
  {
    sink("tmp.log")
    print("test")
    sink(NULL)
  }
  
  capture.output(rec <- record(func(), list(allow_prints = FALSE)), file = "res1.log")
  capture.output(replay(rec), file = "res2.log")
  
  a <- unname(md5sum("res1.log"))
  b <- unname(md5sum("res2.log"))
  
  expect_equal(a, b)
  
  file.remove("tmp.log")
  file.remove("res1.log")
  file.remove("res2.log")
})
