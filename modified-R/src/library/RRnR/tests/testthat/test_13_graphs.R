library(RRnR)
library(tools)

test_that("graph can be saved to a PNG file during recording",
{
  func <- function(f)
  {
    png(f)
    plot(sin, -pi, pi)
    dev.off()
  }
  
  func("res1.png")
  record(func("res2.png"))
  
  a <- unname(md5sum("res1.png"))
  b <- unname(md5sum("res2.png"))
  
  expect_equal(a, b)
  
  file.remove("res1.png")
  file.remove("res2.png")
})

test_that("graph is not saved to a PNG file during replaying",
{
  func <- function(f)
  {
    png(f)
    plot(sin, -pi, pi)
    dev.off()
  }
  
  rec <- record(func("res1.png"))
  file.remove("res1.png")
  
  replay(rec)
  
  expect_equal(file.exists("res1.png"), FALSE)
})

test_that("graph is saved to a PNG file opened outside of the replayed function",
{
  func <- function(f)
  {
    plot(sin, -pi, pi)
  }
  
  png("res1.png")
  rec <- record(func())
  dev.off()
  
  png("res2.png")
  replay(rec)
  dev.off()
  
  a <- unname(md5sum("res1.png"))
  b <- unname(md5sum("res2.png"))
  
  expect_equal(a, b)
  
  file.remove("res1.png")
  file.remove("res2.png")
})

test_that("graph can be saved to a PDF file during recording",
{
  func <- function(f)
  {
    pdf(f)
    plot(sin, -pi, pi)
    dev.off()
  }
  
  func("res1.pdf")
  record(func("res2.pdf"))
  
  a <- unname(md5sum("res1.pdf"))
  b <- unname(md5sum("res2.pdf"))
  
  expect_equal(a, b)
  
  file.remove("res1.pdf")
  file.remove("res2.pdf")
})

test_that("graph is not saved to a PDF file during replaying",
{
  func <- function(f)
  {
    pdf(f)
    plot(sin, -pi, pi)
    dev.off()
  }
  
  rec <- record(func("res1.pdf"))
  file.remove("res1.pdf")
  
  replay(rec)
  
  expect_equal(file.exists("res1.pdf"), FALSE)
})

test_that("graph is saved to a PDF file opened outside of the replayed function",
{
  func <- function(f)
  {
    plot(sin, -pi, pi)
  }
  
  pdf("res1.pdf")
  rec <- record(func())
  dev.off()
  
  pdf("res2.pdf")
  replay(rec)
  dev.off()
  
  a <- unname(md5sum("res1.pdf"))
  b <- unname(md5sum("res2.pdf"))
  
  expect_equal(a, b)
  
  file.remove("res1.pdf")
  file.remove("res2.pdf")
})

test_that("graph can be saved to a PNG file during recording with allow_graphs = FALSE",
{
  func <- function(f)
  {
    png(f)
    plot(sin, -pi, pi)
    dev.off()
  }
  
  func("res1.png")
  record(func("res2.png"), list(allow_graphs = FALSE))
  
  a <- unname(md5sum("res1.png"))
  b <- unname(md5sum("res2.png"))
  
  expect_equal(a, b)
  
  file.remove("res1.png")
  file.remove("res2.png")
})

test_that("graph is not saved to a PNG file during replaying with allow_graphs = FALSE",
{
  func <- function(f)
  {
    png(f)
    plot(sin, -pi, pi)
    dev.off()
  }
  
  rec <- record(func("res1.png"), list(allow_graphs = FALSE))
  file.remove("res1.png")
  
  replay(rec)
  
  expect_equal(file.exists("res1.png"), FALSE)
})

test_that("graph is NOT saved to a PNG file opened outside of the replayed function with allow_graphs = FALSE",
{
  func <- function(f)
  {
    plot(sin, -pi, pi)
  }
  
  png("res1.png")
  rec <- record(func(), list(allow_graphs = FALSE))
  dev.off()
  file.remove("res1.png")
  
  png("res2.png")
  replay(rec)
  dev.off()
  
  expect_equal(file.exists("res2.png"), FALSE)
})
