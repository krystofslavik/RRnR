library(RRnR)

test_that("rprintf is captured and replayed",
{
  func <- function()
  {
    RRnR:::test_rprintf_capture()
  }
  
  capture.output(rec <- record(func()), file = "res1.log")
  capture.output(replay(rec), file = "res2.log")
  
  a <- unname(md5sum("res1.log"))
  b <- unname(md5sum("res2.log"))
  
  expect_equal(a, b)
  
  file.remove("res1.log")
  file.remove("res2.log")
})

test_that("rprintf is NOT captured and replayed with allow_prints = FALSE",
{
  func <- function()
  {
    RRnR:::test_rprintf_capture()
  }
  
  capture.output(rec <- record(func(), list(allow_prints = FALSE)), file = "res1.log")
  capture.output(replay(rec), file = "res2.log")
  
  expect_equal(file.info("res2.log")$size, 0)
  
  file.remove("res1.log")
  file.remove("res2.log")
})
