# Luca Sanfilippo

test_that("OBV works with simple input", {
  p <- c( 20, 22, 24, 25, 23, 26, 28, 26, 29, 27, 28, 30, 27, 29, 28 )
  expect_equal( OBV(p,11)[11], 70 )
  expect_equal( OBV(p,10)[11], 70 )
})

test_that("First value is NA", {
  p <- c( 20, 22, 24, 25, 23, 26, 28, 26, 29, 27, 28, 30, 27, 29, 28 )
  n <- c(221, 241, 222, 283, 244, 265, 286, 267, 288, 279, 285, 304, 255, 234, 222)
  
  x <- all( is.na(OBV(p,n)[1]) )
  expect_equal(x,TRUE)
})

test_that("input with a valid length", {
  p <- c( 20, 22, 24, 25, 23, 26, 28, 26, 29, 27, 28, 30)
  n <- c(221, 241, 222, 283, 244, 265, 286, 267, 288, 279, 285)
  expect_error( OBV(p, n) )
})

test_that("quantmod output as input", {
  expect_length( OBV(BAC$Close,BAC$Volume), nrow(BAC) ) 
  expect_length( OBV(BAC[,4],BAC$Volume), nrow(BAC) ) 
})


