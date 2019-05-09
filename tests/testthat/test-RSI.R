# Luca Sanfilippo, 2019-05-09

test_that("RSI works with simple input", {
  p <- c( 20, 22, 24, 25, 23, 26, 28, 26, 29, 27, 28, 30, 27, 29, 28 )
  expect_equal( RSI(p,11)[11], 70 )
  expect_equal( RSI(p,10)[11], 70 )
})

test_that("First n-1 values are NA", {
  p <- c( 20, 22, 24, 25, 23, 26, 28, 26, 29, 27, 28, 30, 27, 29, 28 )
  
  n <- 5
  x <- all( is.na(RSI(p,n)[1:(n-1)]) )
  expect_equal(x,TRUE)
})

test_that("Negative periods", {
  p <- c( 20, 22, 24, 25, 23, 26, 28, 26, 29, 27, 28, 30, 27, 29, 28 )
  expect_error( RSI(p, period = -1) )
})

test_that("input with a valid length", {
  p <- c( 20, 22, 24, 25, 23, 26, 28, 26, 29, 27, 28, 30)
  expect_error( RSI(p, period = 14) )
})

test_that("xts input", {
  expect_true( xts::is.xts(RSI(USGDP, 2)) )
  expect_equal( zoo::index(RSI(USGDP, 2)), zoo::index(USGDP) )
})

test_that("quantmod output as input", {
  expect_length( RSI(BAC$Close,10), nrow(BAC) ) 
  expect_length( RSI(BAC[,4],10), nrow(BAC) ) 
})


