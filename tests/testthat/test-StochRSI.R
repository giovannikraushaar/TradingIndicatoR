# Luca Sanfilippo, 2019-05-09

test_that("StochRSI works with simple input", {
  rsi <- c( 20, 22, 24, 25, 23, 26, 28, 26, 29, 27, 28, 30, 27, 29, 28 )
  expect_equal( StochRSI(rsi,10)[16], 0.50 )
})

test_that("First 2*(n-1) values are NA", {
  rsi <- c( 20, 22, 24, 25, 23, 26, 28, 26, 29, 27, 28, 30, 27, 29, 28 )
  
  n <- 5
  x <- all( is.na(StochRSI(rsi,n)[1:(2*(n-1))]) )
  expect_equal(x,TRUE)
})

test_that("Negative periods", {
  rsi <- c( 20, 22, 24, 25, 23, 26, 28, 26, 29, 27, 28, 30, 27, 29, 28 )
  expect_error( StochRSI(rsi, period = -1) )
})

test_that("input with a valid length", {
  rsi <- c( 20, 22, 24, 25, 23, 26, 28, 26, 29, 27, 28, 30)
  expect_error( StochRSI(rsi, period = 14) )
})

test_that("xts input", {
  expect_true( xts::is.xts( StochRSI(RSI(USGDP,14), 14) ) ) 
})

test_that("quantmod output as input", {
  expect_length( StochRSI(RSI(BAC$Close,10),10), nrow(BAC) ) 
})
