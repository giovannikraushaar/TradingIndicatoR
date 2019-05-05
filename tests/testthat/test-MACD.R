# Giovanni Kraushaar, 2019-04-26

test_that("MACD vector input", {
  x  <- TWTR$Close
  m  <- MACD(x)
  ms <- MACD(x, signal_line = TRUE )
  
  # without signal line
  expect_length( m, nrow(TWTR) )
  expect_true( is.na(m[25]) & !is.na(m[26]) )
  
  # with signal line
  expect_true( is.list(ms) )
  expect_true( !is.na(ms$MACD[26]) & is.na(ms[[2]][26]) )
})

test_that("MACD xts input", {
  x  <- BAC$Close
  m  <- MACD(x)
  ms <- MACD(x, signal_line = TRUE )
  
  # without signal line
  expect_length( m, nrow(BAC) )
  expect_equal( class(BAC), class(m) )
  expect_true( is.na(m[25]) & !is.na(m[26]) )
  
  # with signal line
  expect_true( xts::is.xts(ms) )
  expect_true( !is.na(ms$MACD[26]) & is.na(ms[[2]][26]) )
})

test_that("MACD invalid input", {
  expect_error( MACD(price = c(1,45,23,6,2,5), period = 50) )
})