test_that("vector input", {
  x <- Bollinger(TWTR$Close)
  expect_true(is.data.frame(x))
  
  y <- Bollinger(TWTR$Close, period = c(10,20), n = 1)
  expect_true(is.list(y))
  
  z <- Bollinger(TWTR$Close, period = c(10,20), n = c(1,2))
  expect_true(is.list(z[[1]]))
})


test_that("xts input", {
  x <- Bollinger(BAC$Close)
  expect_true(xts::is.xts(x))
  expect_true(is.list( Bollinger(BAC$Close, n = c(1,2)) ))
  expect_true(xts::is.xts(Bollinger(BAC$Close,c(10,20))[[1]]))
})

