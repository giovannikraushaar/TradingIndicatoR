# Giovanni Kraushaar, 2019-04-23

test_that("MA works with simple input", {
  p <- c( 20, 22, 24, 25, 23, 26, 28, 26, 29, 27, 28, 30, 27, 29, 28 )
  expect_equal( MA(p,10)[15], 27.8 )
})

test_that("MA works with all method", {
  p <- c( 20, 22, 24, 25, 23, 26, 28, 26, 29, 27, 28, 30, 27, 29, 28 )
  
  expect_length( MA(p, 10, method = 'SMA'), 15 )
  expect_length( MA(p, 10, method = 'WMA'), 15 )
  expect_length( MA(p, 10, 'WMA', weight = rep(.1, 10)), 15 )
  expect_length( MA(p, 10, method = 'EMA'), 15 )
})

test_that("First n-1 values are NA", {
  p <- c( 20, 22, 24, 25, 23, 26, 28, 26, 29, 27, 28, 30, 27, 29, 28 )
  
  n <- 5
  x <- all( is.na(MA(p,n,'WMA')[1:(n-1)]) )
  expect_equal(x,TRUE)
})

test_that("Multiple period output", {
  p <- c( 20, 22, 24, 25, 23, 26, 28, 26, 29, 27, 28, 30, 27, 29, 28 )
  
  x <- c(2,5,10)
  n <- length(x)
  out <- MA(p,x,'EMA')
  
  expect_true( is.list(out) )
  expect_length( out, n )
})

test_that("Negative periods", {
  p <- c( 20, 22, 24, 25, 23, 26, 28, 26, 29, 27, 28, 30, 27, 29, 28 )
  
  expect_error( MA(p, period = -1) )
})

test_that("xts input", {
  expect_true( xts::is.xts(MA(USGDP, 2, 'EMA')) )
  expect_equal( zoo::index(MA(USGDP, 2, 'EMA')), zoo::index(USGDP) )
})

test_that("quantmod output as input", {
  
  expect_length( MA(BAC$Close,10,'WMA'), nrow(BAC) ) 
  expect_length( MA(BAC[,4],10,'WMA'), nrow(BAC) ) 
})

test_that("bug 1, multiperiod xts", {
  x <- MA(BAC$Close, c(50,100,200))
  expect_true( is.list(x) )
})
  
  