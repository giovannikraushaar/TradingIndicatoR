# checks wether an numeric value is an integer
# or if a vector/list of numeric values is a vector/list of integers.

is.numeric_integer <- function(x){
  
  if( length(x)>1 ){
    z <- sapply(x, is.numeric_integer)
    return( all(z) )
  }
  
  y <- real_equality( x, as.integer(x), tol=1e-15 )
  return(y)
}
