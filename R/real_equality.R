# real_equality checks whether two numbers are equal within precision limit
real_equality <- function(a, b, tol=1e-12){
  abs(a-b) / abs(a+b) < tol
}
