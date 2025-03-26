p <- function(dane) {
  y_xo <- rowSums(dane)
  y_ox <- colSums(dane)
  
  n <- sum(dane)
  r <-nrow(dane)
  c <- ncol(dane)
  x <- 1
  for (i in 1:r) {
    for (j in 1:c) {
      y_ij <- dane[i,j]
      x <- ( ( y_xo[i]*y_ox[j] ) / (y_ij*n) )^y_ij * x
    }
  }
  g_2 <- -2 * log(x)
  return( 1-pchisq(g_2, (c-1)*(r-1)) )
}

dane <- matrix(c(20,22,13,7,24,38,28,18,80,104,81,54,82,125,113,92), nrow = 4, ncol = 4 )
dane
p(dane)