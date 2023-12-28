# Poisson scheme
SP <- function(n, k, p) {
  x <- c(1 - p[1], p[1])
  
  length(x) <- n + 1
  
  for (index in 3 : (n + 1)) {
    x[index] = 0
  }
  
  for (index in 2 : n) {
    next_poly <- c(1 - p[index], p[index])
    
    length(next_poly) <- n + 1
    
    for (index2 in 3 : (n + 1)) {
      next_poly[index2] <- 0
    }
    
    x <- Re(fft(fft(x) * fft(next_poly), inverse = TRUE)) / (n + 1)
  }
  
  return (x[k + 1])
}
