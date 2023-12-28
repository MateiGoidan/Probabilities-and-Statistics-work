# Repetitive ball scheme
SBR <- function(n, k, p) {
  return (choose(n, k) * p ^ k * (1 - p) ^ (n - k))
}

# Expected value of the repetitive ball scheme
ESBR <- function(n, p) {
  if (length(n) != length(p)) {
    return("Incorrect")
  }
  else {
    fac_n = factorial(n[1])
    prob = p[1] ^ n[1]
    for (i in 2:length(n)) {
      fac_n = fac_n * factorial(n[i])
      prob = prob * p[i] ^ n[i]
    }
    return((factorial(sum(n)) / fac_n) * prob)
  }
}
