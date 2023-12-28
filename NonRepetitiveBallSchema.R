# Non-repetitive ball schema
SBN <- function(n1, n2, N1, N2) {
  return ((choose(N1, n1) * choose(N2, n2)) / choose(N1 + N2, n1 + n2))
}

# Expected value of the non-repetitive ball schema
ESBN <- function(n, N) {
  if (length(n) != length(N)) {
    return("Incorrect")
  } else {
    x <- choose(N[1], n[1])
    n_total <- sum(n)
    for (i in 2:length(n)) {
      x <- x * choose(N[i], n[i])
    }
    return(x / choose(length(N), length(n)))
  }
}
