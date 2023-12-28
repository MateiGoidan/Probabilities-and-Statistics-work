# ASSIGNMENT: We have n balls numbered from 1 to n, which we randomly place in n available urns.
# What is the probability that the first urn contains k balls?

# Read input
n <- as.integer(readline("n = "))
k <- as.integer(readline("k = "))
N <- as.integer(readline("N = "))
prob5 <- max((N - 1) ^ (n - k) / N ^ n)
print(prob5)

# Q: Calculate the probability of getting the sequence TT when flipping a coin 5 times
omega5 <- tosscoin(5)
fav <- 0
for (row in 1 : nrow(omega5)) {
  for (column in 1 : (length(omega5) - 1)) {
    if (omega5[row, column] == omega5[row, (column + 1)] && omega5[row, column] == 'T') {
      fav <- fav + 1
      break
    }
  }
}
print(fav / nrow(omega5))

# Probability of getting at least one head in 3 coin flips
# Q: Comment on the following code alternatives: which one do you prefer and why?

omega3 <- tosscoin(3)
sum(omega3 == "H") / nrow(omega3) # Not recommended!
# OR
sum(omega3[,1] == "H" | omega3[,2] == "H" | omega3[,3] == "H") / nrow(omega3)
# Another option
sum(rowSums(omega3 == 'H') > 0) / nrow(omega3) # Preferred for readability

# Q: Determine the probability of getting the sequence HH in 7 coin flips
omega7 <- tosscoin(7)
fav <- 0
for (row in 1 : nrow(omega7)) {
  for (column in 1 : (length(omega7) - 1)) {
    if (omega7[row, column] == omega7[row, (column + 1)] && omega7[row, column] == 'H') {
      fav <- fav + 1
      break
    }
  }
}
print(fav / nrow(omega7))

# Q: Determine the probability that at least 3 out of 7 coin flips result in an even number
rolls <- rolldie(7)
fav <- 0
for (roll in 1 : nrow(rolls)) {
  count <- 0
  for (die in 1 : ncol(rolls)) {
    count <- count + (rolls[roll, die] %% 2 == 0)
    if (count == 3) {
      fav <- fav + 1
      break
    }
  }
}
print(fav / nrow(rolls))

# Assignment:
# 1) Using the 'prob' package, create the object 'coin5' containing all possible outcomes of flipping a coin 5 times.
#    Using dataframe selection, determine the following probabilities:
#    a) The appearance of the sequence HHTHH
#    b) The appearance of the sequence THHHT
#    c) The number of occurrences of "H" is greater than the number of occurrences of "T"

coin5 <- tosscoin(5)

# a)
prob1 <- sum((coin5[,1] == 'H') & (coin5[,2] == 'H') & (coin5[,3] == 'T') & (coin5[,4] == 'H') & (coin5[,5] == 'H')) / nrow(coin5)

# b)
prob2 <- sum((coin5[,1] == 'T') & (coin5[,2] == 'H') & (coin5[,3] == 'H') & (coin5[,4] == 'H') & (coin5[,5] == 'T')) / nrow(coin5)

# c)
prob3 <- sum(rowSums(coin5 == 'H') > rowSums(coin5 == 'T')) / nrow(coin5)

# Assignment:
# Calculate the probability:
# a) To draw one ball of each color (with/without replacement)
# b) To draw more red balls than blue balls
# c) To draw only green balls
# d) To draw the first red ball and the other two of the same color

Urn <- rep(c("Red","Green","Blue"),c(5,3,8))
samples <- urnsamples(Urn, 3, FALSE)

# a)
prob1 <- sum((rowSums(samples == "Red") == rowSums(samples == "Blue")) & (rowSums(samples == "Blue") == rowSums(samples == "Green"))) / nrow(samples)

# b)
prob2 <- sum(rowSums(samples == "Red") > rowSums(samples == "Blue")) / nrow(samples)

# c)
prob3 <- sum((rowSums(samples == "Red") == 0) & (rowSums(samples == "Blue") == 0)) / nrow(samples)

# d)
count <- 0
for (sample_index in 1 : nrow(samples)) {
  if (samples[sample_index, 1] == "Red" && samples[sample_index, 2] == samples[sample_index, 3]) {
    count <- count + 1
  }
}
print(count / nrow(samples))
