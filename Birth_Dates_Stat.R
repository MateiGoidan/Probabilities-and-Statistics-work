# The birth dates of 25 students from group 243 are given.

# A) Determine the day of the week corresponding to the most common birth dates.

# B) Randomly choose a student.
# a) Calculate the probability that the birth date falls on a Thursday.
# b) Calculate the probability that the birth date falls on a Monday, knowing that Saturday is the most frequent day.
# c) Calculate the probability that the birth date falls on a Monday or Tuesday, knowing that neither Thursday nor Friday is possible.

# C) Two students are randomly selected.
# a) Calculate the probability that the birth dates fall on the same day of the week.
# b) Calculate the probability that the birth dates fall on consecutive days.
# c) Calculate the probability that the birth dates fall on different days, knowing that the first day is Monday.

set.seed(111)
start_date <- as.Date("2003-01-01")
end_date <- as.Date("2003-12-31")
date <- sample(seq(start_date, end_date, by = "days"), 25)

# A) Determine the day of the week corresponding to the most common birth dates
frequency <- table(weekdays(date))
result <- max(frequency)

# Extract the day of the week for a given value returned by the table function
name <- names(frequency[as.integer(readline("Day number: "))])
print(name)

# B) Randomly choose a student.
# a) Calculate the probability that the birth date falls on a Thursday.
prob1 <- 5 / 25
# Find a general variable solution
max(frequency["Thursday"]) / length(date)

# b) Calculate the probability that the birth date falls on a Monday, knowing that Saturday is the most frequent day
max(frequency["Monday"] / length(date)) # ??? Is this what is required ???

# c) Calculate the probability that the birth date falls on a Monday or Tuesday, knowing that neither Thursday nor Friday is possible
max((frequency["Monday"] + frequency["Tuesday"]) / (length(date) - frequency["Thursday"] - frequency["Friday"]))

# C) Two students are randomly selected
# a) Calculate the probability that the birth dates fall on the same day of the week
prob2 <- 0
for (day_freq in frequency) {
  # Calculate the probability that two students are born on the same day of the week for any of the 7 days and sum them up
  prob2 <- prob2 + (day_freq * (day_freq - 1)) / (length(date) * (length(date) - 1))
}
print(prob2)

# b) Calculate the probability that the birth dates fall on consecutive days
# Calculate for each day of the week the probability that the next student has their birthday the next day 
# (Used multiple if statements because I didn't know how to sort the days in the week)
# Create a table ordered by days of the week
frequency2 <- c(frequency["Monday"], frequency["Tuesday"], frequency["Wednesday"], frequency["Thursday"], frequency["Friday"], frequency["Saturday"], frequency["Sunday"])

prob3 <- 0
for (i in 0:(length(frequency2) - 1)) {
  prob3 <- prob3 + max((frequency2[i + 1] * frequency2[(i + 1) %% length(frequency2) + 1] / (length(date) * (length(date) - 1))))
}
print(prob3)

# c) Calculate the probability that the birth dates fall on different days, knowing that the first day is Monday
# P(Different days | Monday) = (25 - Monday count) / 24
prob4 <- max((length(date) - frequency["Monday"]) / (length(date) - 1))
print(prob4)
