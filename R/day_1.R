
# DAY 1 -------------------------------------------------------------------

# PART 1

# Given a list of calories each elf has, where each elfs set of calories 
# are separated by a blank line, find the most calories carried by 
# any elf

input <- readLines("data/day_1.txt")

calories <- 0
for (i in seq_along(input)) {
  if (input[i] == "") {
    calories <- c(calories, 0)
  } else {
    calories[length(calories)] <-
      calories[length(calories)] + as.numeric(input[i])
  }
}

# Finding the max of the calories gives the answer
max(calories)

# PART 2

# Arrange to get the values in order and take the sum of the 3 greatest values
calories[order(calories, decreasing = TRUE)][1:3] |>  sum()
