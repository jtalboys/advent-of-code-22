
# DAY 2 -------------------------------------------------------------------

# OPPONENT:
# A = ROCK
# B = PAPER
# C = SCISSORS

# ME:
# X = ROCK = 1
# Y = PAPER = 2
# Z = SCISSORS = 3

# LOSS = 0
# DRAW = 3
# WIN = 6

# CALCULATE TOTAL SCORE

input <- readLines("data/day_2.txt")

df <- data.frame(input = input) |> 
  tidyr::separate(input, into = c("opponent", "me"), sep = " ")

# Calculate the score based on some conditions
with_score <- dplyr::mutate(df, score = dplyr::case_when(
  me == "X" ~ 1,
  me == "Y" ~ 2,
  me == "Z" ~ 3
))

# Now add 0, 3 or 6 depending on a win, loss or draw

final_scores <- dplyr::mutate(with_score,
  score = dplyr::case_when(
    opponent == "A" & me == "X" ~ score + 3,
    opponent == "A" & me == "Y" ~ score + 6,
    opponent == "A" & me == "Z" ~ score,
    opponent == "B" & me == "X" ~ score,
    opponent == "B" & me == "Y" ~ score + 3,
    opponent == "B" & me == "Z" ~ score + 6,
    opponent == "C" & me == "X" ~ score + 6,
    opponent == "C" & me == "Y" ~ score,
    opponent == "C" & me == "Z" ~ score + 3
  )
)

# Get the total score
dplyr::summarise(final_scores,sum(score))


# PART 2!

# NOW:
# X = LOSE
# Y = DRAW
# Z = WIN

choices <- dplyr::mutate(df,
                    choice = dplyr::case_when(
                      opponent == "A" & me == "X" ~ "C",
                      opponent == "A" & me == "Y" ~ "A",
                      opponent == "A" & me == "Z" ~ "B",
                      opponent == "B" & me == "X" ~ "A",
                      opponent == "B" & me == "Y" ~ "B",
                      opponent == "B" & me == "Z" ~ "C",
                      opponent == "C" & me == "X" ~ "B",
                      opponent == "C" & me == "Y" ~ "C",
                      opponent == "C" & me == "Z" ~ "A"
                              )
)

# Now work out the scores
new_scores <- dplyr::mutate(choices,
                            score = dplyr::case_when(
                              me == "X" ~ 0,
                              me == "Y" ~ 3,
                              me == "Z" ~ 6
                            )) |> 
  dplyr::mutate(
    score = dplyr::case_when(
      choice == "A" ~ score + 1,
      choice == "B" ~ score + 2,
      choice == "C" ~ score + 3
    )
  )

# Get the score!
dplyr::summarise(new_scores, sum(score))
