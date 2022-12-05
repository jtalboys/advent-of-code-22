
# DAY 5 -------------------------------------------------------------------

input <- readLines("data/day_5.txt")

moves <- input[11:length(input)]
stacks <- input[1:8]

# Create a data frame with the moves

# Convert everything not a number to a blank space
move_values <- gsub("\\D", " ", moves) |>  
  # Trim start and end whitespace
  stringr::str_trim() |>  
  # Split into either a number or empty string
  strsplit(" ")

# Create a data frame, with:
#   - the first element as the amount to move
#   - the 7th element as the stack to move from
#   - the 11th element as the stack to move to

moves_df <- purrr::map_dfr(move_values, function(x) {
  
  return(
    data.frame(
      amount = as.numeric(x[1]),
      from = as.numeric(x[7]),
      to = as.numeric(x[11])
    )
  )
  
})

# We know there's 9 stacks
# Also that the number of characters is always the same, with 1-3 the first stack
# 5-7 the second stack, 9-11 the third stack etc
# Actually we really only need the middle of those ranges!
stack1 <- NULL
stack2 <- NULL
stack3 <- NULL
stack4 <- NULL
stack5 <- NULL
stack6 <- NULL
stack7 <- NULL
stack8 <- NULL
stack9 <- NULL

for (x in stacks) {

  stack1 <- c(substr(x, 2, 2), stack1)
  stack2 <- c(substr(x, 6, 6), stack2)
  stack3 <- c(substr(x, 10, 10), stack3)
  stack4 <- c(substr(x, 14, 14), stack4)
  stack5 <- c(substr(x, 18, 18), stack5)
  stack6 <- c(substr(x, 22, 22), stack6)
  stack7 <- c(substr(x, 26, 26), stack7)
  stack8 <- c(substr(x, 30, 30), stack8)
  stack9 <- c(substr(x, 34, 34), stack9)

}

stacks <- matrix(
  c(stack1, stack2, stack3, stack4, stack5, stack6, stack7, stack8, stack9),
  nrow = 9, ncol = 8, byrow = TRUE
) |> 
  # Always have an empty column at the end
  cbind(matrix(rep(" ", 9 * (9*8)), nrow = 9))


# Now loop over the instructions
for (i in 1:nrow(moves_df)) {
  
  # Get the from stack
  from_stack <- stacks[moves_df$from[i],]
  
  # How many are we taking
  num_crates <- moves_df$amount[i]
  
  # Also get the to stack
  to_stack <- stacks[moves_df$to[i],]
  
  # Iterate over how many we're taking
  for (j in 1:num_crates){
    
    # Find the last value in the from column
    last_value_from <- min(which(from_stack == " ")) - 1
    
    # find the last value in the to column
    last_value_to <- min(which(to_stack == " "))
    
    # Convert the last_value_to to last_value_from
    to_stack[last_value_to] <- from_stack[last_value_from]
    
    # Also remove that crate from the from stack
    from_stack[last_value_from] <- " "
  }
  
  # Assign the stacks back to the overall matrix
  stacks[moves_df$from[i],] <- from_stack
  stacks[moves_df$to[i],] <- to_stack
}



# PART 2

# This time we move boxes in order, not one by one
# re-create stacks

stacks <- matrix(
  c(stack1, stack2, stack3, stack4, stack5, stack6, stack7, stack8, stack9),
  nrow = 9, ncol = 8, byrow = TRUE
) |> 
  # Always have an empty column at the end
  cbind(matrix(rep(" ", 9 * (9*8)), nrow = 9))


# Use a similar for loop to before
# Now loop over the instructions
for (i in 1:nrow(moves_df)) {
  
  # Get the from stack
  from_stack <- stacks[moves_df$from[i],]
  
  # How many are we taking
  num_crates <- moves_df$amount[i]
  
  # Also get the to stack
  to_stack <- stacks[moves_df$to[i],]
  
  # Find the last value in the from column
  last_value_from <- min(which(from_stack == " ")) - 1
  
  # find the last value in the to column
  last_value_to <- min(which(to_stack == " "))
  
  # Add the crates to the end of the to stack, retaining order
  to_stack[last_value_to:(last_value_to + (num_crates - 1))] <- from_stack[(last_value_from - (num_crates - 1)):last_value_from] 
  
  # Re-assign " " to crates just taken from stack
  from_stack[(last_value_from - (num_crates - 1)):last_value_from] <- " "
  
  
  # Assign the stacks back to the overall matrix
  stacks[moves_df$from[i],] <- from_stack
  stacks[moves_df$to[i],] <- to_stack
}
