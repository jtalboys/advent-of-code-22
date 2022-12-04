
# DAY 3 -------------------------------------------------------------------

input <- readLines("data/day_3.txt")

# Split strings into 2 equal parts
# Find the common element
# Assign a score based on the common element
# Sum up the scores

# First let's map over the inputs, splitting them in 2

rucksacks <- purrr::map(input, function(x){
  
  # Get the length of the string
  input_length <- nchar(x)
  
  # Split the string into 2 at the halfway point
  c(substr(x, 1, input_length / 2),
    substr(x, (input_length / 2) + 1, input_length))
  
})

# Find the common element
common <- purrr::map_chr(rucksacks, function(x){
  
  characters <- strsplit(x, "")
  
  intersect(characters[[1]], characters[[2]])
  
})

# Get a named vector of the scores
priorities <- c(
  setNames(1:26, letters),
  setNames(27:52, LETTERS)
)

# Now convert the common elements to their scores
rucksack_priorities <- priorities[common]

# Now take the sum
sum(rucksack_priorities)


# PART 2

# Split the input into groups of 3
badges <- purrr::map_chr(1:(length(input)/3), function(x){
  
  intersect(intersect(strsplit(input[((x - 1) * 3) + 1], "")[[1]],
                      strsplit(input[((x - 1) * 3) + 2], "")[[1]]),
            strsplit(input[((x - 1) * 3) + 3], "")[[1]])
  
})

badge_priorities <- priorities[badges]

sum(badge_priorities)
