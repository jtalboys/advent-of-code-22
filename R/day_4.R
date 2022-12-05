
# DAY 4 -------------------------------------------------------------------

input <- readLines("data/day_4.txt")

split_assignments <- purrr::map(input, strsplit, ",")

purrr::map(split_assignments, function(x) {
  values <- purrr::map(x, strsplit, "-")
  
  a <- seq(values[[1]][[1]][1], values[[1]][[1]][2])
  
  b <- seq(values[[1]][[2]][1], values[[1]][[2]][2])
  
  if (all(a %in% b) || all(b %in% a)) {
    return(TRUE)
  } else {
    return(FALSE)
  }

}) |>  
  unlist() |> 
  sum()

# PART 2

# Do the same as above, but instead of seeing if all overlap, just see if any
# do
purrr::map(split_assignments, function(x) {
  values <- purrr::map(x, strsplit, "-")
  
  a <- seq(values[[1]][[1]][1], values[[1]][[1]][2])
  
  b <- seq(values[[1]][[2]][1], values[[1]][[2]][2])
  
  if (any(a %in% b) || any(b %in% a)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}) |>  
  unlist() |> 
  sum()
