library(tidyverse)
library(purrr)

# Task 3------------------------------------------------------------------------

# 1) Filter collatz_df to retain starting integers that exhibit backtracking in their seq

has_backtracking <- function(seq) {
  if (length(seq) <= 2) {
    return(FALSE)
  }
  for (i in 2:(length(seq) - 1)) {
    if (seq[i] < seq[1] && seq[i + 1] > seq[1]) {
      return(TRUE)
    }
  }
  return(FALSE)  
}

backtracks_df <- collatz_df %>% 
  filter(sapply(seq, has_backtracking))

# 2) What is the most frequently occurring number of times they go above their starting integer?

count_backtrack <- function(seq) {
  sum(seq > seq[1])
}

mode_backtrack <- backtracks_df %>%
  mutate( backtrack_counts = map_int(seq, count_backtrack)) %>%
  count(backtrack_counts) %>%
  arrange(desc(n)) %>%
  pull(backtrack_counts) %>%
  first()

# 3) What is the maximum value reached after the first backtrack for these sequences? 

first_backtrack <- function(seq) {
  start <- seq[1]
  first <- start
  reached_backtrack <- FALSE
  
  for (i in seq) {
    if (reached_backtrack && i > first) {
      first <- i
    }
    if (i < start) {
      reached_backtrack <- TRUE
    }
  }
  return(first)
}

max_after_backtrack <- backtracks_df %>%
  mutate(max_after_backtrack = map_int(seq, first_backtrack)) %>%
  pull(max_after_backtrack)

# 4) Are backtracking sequences more common among even or odd starting integers? 
# Give the frequency counts for even and odd backtracking integers

even_odd_backtrack <- backtracks_df %>%
  group_by(parity) %>%
  summarise(frequency = n()) %>%
  pull(2)