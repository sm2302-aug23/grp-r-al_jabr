library(tidyverse)
library(tibble)

#task 3--------------------------------------------------------------

# Step 1: Filter collatz_df to retain starting integers that exhibit backtracking
backtracks_df <- collatz_df %>%
  filter(length >= 2 & max_val > start)

# Step 2: Calculate the most frequently occurring number of times they go above their starting integer
mode_backtrack <- as.numeric(names(sort(table(backtracks_df$length - 1), decreasing = TRUE)[1]))

# Step 3: Calculate the maximum value reached after the first backtrack
max_after_backtrack <- max(backtracks_df$max_val)

# Step 4: Determine if backtracking sequences are more common among even or odd starting integers
even_odd_backtrack <- table(backtracks_df$parity)

