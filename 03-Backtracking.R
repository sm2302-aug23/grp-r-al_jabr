library(tidyverse)
library(tibble)


# Create gen_collatz function --------------------------------------------------

gen_collatz <- function(n) {
  if (!is.integer(n) || n <= 0) {
    stop("Input must be a positive integer")
  }
  seq <- c(n)
  while (n != 1) {
    if (n %% 2 == 0) {
      n <- n / 2
    } else {
      n <- 3 * n + 1
    }
    seq <- c(seq, n)
  }
  return(seq)
}


# Create collatz_df tibble -----------------------------------------------------

start <- 1:10000

seq <- list()
for (i in start) {
  collatz_seq <- gen_collatz(i)
  seq[[i]] <- collatz_seq
}

length <- double()
for (i in start) {
  length[i] <- length(gen_collatz(i))
}

parity <- ifelse(start %% 2 == 0,
                 "Even",
                 "Odd")

max_val <- double()
for (i in start) {
  max_val[i] <- max(gen_collatz(i))
}


collatz_df <- tibble(start,
                     seq,
                     length,
                     parity,
                     max_val)
collatz_df
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

# Print the results
print("Starting Integers that Exhibit Backtracking:")
print(backtracks_df$start)

print(paste("Most Frequently Occurring Number of Backtracks:", mode_backtrack))
print(paste("Maximum Value Reached After the First Backtrack:", max_after_backtrack))
print("Frequency Counts for Even and Odd Backtracking Integers:")
print(even_odd_backtrack)

