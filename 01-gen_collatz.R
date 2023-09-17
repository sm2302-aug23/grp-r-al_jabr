library(tidyverse)
library(tibble)

# Create gen_collatz function --------------------------------------------------

gen_collatz <- function(n) {
  if (n <= 0) {
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

