library(tidyverse)

# Task 2 -----------------------------------------------------------------------

# Find the top 10 starting integers that produce the longest seq --------------- 

top10longest <-  collatz_df %>%
  arrange(desc(length)) %>%
  slice_head(n = 10) %>%
  pull(start)

# Which starting integer produces a seq that reaches the highest max value -----

max_val_int <- collatz_df %>%
  slice(which.max(max_val)) %>%
  pull(start)

# Average length & standard deviation of the sequence for even starting integers
# compared to odd ones? --------------------------------------------------------

even_odd_avg_len <- collatz_df %>%
  group_by(parity) %>%
  summarise(mean(length)) %>%
  pull(2)

even_odd_sd_len <- collatz_df %>%
  group_by(parity) %>%
  summarise(sd(length)) %>%
  pull(2)
