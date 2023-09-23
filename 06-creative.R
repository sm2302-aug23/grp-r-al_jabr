library(tidyverse)

# Code for Task 6 Creative visualisation ---------------------------------------

# 1) Numerical Progression for each starting integer ---------------------------

collatz_df %>%
  unnest(seq) %>%
  group_by(start) %>%
  mutate(step = row_number()) %>%
  ggplot(.,
         aes(x = step,
             y = seq)) +
  geom_line()

# Choose starting integer = 27

collatz_df %>%
  unnest(seq) %>%
  group_by(start) %>%
  filter(start %in% 27) %>%
  mutate(step = row_number()) %>%
  ggplot(.,
         aes(x = step,
             y = seq)) +
  geom_line(aes(col = "red")) +
  labs(
    title = "Collatz Sequence Line Plot",
    x = "Step",
    y = "Value"
  ) +
  theme_minimal()

# 2) Plot the highest value reached by each starting integer -------------------
# y-axis(0,100000)

collatz_df %>%
  unnest(seq) %>%
  group_by(start) %>%
  filter(start %in% 1:10000) %>%
  slice_max(order_by = seq) %>%
  ggplot(.,
         aes(x = start,
             y = seq)) +
  geom_point(aes(col = start,
                 alpha = length),
             size = 1) +
  labs(
    title = "Collatz Conjecture",
    subtitle = "Max value reached by each starting integer",
    x = "Starting Integer",
    y = "Value"
  ) +
  theme_minimal() + 
  
  xlim(0, 10000) +
  ylim(0, 100000)
