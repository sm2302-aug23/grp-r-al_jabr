library(tidyverse)
library(hexbin)

# Code for Task 6 Creative Visualization ---------------------------------------

# 1) Plot the highest value reached by each starting integer -------------------
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
    subtitle = "Max value reached by each starting integer ",
    x = "Starting Integer",
    y = "Value"
  ) +
  theme_minimal() + 
  xlim(0, 10000) +
  ylim(0, 100000)

ggsave("highest_value_reached_by_each_starting_integer.png",
       width = 1980,
       height = 1980,
       units = "px",
       bg = "white",
       dpi = 300)

# 2) Numerical Progression for each starting integer ---------------------------

# Let's look at the numerical progression of starting integer n = 27

collatz_df %>%
  unnest(seq) %>%
  group_by(start) %>%
  filter(start %in% 27) %>%
  mutate(steps = row_number()) %>%
  ggplot(.,
         aes(x = steps,
             y = seq)) +
  geom_line() +
  labs(
    title = "Collatz Sequence Line Plot",
    x = "Steps",
    y = "Value"
  ) +
  theme_classic()

ggsave("numerical_progression_of_27.png",
       width = 1980,
       height = 1980,
       units = "px",
       bg = "white",
       dpi = 300)

# Now, for starting integer from 1:30

collatz_df %>%
  unnest(seq) %>%
  group_by(start) %>%
  filter(start %in% 1:30) %>%
  mutate(steps = row_number()) %>%
  ggplot(.,
         aes(x = steps,
             y = seq)) +
  geom_line(aes(col = length)) +
  facet_wrap(start ~ length, scales = "free") +
  labs(
    title = "Collatz Sequence Line Plot",
    subtitle = "Numerical Progression for each starting integer",
    x = "Steps",
    y = "Value"
  ) +
  theme_classic()

ggsave("numerical_progression_of_1_to_30.png",
       width = 2500,
       height = 2500,
       units = "px",
       bg = "white",
       dpi = 300)

# 3) How long are Collatz sequences?

collatz_df %>%
  unnest(seq) %>%
  group_by(start) %>%
  mutate(steps = row_number()) %>%
  ggplot(.,
         aes(x = start,
             y = steps)) +
  geom_hex() +
  scale_y_continuous(breaks = seq(0, 275, by = 25)) +
  scale_fill_viridis_c() +
  theme_minimal()

ggsave("collatz_sequences_hex.png",
       width = 2500,
       height = 2500,
       units = "px",
       bg = "white",
       dpi = 300)