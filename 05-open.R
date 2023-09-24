# Code for Task 5 Open-ended exploration

# Investigating the correlation of the starting integers of the sequence
# with the lengths, maximum value, the number of even and odd numbers in the sequence


# Create odd_counts and even_counts to compute the frequency of each no. in the seq

odd_numbers_in_seq <- function(x) {
  odd_count <- sum(x %% 2 != 0)
  return(odd_count)
}
odd_counts <- double()
for (i in start) {
  odd_counts[i] <- odd_numbers_in_seq(collatz_df$seq[[i]])
}

even_numbers_in_seq <- function(x) {
  even_count <- sum(x %% 2 == 0)
  return(even_count)
}
even_counts <- double()
for (i in start) {
  even_counts[i] <- even_numbers_in_seq(collatz_df$seq[[i]])
}

# Compute the correlation of the starting integers with length,
# max_val, even_counts and odd_counts

correlation_start <- collatz_df %>%
  mutate(even_counts, odd_counts) %>%
  select(-seq, -parity) %>%
  cor()

# The correlation of start and length is 0.20541 ---------------------------

# The correlation coefficient is low
# There is a weak positive relationship between the starting integers and the 
# length of the sequence


# The correlation of start and max_val is 0.08813-----------------------------

# The correlation coefficient is very low
# which suggests that there is a very weak positive relationship between 
# the starting integers and maximum value in the sequence


# The correlation of start and even_counts is 0.22123--------------------------

# The correlation coefficient is low
# There is a weak positive relationship between the starting integers and the 
# number of even numbers in the sequence

start_even_counts <- collatz_df %>%
  mutate(even_counts) %>%
  select(-seq, -parity) %>%
  ggplot(., aes(x = start,
                y = even_counts)) +
  geom_point(color = "skyblue") +
  geom_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              color = "red") +
  labs(x = "Starting integers",
       y = "Even numbers in the sequence") +
  theme_minimal()

# The correlation of start and odd_counts is 0.17986--------------------------

# The correlation coefficient is low
# There is a weak positive relationship between the starting integers and the 
# number of even numbers in the sequence

start_odd_counts <- collatz_df %>%
  mutate(odd_counts) %>%
  select(-seq, -parity) %>%
  ggplot(., aes(x = start,
                y = odd_counts)) +
  geom_point(color = "skyblue") +
  geom_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              color = "red") +
  labs(x = "Starting integers",
       y = "Odd numbers in the seq") +
  theme_minimal()

# The correlation between odd_counts and even_counts is 0.99879 -------------

# the correlation coefficient is very high
# There is a very strong positive relationship between the number of even and 
# the number of odd numbers in the sequence

even_odd_counts <- collatz_df %>%
  mutate(odd_counts, even_counts) %>%
  select(-seq, -parity) %>%
  ggplot(., aes(x = even_counts,
                y = odd_counts)) +
  geom_point(color = "skyblue") +
  geom_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              color = "red") +
  labs(x = "Even numbers",
       y = "Odd numbers") +
  theme_minimal()

library(ggpubr)
ggarrange(start_even_counts, start_odd_counts, even_odd_counts,
          labels = c("p = 0.22123", "p = 0.17986", "p = 0.99879"),
          hjust = -2,
          font.label = list(size = 9),
          ncol = 2, nrow = 2)
