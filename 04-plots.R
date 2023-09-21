library(tidyverse)
library(ggplot2)

# Task 4 -----------------------------------------------------------------------

# Scatterplot of all the sequence lengths --------------------------------------
# Horizontal Axis : starting integer
# Vertical Axis   : length of the sequence
# Identify the top 10 starting integers on your plot

#Scatterplot
ggplot( data = collatz_df,
        mapping = aes(x = start,
                      y = length)
)+
  geom_point()
  labs(
    title = "Scatterplot of Sequence Lengths",
    x = "Starting integer",
    y = "Length of sequence"
  )

# Scatterplot of the highest values of starting integers -----------------------
# Horizontal Axis : starting integer
# Vertical Axis   : max value
# Identify the top 10 starting integers

#Scatterplot
ggplot( data = collatz_df,
        mapping =aes(x = start,
                     y = max_val)
)+
  geom_point()+
  labs(
    title = "Scatterplot Of Maximum Value Reached In Sequence",
    x = "Starting Integer",
    y = "Maximum Value"
  )
  
# Boxplot of distribution of sequence length for even,odd  starting integers----

ggplot( data = collatz_df,
        mapping = aes( x = parity,
                       y = length))+
  geom_boxplot()+
  labs(
    title = "Boxplots Comparing Distribution of Even and Odd Starting Integers",
    x = "Parity",
    y = "Length of Sequence"
  )
