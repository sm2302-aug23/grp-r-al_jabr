library(tidyverse)
library(ggplot2)
library(ggrepel)

# Task 4 -----------------------------------------------------------------------

# Scatterplot of all the sequence lengths --------------------------------------
plot1<- ggplot( data = collatz_df,
                mapping = aes(x = start,
                              y = length)
)+
  geom_point()+
  labs(
    title = "Scatterplot of Sequence Lengths",
    x = "Starting Integer",
    y = "Length of Sequence"
  )

# To find the top 10 longest starting integers
sortedlength <- collatz_df %>% arrange(desc(length))
top_10_length <- sortedlength %>%top_n(10,length)

# To identify the top 10 longest starting integers in the scatterplot
scatterplot1 <-
  plot1 + 
  geom_point(data = top_10_length,aes(colour = "Top 10"))+
  scale_colour_manual(values = c("Top 10" = "blue"))+
  geom_text_repel(data = top_10_length, aes(label = start))

scatterplot1

# Scatterplot of the highest values of starting integers -----------------------
plot2<- ggplot( data = collatz_df,
                mapping = aes(x = start,
                              y = max_val)
)+
  geom_point()+
  labs(
    title = "Scatterplot Of Maximum Value Reached In Sequence",
    x = "Starting Integer",
    y = "Maximum Value"
  )

# To find the top 10 starting integers with the highest value
sortedvalue <- collatz_df %>% arrange(desc(max_val))
top_10_value <- sortedvalue[1:10,]

# To highlight the top 10 starting integers 
scatterplot2 <-
  plot2 + 
  geom_point(data = top_10_value,
             aes(colour = "Top 10"))+
  scale_colour_manual(values = c("Top 10" = "red"))+
  geom_text_repel( data = top_10_value, aes(label = start))

scatterplot2

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

#Are there any noticeable differences?
# Boxplot A: even starting integers
# Boxplot B: odd starting integers

# Outliers are only present in boxplot B.
# The median in boxplot B is higher than in boxplot A, which indicates that odd
  # starting integers result in longer sequences.

