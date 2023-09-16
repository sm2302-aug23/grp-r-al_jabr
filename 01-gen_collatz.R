library(tibble)

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

