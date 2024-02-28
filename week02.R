# STAT295
# Week 2, Mon Feb 26 2024

# Exceptions and Timings

vec1 <- c(1, 2, 3)
vec2 <- c(1, 2)

# Notice,
vec1 * vec2
# Produces warning!

warn_demo <- function(x) {
  if (x <= 0) {
    warning("`x` is less than or equal to zero")
    x <- 1
  }

  return(2 / x)
}

warn_demo(0)

error_demo <- function(x) {
  if (x <= 0) {
    stop("`x` cannot be less than or equal to zero")
  }

  return(2 / x)
}

error_demo(0)

# Recursion and errors

recursive_fib <- function(n) {
  if (n < 0) {
    warning("got a negative value for `n`")
    return(recursive_fib(-n))
  } else if (n == 0) {
    stop("not defined for `n` equal to zero")
  } else if (n == 1 || n == 2) {
    return(1)
  } else {
    return(recursive_fib(n - 1) + recursive_fib(n - 2))
  }
}

recursive_fib(0)

# Try:
# Capture the error and store it in a new variable of class `try-error`.
attempt1 <- try(recursive_fib(0), silent = TRUE)
class(attempt)

# Capture the value if no error has occurred.
attempt2 <- try(recursive_fib(2), silent = TRUE)
attempt2

recursive_fibv <- function(nvec) {
  nterms <- length(nvec)
  result <- rep(0, length.out = nterms)
  for (i in seq_len(nterms)) {
    attempt <- try(recursive_fib(nvec[i]))
    # result[i] <- if (class(attempt) == "try-error") NA else attempt # nolint
    result[i] <- ifelse(class(attempt) == "try-error", NA, attempt)
  }
  result
}

foo <- recursive_fibv(c(1, 2, 10, 8))
bar <- recursive_fibv(c(3, 2, 7, 0, 9, 13))

# Suppression of warnings
attempt3 <- suppressWarnings(recursive_fib(-3))
attempt3

# Progress bars and timing
Sys.sleep(time = 3) # sleep for 3 seconds

sleep_demo <- function(n) {
  result <- 0
  for (i in seq_len(n)) {
    result <- result + 1
    Sys.sleep(0.5)
  }
  result
}

sleep_demo(5)

progressbar_demo <- function(n) {
  result <- 0
  bar <- txtProgressBar(min = 0, max = n, initial = 0, style = 3, char = "=")
  for (i in seq_len(n)) {
    result <- result + 1
    setTxtProgressBar(bar, value = i)
    Sys.sleep(0.2)
  }
  close(bar)
  result
}

progressbar_demo(10)

# Measure completion time

# A primitive way to bench a function by measuring the time it takes to
# complete.
bench <- function(f, ...) {
  t1 <- Sys.time()
  f(...)
  t2 <- Sys.time()
  t2 - t1
}

bench(progressbar_demo, 10)

# or

system.time(progressbar_demo(10))

# Avoid for loops

df <- as.data.frame(cbind(runif(10000), runif(10000)))

system.time(for (i in seq_len(dim(df)[1])) {
  df$mean2[i] <- mean(c(df[i, 1], df[i, 2]))
})

# or

system.time(df$mean2 <- apply(df, MARGIN = 1, FUN = mean))

v <- 1:10
lapply(2:3, function(i) v <- v * i) # WHAT DOES v <- mean?

# Unoptimized
timecal1 <- function(n) {
  res <- numeric(n)
  for (i in seq_len(n)) res[i] <- 2 * pi * sin(i)
  res
}

system.time(timecal1(10000000))

# Optimized
timecal2 <- function(n) {
  res <- numeric(n)
  for (i in seq_len(n)) res[i] <- sin(pi)
  res * 2 * pi
}

system.time(timecal2(10000000))

# Piping in R

library(tidyverse)

data(tips, package = "reshape2")
head(tips)

tips %>%
  subset(total_bill > 19) %>%
  aggregate(. ~ sex, ., mean) # . is a placeholder

a <- rnorm(10)
# compound assignment
a <- a |> abs() |> log() |> round(2)
a

env <- environment()
"a" %>% assign(20, envir = globalenv())
a

rnorm(100) %>%
  matrix(ncol = 2) %T>%
  plot() %>%
  str()
