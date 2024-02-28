# Question 1: Sequences

# Create the following sequences respectively.

# i. (40 60 80 100 120 140 160 180 200)
seq(from = 40, to = 200, by = 20)

# ii. (10 4 -2 -8 -14 -20 -26 -32)
seq(10, -32, -6)

# iii. (-2 -2 -2 -1 -1 -1 0 0 0 1 1 1 2 2 2 3 3 3 )
rep(-2:3, each = 3)

# iv. (“b” “d” “f” “h” “j” “l” “n” “p” “r” “t” “v” “x” “z”)
letters[seq(from = 2, to = 26, by = 2)]

# Question 2: Matrices

# Part A

A <- matrix(rep(c(2, 3, 2), each = 3), nrow = 3)
#      [,1] [,2] [,3]
# [1,]    2    3    2
# [2,]    2    3    2
# [3,]    2    3    2

B <- matrix(c(4, 2, -4, 6, -2, 2, 5, 4, -4, 1, 2, 6), nrow = 4)
#      [,1] [,2] [,3]
# [1,]    4   -2   -4
# [2,]    2    2    1
# [3,]   -4    5    2
# [4,]    6    4    6

C <- matrix(c(3, 4, 5, -2, -3, -4, 2, 5, 6), nrow = 3)
#      [,1] [,2] [,3]
# [1,]    3   -2    2
# [2,]    4   -3    5
# [3,]    5   -4    6

D <- diag(2:4)
#      [,1] [,2] [,3]
# [1,]    2    0    0
# [2,]    0    3    0
# [3,]    0    0    4

# Part B

A + D

solve(D) %*% C

A %*% t(C)

max(c(det(A), det(C), det(D)))

# Part C

replace_element <- function(matrix) {
  matrix[2, 1] <- -1
  matrix[3, 2] <- 1
  return(matrix)
}

A <- replace_element(A)
B <- replace_element(B)
C <- replace_element(C)

M <- rbind(A, B)
#      [,1] [,2] [,3]
# [1,]    2    3    2
# [2,]   -1    3    2
# [3,]    2    1    2
# [4,]    4   -2   -4
# [5,]   -1    2    1
# [6,]   -4    1    2
# [7,]    6    4    6

L <- rbind(C, D)
rownames(L) <- paste("row.", 1:nrow(L), sep = "")
colnames(L) <- paste("col.", 1:ncol(L), sep = "")
#       col.1 col.2 col.3
# row.1     3    -2     2
# row.2    -1    -3     5
# row.3     5     1     6
# row.4     2     0     0
# row.5     0     3     0
# row.6     0     0     4

my_matrices <- list(B, C, D)
my_matrices$favorite_films <- c("A", "B", "C")
# [[1]]
#      [,1] [,2] [,3]
# [1,]    4   -2   -4
# [2,]   -1    2    1
# [3,]   -4    1    2
# [4,]    6    4    6
#
# [[2]]
#      [,1] [,2] [,3]
# [1,]    3   -2    2
# [2,]   -1   -3    5
# [3,]    5    1    6
#
# [[3]]
#      [,1] [,2] [,3]
# [1,]    2    0    0
# [2,]    0    3    0
# [3,]    0    0    4
#
# $favorite_films
# [1] "A" "B" "C"

K <- solve(A) %*% C + A %*% t(D)
#           [,1]     [,2] [,3]
# [1,]  5.333333 9.333333    7
# [2,] -3.000000 7.500000    6
# [3,]  5.666667 3.916667   13

arr <- array(c(A, C), dim = c(nrow(A), ncol(A), 2))
arr

# Question 3: Loops

set.seed(16)
values <- c()
while (TRUE) {
  x <- sample(1:10, 1)
  if (x == 7) {
    break
  }
  values <- c(values, x)
}
freq <- table(values)
hist(freq, breaks = seq(0.5, 10.5, by = 1),
     main = "Histogram of the Generated Values",
     xlab = "Values", ylab = "Frequency",
     col = "darkblue", border = "red")

# Question 4: Multiplication Table

n <- 10
m <- matrix(0, nrow = n, ncol = n)

for (i in 1:dim(m)[1]) {
  for (j in 1:dim(m)[2]) {
    m[i, j] <- i * j
  }
}

# Question 5: Nested Loops

n <- 4
m <- matrix(0, nrow = n, ncol = n)

for (i in 1:n) {
  for (j in 1:n) {
    if (i == j) {
      m[i, j] <- 0
    } else {
      m[i, j] <- abs(i - j)
    }
  }
}

# Question 6: Currency Function

converter <- function(tl) {
  factors <- c(TL = 1, USD = 0.033, CAD = 0.044, EUR = 0.030, CHF = 0.029)
  return(tl * factors)
}
converter(17000)

# Question 7: Loading Data

data <- read.csv("https://users.ssc.wisc.edu/~hemken/Rworkshops/read/classm.csv")

str(data)
# $ Name  : chr  "Alfred" "Alice" "Barbara" "Carol" ...
# $ Sex   : chr  "M" "F" "F" "F" ...
# $ Age   : int  14 13 13 14 14 12 12 15 13 12 ...
# $ Height: num  NA 56.5 65.3 62.8 63.5 57.3 59.8 62.5 62.5 59 ...
# $ Weight: num  112 84 98 102 102 ...

head(data, 4)
tail(data, 2)

sum(is.na(data))

na.omit(data)
# or

fill_na <- function(col) {
  mean_val <- mean(col, na.rm = TRUE)
  col[is.na(col)] <- mean_val
  return(col)
}

data <- as.data.frame(lapply(data, fill_na))

data$Height <- data$Height * 0.0254
data$BMI <- data$Weight / (data$Height^2)
