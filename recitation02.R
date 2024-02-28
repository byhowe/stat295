# Get the current working directory
getwd()

setwd(file.path(getwd(), "recit2"))

set.seed(16)

# Create a list of matrices and write them out to disk.
m <- matrix(rbinom(72, size = 10, prob = 0.5), nrow = 12, ncol = 6)
rownames(m) <- paste0("Row.", seq_len(nrow(m)))
colnames(m) <- paste0("Col.", seq_len(ncol(m)))

l <- matrix(rnorm(72), nrow = 12, ncol = 6)
rownames(l) <- paste0("Row.", seq_len(nrow(l)))
colnames(l) <- paste0("Col.", seq_len(ncol(l)))

out <- list(l, m)
write.csv(out, "recit2out.csv")

# Create a function
pow <- function(x, y) {
  print(paste0("`x` raised to the power of `y` = ", x^y))
}
pow(2, 5)

# Read data from web and plot
data <- read.csv("https://people.sc.fsu.edu/~jburkardt/data/csv/biostats.csv")
colnames(data) <- c("name", "sex", "age", "height", "weight")

plot(data$height, data$weight, col = "lightblue", lwd = 3,
     xlab = "Height", ylab = "Weight",
     main = "Relationship between height and weight")

dev.print(pdf, "heightweight.pdf")

#
library(babynames)
baby10 <- babynames[sample(nrow(babynames), size = 10, replace = FALSE), ]

barplot(baby10$n, names.arg = baby10$name,
        col = ifelse(baby10$sex == "M", "lightblue", "pink"),
        main = "Number of names by sex",
        xlab = "Name", ylab = "Proportion",
        cex.names = 0.8, beside = TRUE)
legend("topright", legend = unique(baby10$sex),
       fill = c("lightblue", "pink"), title = "sex")
dev.print(pdf, "babygender.pdf")

var1 <- seq(10, 60, 6)
var2 <- list(1:20, letters[1:20])
var3 <- matrix(rnorm(24), nrow = 12)
var4 <- matrix(rep(c(4, 6, 4), 3), nrow = 3, byrow = TRUE)

save(var1, var2, var3, var4, file = "vars.RData")

rm(list = paste0("var", 1:4))
"var2" %in% ls()
# FALSE

load("vars.RData")
"var2" %in% ls()
# TRUE
