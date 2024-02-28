data <- read.csv("https://users.ssc.wisc.edu/~hemken/Rworkshops/read/classm.csv")

fill_na <- function(col) {
  mean <- mean(col, na.rm = TRUE)
  col[is.na(col)] <- mean
  return(col)
}

data <- as.data.frame(lapply(data, fill_na))
data$Height <- data$Height * 0.0254
data$BMI <- data$Weight / (data$Height^2)

data[                 data$BMI < 30, "Class"] <- "Not Classified"
data[data$BMI >= 30 & data$BMI < 35, "Class"] <- "Obesity Class 1"
data[data$BMI >= 35 & data$BMI < 40, "Class"] <- "Obesity Class 2"
data[data$BMI >= 40                , "Class"] <- "Obesity Class 3"

data
#       Name Sex Age   Height    Weight      BMI           Class
# 1   Alfred   M  14 1.573953 112.50000 45.41181 Obesity Class 3
# 2    Alice   F  13 1.435100  84.00000 40.78636 Obesity Class 3
# 3  Barbara   F  13 1.658620  98.00000 35.62315 Obesity Class 2
# 4    Carol   F  14 1.595120 102.50000 40.28442 Obesity Class 3
# 5    Henry   M  14 1.612900 102.50000 39.40116 Obesity Class 2
# 6    James   M  12 1.455420  83.00000 39.18334 Obesity Class 2
# 7     Jane   F  12 1.518920  84.50000 36.62578 Obesity Class 2
# 8    Janet   F  15 1.587500 112.50000 44.64009 Obesity Class 3
# 9  Jeffrey   M  13 1.587500  84.00000 33.33127 Obesity Class 1
# 10    John   M  12 1.498600  99.50000 44.30489 Obesity Class 3
# 11   Joyce   F  11 1.303020  50.50000 29.74330  Not Classified
# 12    Judy   F  14 1.633220  90.00000 33.74063 Obesity Class 1
# 13  Louise   F  12 1.430020  77.00000 37.65360 Obesity Class 2
# 14    Mary   F  15 1.689100 112.00000 39.25611 Obesity Class 2
# 15  Philip   M  16 1.828800 150.00000 44.84963 Obesity Class 3
# 16  Robert   M  12 1.645920  96.44118 35.59959 Obesity Class 2
# 17  Ronald   M  15 1.701800  96.44118 33.30009 Obesity Class 1
# 18  Thomas   M  11 1.460500  85.00000 39.84885 Obesity Class 2
# 19 William   M  15 1.689100 112.00000 39.25611 Obesity Class 2

# Install the babynames package in order to obtain the dataset.
install.packages("babynames")
library(babynames)

# Check dimensions
dim(babynames)
# There are about 2M rows.

# Change column names
colnames(babynames) <- c("Year", "Sex", "Name", "Count", "Proportion")

# Find the count regardless of the year by their names and sex.
freqs <- aggregate(babynames$Count, by = list(babynames$Sex, babynames$Name), FUN=sum)
# Rename the columns to be more understandable.
names(freqs) <- c("Sex", "Name", "Count")
dim(freqs)
# There are about 108K unique name and sex combinations.

# Sort based on the count.
sorted <- freqs[order(freqs$Count, decreasing = TRUE), ]
top20 <- head(sorted, 20)

# Use pink color for girls and blue color for boys.
colors <- top20$Sex
colors[colors == "F"] <- "pink"
colors[colors == "M"] <- "blue"

barplot(height = top20$Count / 1e6, names = top20$Name,
        main = "Popular Baby Names (1880 - 2017)",
        las = 2, ylab = "Count (Millions)",
        col = colors)
