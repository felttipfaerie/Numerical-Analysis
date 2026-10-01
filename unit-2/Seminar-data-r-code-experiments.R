# Sample dataset
x <- c(12, 18, 19, 21, 22, 23, 24, 25, 26, 27, 3, 28, 29, 29, 30, 31, 32, 33, 34, 35, 36)


# Histogram with class width 5
hist(
  x,
  main = "Histogram of Sample Data",
  xlab = "Value",
  ylab = "Frequency",
  breaks = seq(0, 40, by = 5)  # 10–14, 15–19, ...
)

# Boxplot
boxplot(
  x,
  main = "Boxplot of Sample Data",
  ylab = "Value",
  horizontal = FALSE
)

# install.packages(c("ggplot2"))
library(ggplot2)

df <- data.frame(value = x)

# Histogram with binwidth = 2
ggplot(df, aes(value)) +
geom_histogram(binwidth = 2, boundary = 10, closed = "right") +
labs(title = "Histogram of Sample Data", x = "Value", y = "Frequency")

# Boxplot
ggplot(df, aes(y = value)) +
geom_boxplot() +
labs(title = "Boxplot of Sample Data", y = "Value")

# Define class breaks from 10 to 40 by 5
breaks <- seq(10, 40, by = 5)

# Cut into intervals and tabulate
classes <- cut(x, breaks = breaks, right = TRUE, include.lowest = TRUE)
freq <- as.data.frame(table(classes))
names(freq) <- c("ClassInterval", "Frequency")

# Add relative and cumulative frequencies
freq$RelFreq <- freq$Frequency / sum(freq$Frequency)
freq$CumFreq <- cumsum(freq$Frequency)

print(freq)


# Five-number summary and quartiles
summary(x) # Min, 1st Qu., Median, Mean, 3rd Qu., Max
fivenum(x) # Tukey’s five-number summary

Q1 <- as.numeric(quantile(x, probs = 0.25, type = 2))
Q2 <- as.numeric(quantile(x, probs = 0.50, type = 2)) # median
Q3 <- as.numeric(quantile(x, probs = 0.75, type = 2))

Min <- min(x); Max <- max(x)

res <- data.frame(Min, Q1, Median = Q2, Q3, Max)
print(res)

Mode <- function(v) {
  tab <- table(v)
  m <- max(tab)
  as.numeric(names(tab)[tab == m])
}

Mode(x)