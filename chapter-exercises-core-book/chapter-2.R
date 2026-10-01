#ata set
x <- c(3, 5, 1, 3, 7, 6, 7, 4, 2) # creating a variable

# extract info
n <- length(x) # extracting the length (n)
mean_x <- mean(x) # calculating the mean

#variance
ss <- sum((mean_x - x)^2) # calculating the sum of squares

v <- ss/(n-1) # calculating the variance
var(x) # using the R function

# standard deviation
sd <- sqrt(v)
sd(x)

#standard error
se <- sd/n 


# standard error function
sandard_error <- function(x) {
  sd(x)/sqrt(length(x))
  }


hist(x, xlim = c(0, 8))
dotchart(x, xlim = c(0, 8))