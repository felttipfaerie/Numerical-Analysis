library(fitdistrplus)

## Load the built-in ‘airquality’ dataset
data(airquality)

## Create an NA-free ozone vector
o3 <- as.numeric(na.omit(airquality$Ozone))

norm <- fitdist(data = o3, distr = dnorm)
norm

plot(norm)

qqcomp(norm)

hist(airquality$Temp, xlim = c(50, 100))
dotchart(airquality$Temp, xlim = c(50, 100))

gamma <- fitdist(data = o3, distr = dgamma)
plot(gamma)# Figure <link href="#c03-fig-0003"><?LINK 3.3?></link>


dbinom(x = 1, size = 5, prob = (3 / 13))

pnorm(q = 180, mean = 155, sd = 11, lower.tail = TRUE)
pbinom(q = 18, size = 155, prob = 0.11, lower.tail = FALSE)

pnorm(q = 17, mean = 20, sd = 5) ‐ pnorm(q = 12, mean = 20, sd = 5)

pnorm(q = 180, mean = 155, sd = 11, lower.tail = F)
qnorm(p = c(0.01, 0.99), mean = 155, sd = 11, lower.tail = T)

qnorm(p = c(0.01, 0.99), mean = 6, sd = 0.5, lower.tail = T)
