library(MASS)
library(dplyr)
library(performance)

# Source: Bader and Leuzinger (2024, Chapter 8)
lr <- leaf_respiration_1
str(lr)
summary(lr)

plot(leafresp ~ temp, data = lr)

m1 <-  lm(leafresp ~ temp, data = lr)
summary(m1)

quantile(resid(m1))

2 * pt(q = abs(coef(summary(m1))[, 3]), df = df.residual(m1), lower.tail = F)

sqrt(sum(resid(m1)^2)/df.residual(m1))

anova(m1)

par(mfrow = c(2, 2))
plot(m1)

check_model(m1)

model <- aov(temp ~ ., data = lr)
model

predict(m1, newdata = data.frame(temp = 45))

newdat <- data.frame(temp = seq(min(lr$temp, na.rm = T), max(lr$temp, na.rm = T), length.out = 200))
preds <- predict(m1, newdata = newdat, interval = "confidence", level = 0.95)
newdat <- bind_cols(newdat, preds) # combine all into one dataframe
head(newdat, n = 3)

plot(leafresp ~ temp, data = lr)

with(
  newdat,
  polygon(
    x = c(temp, rev(temp)),
    y = c(lwr, rev(upr)),
    col = "grey75",
    border = "grey75"
  )
)  # 95% confidence interval

points(
  leafresp ~ temp,
  data = lr,
  pch = 21,
  bg = "black"
)

# Regression line within the predictor range
lines(
  fit ~ temp,
  data = newdat
)

# Add R-squared value
text(
  x = 9.5,
  y = 8.3,
  label = expression(paste(italic("R")^2, " = 0.84")),
  adj = c(0, 0.5)
)

# Add p-value
text(
  x = 9.5,
  y = 7.7,
  label = expression(paste(italic("P"), " < 0.001")),
  adj = c(0, 0.5)
)

library(readr)
leaf_respiration_2 <- read_csv("leaf_respiration_2.csv")

lr2 <- leaf_respiration_2
plot(leafresp2 ~ temp, data = lr2)

m2 <-  lm(leafresp2 ~ temp, data = lr2)

par(mfrow = c(2, 2))
plot(m2)

lr2 <- mutate(.data = lr2, leafresp2_cr = leafresp2^(1/3), 
              temp_cr = temp^(1/3))
head(lr2, n = 3)

plot(leafresp2_cr ~ temp_cr, data = lr2)

library(readr)
leaf_respiration_3 <- read_csv("leaf_respiration_3.csv")

lr3 <- leaf_respiration_3
str(lr3)
summary(lr3)

pairs(lr3)

pairs.panels(lr3,
             method = "pearson", # correlation method
             hist.col = "grey", # colour of histograms
             density = TRUE,  # show density plots
             ellipses = F, scale = T, cex.cor = 1 # additional settings
)

library(car)

m4 <- lm(
  leafresp ~ temp + sla + nitro,
  data = lr3
)

summary(m4)

vif(m4)