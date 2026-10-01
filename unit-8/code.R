# Coding-assistant note: OpenAI Codex assisted with coding. The project owner made the key decisions on how to segment and manage the data.
# Dataset note: Uses the Health Data SPSS dataset. It is intentionally excluded from Git; obtain it from the project owner and place it at unit-8/Health Data.sav. The project does not record a public download source for this file.
setwd("~/GitHub/Numerical Analysis/unit-8")
 
library(haven)
Health_Data <- read_sav("Health Data.sav")
View(Health_Data)

d1 <- Health_Data

wilcox.test(sbp~sex_1,Health_Data)
# compared to scatter plot no obvious corelation 
plot(d1$sbp, d1$sex_1)


#more variance in one gender than another (right-tailed) although medians are similar
boxplot(sbp ~ sex,
          data = d1,
          main = "systolic blood pressure by sex",
          xlab = "systolic blood pressure",
          ylab = "Sex",
          col = "lightblue")

# Split SBP by sex
sbp_f <- d1$sbp[d1$sex_1 == 0]
sbp_m <- d1$sbp[d1$sex_1 == 1]

# Density curves
female <- density(sbp_f, na.rm = TRUE)
male   <- density(sbp_m, na.rm = TRUE)

# Find maximum density across BOTH curves
max_y <- max(female$y, male$y)

# Plot female curve
plot(female,
     col = "red",
     lwd = 2,
     ylim = c(0, max_y * 1.1),
     main = "Distribution of systolic blood pressure by sex",
     xlab = "Systolic blood pressure",
     ylab = "Density")

# Add male curve
lines(male,
      col = "blue",
      lwd = 2)

# Legend
legend("topright",
       legend = c("Female", "Male"),
       col = c("red", "blue"),
       lwd = 2)

#variance between groups
# p value of 0.04014 suggests a statistical significance in variance, but not median
tapply(d1$sbp, d1$sex_1, var, na.rm = TRUE)
var.test(sbp ~ sex_1, data = d1)

# wilcox test
wilcox.test(
  d1$pre_test,
  d1$post_test,
  exact = FALSE,
  paired = TRUE
)

#kruskal test - p-value = 0.9733 can not reject null hypothisis
kruskal.test(sbp~religion, data = Health_Data)
