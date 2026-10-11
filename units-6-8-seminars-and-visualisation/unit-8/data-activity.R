# Coding-assistant note: OpenAI Codex assisted with coding. The project owner made the key decisions on how to segment and manage the data.
# Dataset note: Uses the vendor score data at units-6-8-seminars-and-visualisation/unit-8/data-activity.csv in this repository. The project does not record a public download source for this file.
# Anova highlighted a statistically significant variance between the groups.
# Further investigation showed that there was no conclusive evidence for which vendor shows a higher significance of improvement.
# However, Vedor 3 deminstrated a significantly worse improvment in comparison to the other venders at a 95% confidence level.


library(readr)
data_activity <- read_csv("data-activity.csv")
View(data_activity)

d1 <- data_activity

#ANOVA
scores <- c(
  d1$`Group 1 scores (Vendor 1)`,
  d1$`Group 2 scores (Vendor 2)`,
  d1$`Group 3 scores (Vendor 3)`
)

vendor <- factor(rep(
  c("Vendor 1", "Vendor 2", "Vendor 3"),
  each = nrow(d1)
))

model <- aov(scores ~ vendor)

summary(model)

# find significance
TukeyHSD(model)

#box and whisker
boxplot(
  d1$`Group 1 scores (Vendor 1)`,
  d1$`Group 2 scores (Vendor 2)`,
  d1$`Group 3 scores (Vendor 3)`,
  names = c("Vendor 1", "Vendor 2", "Vendor 3"),
  xlab = "Vendor",
  ylab = "Score",
  main = "Scores by Vendor"
)

# stripchart
stripchart(list(
  "Vendor 1" = d1$`Group 1 scores (Vendor 1)`,
  "Vendor 2" = d1$`Group 2 scores (Vendor 2)`,
  "Vendor 3" = d1$`Group 3 scores (Vendor 3)`
),
method = "overplot",
vertical = TRUE,
pch = 16,
xlab = "Vendor",
ylab = "Score")

#jitter plot

stripchart(list(
  "Vendor 1" = d1$`Group 1 scores (Vendor 1)`,
  "Vendor 2" = d1$`Group 2 scores (Vendor 2)`,
  "Vendor 3" = d1$`Group 3 scores (Vendor 3)`
),
method = "jitter",
vertical = TRUE,
pch = 16,
xlab = "Vendor",
ylab = "Score",
main = "Scores by Vendor")

# density plot 

plot(
  density(d1$`Group 1 scores (Vendor 1)`, na.rm = TRUE),
  col = "blue",
  lwd = 2,
  xlab = "Score",
  main = "Score Distributions"
)

lines(
  density(d1$`Group 2 scores (Vendor 2)`, na.rm = TRUE),
  col = "red",
  lwd = 2
)

lines(
  density(d1$`Group 3 scores (Vendor 3)`, na.rm = TRUE),
  col = "green",
  lwd = 2
)

legend(
  "topright",
  legend = c("Vendor 1", "Vendor 2", "Vendor 3"),
  col = c("blue", "red", "green"),
  lwd = 2
)


