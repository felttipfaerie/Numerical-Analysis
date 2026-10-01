library(readr)
library(dplyr)

train <- read_csv("seminar/train.csv")
View(train)

d1 <- train

#query segment
table(d1$Segment)

#simplify dataset
d2 <- train %>%
  select(Segment, Sales)

#remove home office
d3 <- d2[d2$Segment != "Home Office", ]

#count
#!!! high disparity between number of purchases - might cause later issues
table(d3$Segment)


#inspect - highly right-squewed data!
consumer <- d3$Sales[d3$Segment == "Consumer"]
corporate <- d3$Sales[d3$Segment == "Corporate"]

plot(
  density(consumer, na.rm = TRUE),
  main = "Sales Density by Segment",
  xlab = "Sales",
  ylab = "Density",
  col = "orange",
  lwd = 2
)

lines(
  density(corporate, na.rm = TRUE),
  col = "blue",
  lwd = 2
)

legend(
  "topright",
  legend = c("Consumer", "Corporate"),
  col = c("orange", "blue"),
  lwd = 2
)

#compare
by(d3$Sales, d3$Segment, summary)

#test for variance
var.test(
  Sales ~ Segment,
  data = d3
)


# nonpaired ttest with demonstrated same variance
t.test(
  Sales ~ Segment,
  data = d3,
  var.equal = TRUE
)

#for comparison a Mann–Whitney U test
wilcox.test(
  Sales ~ Segment,
  data = d3
)