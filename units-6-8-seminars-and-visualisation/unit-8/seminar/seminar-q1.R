# Coding-assistant note: OpenAI Codex assisted with coding. The project owner made the key decisions on how to segment and manage the data.
# Dataset note: Uses the Sample Superstore sales training data at units-6-8-seminars-and-visualisation/unit-8/seminar/train.csv in this repository. A public compatible download is available at https://www.kaggle.com/datasets/vivek468/superstore-dataset-final
library(readr)
library(dplyr)


train <- read_csv("seminar/train.csv")
View(train)

d1 <- train

#simplify dataset
d2 <- train %>%
  select(`Customer ID`, Category, Sales)


# box plot
boxplot(Sales ~ Category,
        data = d2,
        main = "Category Vs Sales",
        xlab = "Category",
        ylab = "Sales",
        col = "lightblue")


#get values
table(d2$Category)

#remove office supplies
d3 <- d2[d2$Category != "Office Supplies", ]

aggregate(Sales ~ Category, data = d3, summary)

#boxplot new dataset - obvious outliers in technology - right tailed
boxplot(Sales ~ Category,
        data = d3,
        main = "Category Vs Sales",
        xlab = "Category",
        ylab = "Sales",
        col = "lightblue")

#t test
t.test(Sales ~ Category, data = d3, var.equal = TRUE)

#wilcox test
wilcox.test(Sales ~ Category, data = d3, exact = FALSE)

#create new data set remove outliers - 1.5IQR

d4 <- d3 %>%
  group_by(Category) %>%
  filter(
    Sales >= quantile(Sales, 0.25, na.rm = TRUE) - 1.5 * IQR(Sales, na.rm = TRUE),
    Sales <= quantile(Sales, 0.75, na.rm = TRUE) + 1.5 * IQR(Sales, na.rm = TRUE)
  ) %>%
  ungroup()

#get values
table(d3$Category)
table(d4$Category)

#check outliers
aggregate(Sales ~ Category, data = d4, summary)


#t test
t.test(Sales ~ Category, data = d4, var.equal = TRUE)

#wilcox test
wilcox.test(Sales ~ Category, data = d4, exact = FALSE)
