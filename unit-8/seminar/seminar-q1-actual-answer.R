library(readr)
library(dplyr)

train <- read_csv("seminar/train.csv")
View(train)

d1 <- train

#simplify dataset
d2 <- train %>%
  select(`Customer ID`, Category, Sales)

#get values
table(d2$Category)

#remove office supplies
d3 <- d2[d2$Category != "Office Supplies", ]
table(d3$Category)

#Check zero values
sum(df$`Furniture sales total` == 0, na.rm = TRUE)
sum(df$`Technology sales total` == 0, na.rm = TRUE)

aggregate(Sales ~ Category, data = d3, summary)


#manipulate data to match customer id for catagories
d4 <- d3 |>
  group_by(`Customer ID`) |>
  summarise(
    `Furniture sales total` = sum(Sales[Category == "Furniture"], na.rm = TRUE),
    `Technology sales total` = sum(Sales[Category == "Technology"], na.rm = TRUE)
  )

#check fir empty values
colSums(is.na(d4))

# Check zero values
sum(d4$`Furniture sales total` == 0, na.rm = TRUE)
sum(d4$`Technology sales total` == 0, na.rm = TRUE)

#remove zero values
d5 <- d4 |>
  dplyr::filter(
    `Furniture sales total` != 0,
    `Technology sales total` != 0
  )

#boxplot new dataset - obvious outliers in technology - right tailed high number of outliers
boxplot(
  d5$`Furniture sales total`,
  d5$`Technology sales total`,
  names = c("Furniture", "Technology"),
  main = "Furniture vs Technology",
  xlab = "Category",
  ylab = "Sales total",
  col = c("orange", "lightblue")
)

#compaire - !! note high difference between median and mean
summary(d5[, c("Furniture sales total", "Technology sales total")])


#paired t-test on unclean values 
# p value high - do not reject null hypothisis
t.test(
  d5$`Furniture sales total`,
  d5$`Technology sales total`,
  paired = TRUE
)

#wilcox test
# p value high - do not reject null hypothisis
wilcox.test(
  d5$`Furniture sales total`,
  d5$`Technology sales total`,
  paired = TRUE
)

#create new data set remove outliers - 1.5IQR
Q1_f <- quantile(d5$`Furniture sales total`, 0.25, na.rm = TRUE)
Q3_f <- quantile(d5$`Furniture sales total`, 0.75, na.rm = TRUE)
IQR_f <- IQR(d5$`Furniture sales total`, na.rm = TRUE)

Q1_t <- quantile(d5$`Technology sales total`, 0.25, na.rm = TRUE)
Q3_t <- quantile(d5$`Technology sales total`, 0.75, na.rm = TRUE)
IQR_t <- IQR(d5$`Technology sales total`, na.rm = TRUE)

d6 <- d5[
  d5$`Furniture sales total` >= (Q1_f - 1.5 * IQR_f) &
    d5$`Furniture sales total` <= (Q3_f + 1.5 * IQR_f) &
    d5$`Technology sales total` >= (Q1_t - 1.5 * IQR_t) &
    d5$`Technology sales total` <= (Q3_t + 1.5 * IQR_t),
]

#compare
nrow(d5)
nrow(d6)


#boxplot new dataset - obvious outliers in technology - less right-tailed data
boxplot(
  d6$`Furniture sales total`,
  d6$`Technology sales total`,
  names = c("Furniture", "Technology"),
  main = "Furniture vs Technology",
  xlab = "Category",
  ylab = "Sales total",
  col = c("orange", "lightblue")
)

#compare
# !! mildly less difference between mean and median, still notable in technology
summary(d6[, c("Furniture sales total", "Technology sales total")])


#paired t-test on no outlier values
# p value low - reject null hypothisis
t.test(
  d6$`Furniture sales total`,
  d6$`Technology sales total`,
  paired = TRUE
)

#wilcox test
# p value low - reject null hypothisis
wilcox.test(
  d6$`Furniture sales total`,
  d6$`Technology sales total`,
  paired = TRUE
)

