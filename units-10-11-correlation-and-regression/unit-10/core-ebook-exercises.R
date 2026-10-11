library(rpart)
library(rpart.plot)

set.seed(68176916) # use this command to reproduce the same dataset
surv <-  data.frame(survived = rbinom(132, 1, .4),
    genotype = round(runif(132, 0.5, 3.5)),
    method = round(runif(132, .8, 3.5)))

str(surv)
sapply(surv, class)

sur$genotype <- as.integer(surv$genotype)
surv$method <- as.integer(surv$method)
surv$survived <- as.factor(surv$survived)

sapply(surv, class)

table(surv[, c('survived', 'method')])

mosaicplot(survived ~ method, data = surv, main = '(a)  only method')

mosaicplot(survived ~ method + genotype, data = surv, main = '(b) method and genotype')

method <- table(surv[, c('survived', 'method')], dnn = c('survived', 'method')) # dnn adds the variable names
method <- addmargins(method) # addmargins adds the row and column totals
method


#chi-squared test
tab <- table(surv$survived, surv$genotype)
tab

chisq.test(surv$survived, surv$genotype)
chi$statistic
pchisq(22.62, df = 2, lower.tail = F)


set.seed(31129) # so you can reproduce the dataset
surv$size <- round(rnorm(132, mean = 20, sd = 3), digits = 1)
head(surv, n = 3)

boxplot(size ~ survived, data = surv)

#create testing/training split
set.seed(1) # setting a seed so you can reproduce this
split <- sample(1:nrow(surv), 0.8*nrow(surv)) # 80/20 split
trainSplit$survived <- as.factor(trainSplit$survived)
testSplit$survived <- as.factor(testSplit$survived)

m1 <- rpart(survived ~ ., data = trainSplit, method = "class")

predictTest <- predict(m1, testSplit, type = "class")
predictTest
table(predictTest, testSplit$survived)
accuracy <- function(x, y) { # x and y are the modelled and true classifications
  round(100 * sum(diag(table(x, y)))/sum(table(x, y)))
}
accuracy(predictTest, testSplit$survived)