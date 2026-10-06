#The following code was created for the end of module assement for the Numerical Analysis July 2026 Statistical Analysis Presentation
#the dataset was provided for the presentation
#ChatGPT was used as a coding agent and to check for errors and inconsistencies in the final code, results and output.
#Any borrowed or adapted code snippet are indicated if and where applicable
# All prompts were written by Eleanor-Rose
# And all code was reviewed and edited by Eleanor-Rose and the conclusions from the data reached are her own


library(GGally)
library(readr)
library(ggplot2)
library(psych)
library(car)
library(rpart)
library(rpart.plot)
library(broom)
library(dplyr)

akshaydattatraykhare_diabetes_dataset <- read_csv("akshaydattatraykhare-diabetes-dataset.csv")

df <- akshaydattatraykhare_diabetes_dataset

#inspect dataset
summary(df)

#there apear to be missing values that have been filled with 0
colSums(is.na(df))
colSums(df == 0)

#full summary table
# Function to calculate mode
get_mode <- function(x) {
  x <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Select numeric columns only
numeric_df <- df[sapply(df, is.numeric)]

# Variables where 0 should be excluded
zero_remove <- c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")

# Remove zero values only for variables in zero_remove
if (col %in% zero_remove) {
  x <- x[x != 0]
}

#create summary plot, remove zeros
num_cols <- names(df)[sapply(df, is.numeric)]

par(mfrow = c(3, 3))

for (col in num_cols) {
  
  x <- df[[col]]
  
  # Remove zero values only for variables in zero_remove
  if (col %in% zero_remove) {
    x <- x[x != 0]
  }
  
  x <- na.omit(x)
  
  # Add * to labels where zero values were removed
  label <- ifelse(col %in% zero_remove, paste0(col, "*"), col)
  
  hist(
    x,
    main = label,
    xlab = label,
    breaks = 20
  )
}

par(mfrow = c(1, 1))







# Create summary table for all values, excluding the zero value for glucose, blood pressure, skin thickness insulin bmi


# Function to remove zeros only where needed
clean_values <- function(x, name) {
  if (name %in% zero_remove) {
    x <- x[x != 0]
  }
  x
}

summary_table <- data.frame(
  Variable = names(numeric_df),
  
  Total = sapply(names(numeric_df), function(name) {
    x <- clean_values(numeric_df[[name]], name)
    sum(!is.na(x))
  }),
  
  Mean = sapply(names(numeric_df), function(name) {
    x <- clean_values(numeric_df[[name]], name)
    mean(x, na.rm = TRUE)
  }),
  
  Median = sapply(names(numeric_df), function(name) {
    x <- clean_values(numeric_df[[name]], name)
    median(x, na.rm = TRUE)
  }),
  
  Mode = sapply(names(numeric_df), function(name) {
    x <- clean_values(numeric_df[[name]], name)
    get_mode(x)
  }),
  
  Minimum = sapply(names(numeric_df), function(name) {
    x <- clean_values(numeric_df[[name]], name)
    min(x, na.rm = TRUE)
  }),
  
  Maximum = sapply(names(numeric_df), function(name) {
    x <- clean_values(numeric_df[[name]], name)
    max(x, na.rm = TRUE)
  }),
  
  Range = sapply(names(numeric_df), function(name) {
    x <- clean_values(numeric_df[[name]], name)
    diff(range(x, na.rm = TRUE))
  }),
  
  SD = sapply(names(numeric_df), function(name) {
    x <- clean_values(numeric_df[[name]], name)
    sd(x, na.rm = TRUE)
  })
)

rownames(summary_table) <- NULL

print(summary_table)




sapply(df[!sapply(df, is.numeric)], class)
sapply(df, class)

#box plot all values zero values removed
# Create cleaned copy only for plotting
plot_df <- numeric_df

for (name in names(plot_df)) {
  if (name %in% zero_remove) {
    plot_df[[name]][plot_df[[name]] == 0] <- NA
  }
}

boxplot(
  plot_df,
  main = "Boxplots of Numeric Variables",
  las = 2
)


#note that Insulin seems to have a large number of outliers
#box plot adjusted for zero value removal
df %>%
  filter(Insulin != 0) %>%
  ggplot(aes(x = factor(1), y = Insulin)) +
  geom_violin() +
  geom_boxplot(width = 0.12) +
  coord_flip() +
  labs(
    title = "Distribution of Insulin",
    x = "",
    y = "Insulin"
  )














# Shapiro–Wilk normality test
#data does not apear to be normally distributed!! Although glucose and BMI is close - clear on visual plot
# Shapiro-Wilk normality test
# Zero values are excluded only for the variables listed above

for (col in names(df)) {
  
  if (is.numeric(df[[col]])) {
    
    x <- df[[col]]
    
    # Remove zero values only for selected variables
    if (col %in% zero_remove) {
      x <- x[x != 0]
    }
    
    # Remove NA values
    x <- na.omit(x)
    
    if (length(x) >= 3 && length(x) <= 5000) {
      
      test <- shapiro.test(x)
      
      if (test$p.value > 0.05) {
        cat(
          col, ": Normally distributed",
          "(W =", round(test$statistic, 5),
          ", p =", round(test$p.value, 5),
          ", n =", length(x), ")\n"
        )
      } else {
        cat(
          col, ": NOT normally distributed",
          "(W =", round(test$statistic, 5),
          ", p =", round(test$p.value, 5),
          ", n =", length(x), ")\n"
        )
      }
    }
  }
}








#code adapted from Source: Bader and Leuzinger (2024, Chapter 8)
# Create temporary copy for plotting
pairs_df <- df

# Apply same zero removal logic
for (col in names(pairs_df)) {
  if (col %in% zero_remove) {
    pairs_df[[col]][pairs_df[[col]] == 0] <- NA
  }
}

pairs.panels(
  pairs_df,
  method = "kendall",     # correlation method
  hist.col = "grey",      # histogram colour
  density = TRUE,         # show density curves
  ellipses = FALSE,       # remove correlation ellipses
  scale = TRUE,
  cex.cor = 2.5,
  main = "Fresh Data Inspection",
  pch = 19
)


#% of people with diabetes vs not
# % of people with diabetes vs not
diabetes <- round(prop.table(table(df$Outcome)) * 100, 2)

barplot(
  diabetes,
  names.arg = c("No Diabetes", "Has Diabetes"),
  main = "% of Individuals with Diabetes",
  ylab = "Percentage (%)",
  col = c("#B2DFDB", "#26A69A"),
  border = NA,
  ylim = c(0, 100)
)

# Add percentage labels above bars
text(
  x = c(0.7, 1.9),
  y = diabetes + 3,
  labels = paste0(diabetes, "%")
)


#age distribution

df_age <- df # Added extra data set for saftey - in case later operations change dataset


round(prop.table(table(df_age$Age)) * 100, 0)
stem(df$Age)

h <- hist(
  df_age$Age,
  plot = FALSE
)

h$counts <- h$counts / sum(h$counts) * 100

plot(
  h,
  main = "Age Distribution",
  xlab = "Age",
  ylab = "Percentage (%)",
  col = "#26A69A",
  border = "#B2DFDB"
)




#pregancy
# Copy original dataset
df_pregnacy <- df

# Create indicator only in new dataset
df_pregnacy$Was_pregnant <- ifelse(df_pregnacy$Pregnancies == 0, 0, 1)

# Remove original pregnancy count column from new dataset
df_pregnacy$Pregnancies <- NULL

#plot to bar
preg_table <- table(df_pregnacy$Was_pregnant)
preg_pct <- prop.table(preg_table) * 100

bp <- barplot(
  preg_pct,
  names.arg = c("Never Pregnant", "Was Pregnant"),
  main = "Pregnancy History",
  xlab = "Pregnancy History",
  ylab = "Percentage (%)",
  col = "#26A69A",
  border = "#B2DFDB",
  ylim = c(0, 100)
)

text(
  x = bp,
  y = preg_pct,
  labels = paste0(round(preg_pct, 1), "%"),
  pos = 3
)

#inspect new dataset
summary(df_pregnacy)
colSums(is.na(df_pregnacy))
colSums(df_pregnacy == 0) #zero values present!!! take note of this

sapply(df_pregnacy[!sapply(df_pregnacy, is.numeric)], class)
sapply(df_pregnacy, class)

#plot for corrolation
pairs.panels(df_pregnacy,
             main = "Scatterplot Matrix",
             pch = 19,
             cex = 0.5)

pairs.panels(
  data.frame(
    lapply(df_pregnacy, function(x) {
      if (is.factor(x)) as.numeric(x) else x
    })
  ),
  method = "kendall",
  hist.col = "grey",
  density = TRUE,
  ellipses = FALSE,
  scale = TRUE,
  cex.cor = 2.5,
  main = "Inspect Pregnancy",
  pch = 19
)



#binary functions
lapply(df_pregnacy[sapply(df_pregnacy, function(x) length(unique(na.omit(x))) == 2)], unique)

# Contingency table
binary <- table(
  df_pregnacy$Outcome,
  df_pregnacy$Was_pregnant
)

binary

prop.table(binary, margin = 2) * 100

chisq.test(binary)

phi(binary)

#make nice plot fo pregnacy ever vs diabetease
df_pregnacy$Was_pregnant <- factor(
  df_pregnacy$Was_pregnant,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

df_pregnacy$Outcome <- factor(
  df_pregnacy$Outcome,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

ggplot(df_pregnacy, aes(x = Was_pregnant, fill = Outcome)) +
  geom_bar(position = "fill", width = 0.65) +
  geom_text(
    stat = "count",
    aes(
      label = after_stat(
        paste0(
          sprintf("%.2f", count / ave(count, x, FUN = sum) * 100),
          "%"
        )
      )
    ),
    position = position_fill(vjust = 0.5),
    colour = "white",
    fontface = "bold"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = c("#66D9CC", "#008C95"),
    name = "Diabetic"
  ) +
  labs(
    title = "Diabetic Vs Previously Pregnant",
    x = "Previously pregnant",
    y = "Percentage"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "top"
  )


#corrolation test weak correlation ρ = 0.231 p-value = 8.98 × 10⁻¹¹
cor.test(df$Glucose, df$BMI, method = "spearman")


# τ = 0.156 p-value=1.38 × 10⁻¹⁰
cor.test(df$Glucose, df$BMI, method = "kendall")

# find relationships accounting for other variab
set.seed(123)

#annova
df[] <- lapply(df, function(x) {
  if (is.factor(x) || is.character(x)) {
    as.numeric(as.factor(x))
  } else {
    as.numeric(x)
  }
})

summary(
  aov(
    Outcome ~ .,
    data = data.frame(lapply(df, function(x) {
      if (is.factor(x) || is.character(x)) {
        as.numeric(as.factor(x))
      } else {
        as.numeric(x)
      }
    }))
  )
)



sapply(df, class) #seems to be issue here, at some point df become numeric is something changing it? 



#train data with split

# Random 70/30 split
index <- sample(
  1:nrow(df),
  size = 0.7 * nrow(df)
)

trainSplit <- df[index, ]
testSplit <- df[-index, ]


# box-cox unsutible as outcome is binary 

# Fit model using training data only
model <- glm(
  Outcome ~ .,
  data = trainSplit,
  family = binomial
)

# Predict probabilities on unseen test data
predictProb <- predict(
  model,
  testSplit,
  type = "response"
)

# Convert probabilities to classes
predictTest <- ifelse(
  predictProb >= 0.5,
  1,
  0
)

# Confusion matrix
table(
  Predicted = predictTest,
  Actual = testSplit$Outcome
)


#accuracy of model predictions
round(mean(predictTest == testSplit$Outcome) * 100)

#baseline prediction
round(
  max(prop.table(table(testSplit$Outcome))) * 100
)


#decision tree and plot
# Decision tree
tree_model <- rpart(
  Outcome ~ .,
  data = trainSplit,
  method = "class"
)

# Nice tree plot
rpart.plot(
  tree_model,
  type = 4,
  extra = 104,
  fallen.leaves = TRUE,
  tweak = 1.1
)



# Difference in glucose levels between diabetic and non-diabetic groups
# Glucose is not normally distributed!


# Descriptive statistics
df %>%
  group_by(Outcome) %>%
  summarise(
    n = sum(!is.na(Glucose)),
    Median = median(Glucose, na.rm = TRUE),
    Q1 = quantile(Glucose, 0.25, na.rm = TRUE),
    Q3 = quantile(Glucose, 0.75, na.rm = TRUE),
    IQR = IQR(Glucose, na.rm = TRUE)
  )

# Visual comparison
boxplot(Glucose ~ Outcome,
        data = df,
        names = c("Not diabetic", "Diabetic"),
        main = "Glucose Levels by Diabetes Outcome",
        xlab = "Diabetes Outcome",
        ylab = "Glucose")

# Mann-Whitney U test
wilcox.test(Glucose ~ Outcome,
            data = df,
            exact = FALSE)



#diabetease pregnacy
# Descriptive statistics - number of pregnacies
df %>%
  group_by(Outcome) %>%
  summarise(
    n = sum(!is.na(Pregnancies)),
    Median = median(Pregnancies, na.rm = TRUE),
    Q1 = quantile(Pregnancies, 0.25, na.rm = TRUE),
    Q3 = quantile(Pregnancies, 0.75, na.rm = TRUE),
    IQR = IQR(Pregnancies, na.rm = TRUE)
  )

boxplot(
  Pregnancies ~ Outcome,
  data = df,
  names = c("Not diabetic", "Diabetic"),
  main = "Number of Pregnancies by Diabetes Outcome",
  xlab = "Diabetes Outcome",
  ylab = "Number of Pregnancies"
)

# Calculate group means
means <- tapply(df$Pregnancies, df$Outcome, mean, na.rm = TRUE)

# Add means to boxplot
points(
  1:2,
  means,
  pch = 19
)

# Print mean values above the points
text(
  1:2,
  means,
  labels = round(means, 2),
  pos = 3
)boxplot(
  Pregnancies ~ Outcome,
  data = df,
  names = c("Not diabetic", "Diabetic"),
  main = "Number of Pregnancies by Diabetes Outcome",
  xlab = "Diabetes Outcome",
  ylab = "Number of Pregnancies"
)

# Calculate group means
means <- tapply(df$Pregnancies, df$Outcome, mean, na.rm = TRUE)

# Add means to boxplot
points(
  1:2,
  means,
  pch = 19
)

# Print mean values above the points
text(
  1:2,
  means,
  labels = round(means, 2),
  pos = 3
)


#Mann-U Whintney test
wilcox.test(
  Pregnancies ~ Outcome,
  data = df,
  exact = FALSE
)


#binary ever pregnany vs not
# Was ever pregnant vs diabetes outcome

pregnant_table <- table(
  df_pregnacy$Was_pregnant,
  df_pregnacy$Outcome
)

pregnant_table

# Convert to percentages within pregnancy groups
prop.table(pregnant_table, margin = 1) * 100

# Chi-square test
chisq.test(pregnant_table)

barplot(
  prop.table(pregnant_table, margin = 1),
  beside = TRUE,
  names.arg = c("Never pregnant", "Was pregnant"),
  legend.text = c("Not diabetic", "Diabetic"),
  main = "Diabetes Outcome by Pregnancy History",
  xlab = "Pregnancy History",
  ylab = "Proportion"
)


#correlation test pregnacy number vs if ever

# Pregnancy count vs diabetes outcome
cor(
  df$Pregnancies,
  df$Outcome,
  method = "spearman",
  use = "complete.obs"
)

# Ever pregnant vs diabetes outcome
cor(
  df_pregnacy$Was_pregnant,
  df_pregnacy$Outcome,
  method = "pearson",
  use = "complete.obs"
)


# MODEL 1 Number of pregnancies
model_count <- glm(
  Outcome ~ Pregnancies + Glucose + BloodPressure +
    SkinThickness + Insulin + BMI +
    DiabetesPedigreeFunction + Age,
  data = df,
  family = binomial
)

# Pregnancy effect
exp(cbind(
  OR = coef(model_count)["Pregnancies"],
  confint(model_count)["Pregnancies", ]
))



# MODEL 2 Ever pregnant
model_binary <- glm(
  Outcome ~ Was_pregnant + Glucose + BloodPressure +
    SkinThickness + Insulin + BMI +
    DiabetesPedigreeFunction + Age,
  data = df_pregnacy,
  family = binomial
)

# Ever-pregnant effect
exp(cbind(
  OR = coef(model_binary)["Was_pregnant"],
  confint(model_binary)["Was_pregnant", ]
))



#visualize
# -------------------------
# MODEL 1: pregnancy count
# -------------------------

new_count <- data.frame(
  Pregnancies = 0:max(df$Pregnancies, na.rm = TRUE),
  Glucose = mean(df$Glucose, na.rm = TRUE),
  BloodPressure = mean(df$BloodPressure, na.rm = TRUE),
  SkinThickness = mean(df$SkinThickness, na.rm = TRUE),
  Insulin = mean(df$Insulin, na.rm = TRUE),
  BMI = mean(df$BMI, na.rm = TRUE),
  DiabetesPedigreeFunction = mean(df$DiabetesPedigreeFunction, na.rm = TRUE),
  Age = mean(df$Age, na.rm = TRUE)
)

new_count$Probability <- predict(
  model_count,
  newdata = new_count,
  type = "response"
)

ggplot(new_count, aes(x = Pregnancies, y = Probability)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Adjusted Diabetes Probability by Number of Pregnancies",
    x = "Number of Pregnancies",
    y = "Predicted probability of diabetes"
  ) +
  theme_minimal()





#convert data set to catagories, one by one


#BMI catagory

df_bmi <- df
df_bmi$bmi_cat <- ordered(
  cut(
    df_bmi$BMI,
    breaks = c(-Inf, 18.5, 25, 30, Inf),
    labels = c("Underweight", "Normal", "Overweight", "Obese")
  )
)

#remove redundant bmi cat
# Remove original BMI count column from new dataset
df_bmi$BMI <- NULL

sapply(df_bmi, class)

str(df_bmi)

round(prop.table(table(df_bmi$Outcome)) * 100, 0)


#age catagory

df_age_bmi <- df_bmi

df_age_bmi$age_cat <- ordered(
  cut(
    df_age_bmi$Age,
    breaks = c(-Inf, 30, 40, 50, Inf),
    labels = c(
      "Under 30",
      "30–39",
      "40–49",
      "50+"
    ),
    right = FALSE
  )
)

#remove redundant Age cat
# Remove original BMI count column from new dataset
df_age_bmi$Age <- NULL

sapply(df_age_bmi, class)

str(df_age_bmi)



#Pregancy Catagory

pregnancies_bmi_age <- df_age_bmi

#add catagories BMI
pregnancies_bmi_age$pregnancy_cat <- ordered(
  cut(
    pregnancies_bmi_age$Pregnancies,
    breaks = c(-Inf, 0, 2, 4, Inf),
    labels = c("None", "1-2", "3-4", "5+"),
    right = TRUE
  )
)

#remove redundant Pregnancies variable
# Remove original Pregnancies count column from new dataset
pregnancies_bmi_age$Pregnancies <- NULL

sapply(pregnancies_bmi_age, class)

str(pregnancies_bmi_age)



#compare catagories

# Age category vs Outcome
tab_age <- table(
  pregnancies_bmi_age$age_cat,
  pregnancies_bmi_age$Outcome
)

tab_age
chisq.test(tab_age)


# Pregnancy category vs Outcome
tab_pregnancy <- table(
  pregnancies_bmi_age$pregnancy_cat,
  pregnancies_bmi_age$Outcome
)

tab_pregnancy
chisq.test(tab_pregnancy)


# BMI category vs Outcome
tab_bmi <- table(
  pregnancies_bmi_age$bmi_cat,
  pregnancies_bmi_age$Outcome
)

tab_bmi
chisq.test(tab_bmi)


prop.table(tab_age, margin = 1) * 100

prop.table(tab_pregnancy, margin = 1) * 100

prop.table(tab_bmi, margin = 1) * 100



#plot to graph
#this graph does not account for the fact that each catagory has a different effect size and therefore should be split up
par(mfrow = c(1, 3))

# Age
barplot(
  t(prop.table(tab_age, margin = 1) * 100),
  beside = FALSE,
  main = "Diabetes by Age",
  xlab = "Age category",
  ylab = "Percentage",
  legend.text = c("Not diabetic", "Diabetic"),
  ylim = c(0, 100)
)

# Pregnancy
barplot(
  t(prop.table(tab_pregnancy, margin = 1) * 100),
  beside = FALSE,
  main = "Diabetes by Pregnancies",
  xlab = "Pregnancy category",
  ylab = "Percentage",
  legend.text = c("Not diabetic", "Diabetic"),
  ylim = c(0, 100)
)

# BMI
barplot(
  t(prop.table(tab_bmi, margin = 1) * 100),
  beside = FALSE,
  main = "Diabetes by BMI",
  xlab = "BMI category",
  ylab = "Percentage",
  legend.text = c("Not diabetic", "Diabetic"),
  ylim = c(0, 100)
)

par(mfrow = c(1, 1))




#individual catagories

#BMI low confidence suggests that there is not a large enough datset.
# Create separate dataset
bmi_model_data <- pregnancies_bmi_age

# Make factors unordered
bmi_model_data$bmi_cat <- factor(
  bmi_model_data$bmi_cat,
  ordered = FALSE
)

bmi_model_data$age_cat <- factor(
  bmi_model_data$age_cat,
  ordered = FALSE
)

bmi_model_data$pregnancy_cat <- factor(
  bmi_model_data$pregnancy_cat,
  ordered = FALSE
)

# Set reference category
bmi_model_data$bmi_cat <- relevel(
  bmi_model_data$bmi_cat,
  ref = "Normal"
)

# Logistic regression
bmi_model <- glm(
  Outcome ~ bmi_cat + age_cat + pregnancy_cat,
  data = bmi_model_data,
  family = binomial
)

# Extract BMI odds ratios
bmi_results <- tidy(
  bmi_model,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  filter(grepl("^bmi_cat", term)) %>%
  mutate(
    Category = sub("bmi_cat", "", term)
  )

print(bmi_results)

# Plot
ggplot(
  bmi_results,
  aes(x = reorder(Category, estimate), y = estimate)
) +
  geom_col(width = 0.65) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.15
  ) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed"
  ) +
  coord_flip() +
  labs(
    title = "BMI Category and Diabetes Outcome",
    subtitle = "Reference category: Normal BMI",
    x = "BMI category",
    y = "Adjusted Odds Ratio (95% CI)"
  ) +
  theme_minimal()




#Age ConfidencePlot
#less uncertanty, however still not great

# Create separate dataset
age_model_data <- pregnancies_bmi_age

# Make factors unordered
age_model_data$age_cat <- factor(
  age_model_data$age_cat,
  ordered = FALSE
)

age_model_data$bmi_cat <- factor(
  age_model_data$bmi_cat,
  ordered = FALSE
)

age_model_data$pregnancy_cat <- factor(
  age_model_data$pregnancy_cat,
  ordered = FALSE
)

# Set reference category
age_model_data$age_cat <- relevel(
  age_model_data$age_cat,
  ref = "Under 30"
)

# Logistic regression
age_model <- glm(
  Outcome ~ age_cat + bmi_cat + pregnancy_cat,
  data = age_model_data,
  family = binomial
)

# Extract age odds ratios
age_results <- tidy(
  age_model,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  filter(grepl("^age_cat", term)) %>%
  mutate(
    Category = sub("age_cat", "", term)
  )

print(age_results)

# Plot
ggplot(
  age_results,
  aes(x = reorder(Category, estimate), y = estimate)
) +
  geom_col(width = 0.65) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.15
  ) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed"
  ) +
  coord_flip() +
  labs(
    title = "Age Category and Diabetes Outcome",
    subtitle = "Reference category: Under 30",
    x = "Age category",
    y = "Adjusted Odds Ratio (95% CI)"
  ) +
  theme_minimal()




#Pregancies confidence interval
#less uncertanty, more evenly weighted

# Create separate dataset
pregnancy_model_data <- pregnancies_bmi_age

# Make factors unordered
pregnancy_model_data$pregnancy_cat <- factor(
  pregnancy_model_data$pregnancy_cat,
  ordered = FALSE
)

pregnancy_model_data$bmi_cat <- factor(
  pregnancy_model_data$bmi_cat,
  ordered = FALSE
)

pregnancy_model_data$age_cat <- factor(
  pregnancy_model_data$age_cat,
  ordered = FALSE
)

# Set reference category
pregnancy_model_data$pregnancy_cat <- relevel(
  pregnancy_model_data$pregnancy_cat,
  ref = "None"
)

# Logistic regression
pregnancy_model <- glm(
  Outcome ~ pregnancy_cat + bmi_cat + age_cat,
  data = pregnancy_model_data,
  family = binomial
)

# Extract pregnancy odds ratios
pregnancy_results <- tidy(
  pregnancy_model,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  filter(grepl("^pregnancy_cat", term)) %>%
  mutate(
    Category = sub("pregnancy_cat", "", term)
  )

print(pregnancy_results)

# Plot
ggplot(
  pregnancy_results,
  aes(x = reorder(Category, estimate), y = estimate)
) +
  geom_col(width = 0.65) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.15
  ) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed"
  ) +
  coord_flip() +
  labs(
    title = "Pregnancy Category and Diabetes Outcome",
    subtitle = "Reference category: No pregnancies",
    x = "Pregnancy category",
    y = "Adjusted Odds Ratio (95% CI)"
  ) +
  theme_minimal()






#what is this - check later
round(prop.table(table(df_bmi$Outcome)) * 100, 0)

diabetes <- round(prop.table(table(df_bmi$Outcome)) * 100, 0)

pie(diabetes,
    labels = paste0(c("No diabetes", "Has diabetes"), ": ", diabetes, "%"),
    main = "diabetes")