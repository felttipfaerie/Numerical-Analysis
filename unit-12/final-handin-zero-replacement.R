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
library(scales)
library(rcompanion)


akshaydattatraykhare_diabetes_dataset <- read_csv("akshaydattatraykhare-diabetes-dataset.csv")

df <- akshaydattatraykhare_diabetes_dataset

#inspect dataset
summary(df)

#there apear to be missing values that have been filled with 0
colSums(is.na(df))
colSums(df == 0)

#code adapted from Source: Bader and Leuzinger (2024, Chapter 8)
#zero-value replacement change

#--------------------------------------------------------------------------------------------------
# SkinThickness demonstrated a considerable association with BMI (Kendall's tau = 0.33). And had 227 zero values - treated as missing
# Insulin has 374 zero values (48.7% of 768 observations), treated as missing in the adjusted data.
#--------------------------------------------------------------------------------------------------

# Create a raw-data plot before zero values are replaced and columns are removed.
pairs_df_before_adjustment <- df

pairs.panels(
  pairs_df_before_adjustment,
  method = "kendall",     # correlation method
  hist.col = "grey",      # histogram colour
  density = TRUE,          # show density curves
  ellipses = FALSE,        # remove correlation ellipses
  scale = TRUE,
  cex.cor = 2.5,
  main = "Unedited Data Inspection: Before Zero-Value Replacement",
  pch = 19
)

#full summary table
# Function to calculate mode
get_mode <- function(x) {
  x <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#zero-value replacement change
zero_remove <- c("Glucose", "BloodPressure", "BMI")

#zero-value replacement change
zero_replacement_counts <- colSums(df[zero_remove] == 0)

#zero-value replacement change
df_zero_replaced <- df
#zero-value replacement change
# Missing values remain as NA and are not imputed with replacement values.
df_zero_replaced[zero_remove] <- lapply(df_zero_replaced[zero_remove], function(x) {
  x[x == 0] <- NA
  x
})

#zero-value replacement change
df_zero_replaced$Insulin <- NULL

#zero-value replacement change
df_zero_replaced$SkinThickness <- NULL

#zero-value replacement change
adjustment_caption <- paste0(
  "Adjusted data: 0 -> NA (",
  paste(names(zero_replacement_counts), zero_replacement_counts,
    sep = "=", collapse = "; "),
  "); Insulin and SkinThickness removed"
)

#zero-value replacement change
df <- df_zero_replaced
#zero-value replacement change
# Summaries and plots omit NA values where required; each model uses complete cases for its included variables.

# Select numeric columns only
numeric_df <- df[sapply(df, is.numeric)]

#create summary plot, remove zeros
num_cols <- names(df)[sapply(df, is.numeric)]



#visual predictor corolation plot
#code adapted from Source: Bader and Leuzinger (2024, Chapter 8)
# Create temporary copy for plotting
pairs_df <- df

pairs.panels(
  pairs_df,
  method = "kendall",     # correlation method
  hist.col = "grey",      # histogram colour
  density = TRUE,         # show density curves
  ellipses = FALSE,       # remove correlation ellipses
  scale = TRUE,
  cex.cor = 2.5,
  #zero-value replacement change
  main = paste("Overview of data"),
  sub = paste(adjustment_caption),
  pch = 19
)

#Table data
cor_table$EffectSize <- cut(
  abs(cor_table$Kendall_tau),
  breaks = c(-Inf, 0.1, 0.3, 0.5, Inf),
  labels = c("Negligible", "Weak", "Moderate", "Strong")
)
rownames(cor_table) <- NULL
cor_table




#zero-value replacement change
par(mfrow = c(3, 3), oma = c(2, 0, 0, 0))

for (col in num_cols) {
  
  x <- df[[col]]
  
  #zero-value replacement change
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

#zero-value replacement change
mtext(adjustment_caption, side = 1, outer = TRUE, line = 0.5, cex = 0.75)

#zero-value replacement change
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))





# Create summary table for all values, excluding the zero value for glucose, blood pressure, skin thickness insulin bmi


# Function to remove zeros only where needed
#zero-value replacement change
clean_values <- function(x, name) {
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


#check variable type
sapply(df[!sapply(df, is.numeric)], class)
sapply(df, class)



#box plot remaining predictors

# Create cleaned copy only for plotting
plot_df <- numeric_df

# Remove binary outcome variable
plot_df$Outcome <- NULL

par(mar = c(6, 11, 4, 2) + 0.1)

boxplot(
  plot_df,
  main = "Boxplots of Predictors",
  horizontal = TRUE,
  las = 1,
  xlab = "Value",
  ylab = "",
  cex.axis = 0.8
)

mtext(
  adjustment_caption,
  side = 1,
  line = 5,
  cex = 0.55
)


# Shapiro–Wilk normality test
#data does not apear to be normally distributed!! Although glucose and BMI is close - clear on visual plot
# Zero values are excluded only for the variables listed above

for (col in names(df)) {
  
  if (is.numeric(df[[col]])) {
    
    x <- df[[col]]
    
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



#-----------
# Start questions
#-----------


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
  #zero-value replacement change
  sub = adjustment_caption,
  #zero-value replacement change
  cex.sub = 0.55,
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
  #zero-value replacement change
  sub = adjustment_caption,
  #zero-value replacement change
  cex.sub = 0.55,
  border = "#B2DFDB"
)




#pregancy
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
  #zero-value replacement change
  sub = adjustment_caption,
  #zero-value replacement change
  cex.sub = 0.55,
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
#zero-value replacement change
sapply(df_pregnacy[!sapply(df_pregnacy, is.numeric)], class)
sapply(df_pregnacy, class)





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
    y = "Percentage",
    #zero-value replacement change
    caption = adjustment_caption
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "top"
  )



#------------------------------------------
#convert data set to catagories, one by one
#------------------------------------------

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
cramerV(tab_age)

# Pregnancy category vs Outcome
tab_pregnancy <- table(
  pregnancies_bmi_age$pregnancy_cat,
  pregnancies_bmi_age$Outcome
)

tab_pregnancy
chisq.test(tab_pregnancy)
cramerV(tab_pregnancy)

# BMI category vs Outcome
tab_bmi <- table(
  pregnancies_bmi_age$bmi_cat,
  pregnancies_bmi_age$Outcome
)

tab_bmi
chisq.test(tab_bmi)
cramerV(tab_bmi)

prop.table(tab_age, margin = 1) * 100

prop.table(tab_pregnancy, margin = 1) * 100

prop.table(tab_bmi, margin = 1) * 100



#plot to graph
#zero-value replacement change

#Age

# Convert age table to dataframe
df_age_plot <- as.data.frame(tab_age)

colnames(df_age_plot) <- c("Age", "Outcome", "Count")

ggplot(df_age_plot, aes(x = Age, y = Count, fill = Outcome)) +
  geom_col(
    position = "fill",
    width = 0.65
  ) +
  geom_text(
    aes(
      label = paste0(
        sprintf(
          "%.2f",
          Count / ave(Count, Age, FUN = sum) * 100
        ),
        "%"
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
    title = "Diabetes by Age",
    x = "Age category",
    y = "Percentage",
    caption = adjustment_caption
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    legend.position = "top"
  )


# PREGNANCY

df_pregnancy_plot <- as.data.frame(tab_pregnancy)

colnames(df_pregnancy_plot) <- c("Pregnancy", "Outcome", "Count")

ggplot(df_pregnancy_plot, aes(x = Pregnancy, y = Count, fill = Outcome)) +
  geom_col(
    position = "fill",
    width = 0.65
  ) +
  geom_text(
    aes(
      label = paste0(
        sprintf(
          "%.2f",
          Count / ave(Count, Pregnancy, FUN = sum) * 100
        ),
        "%"
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
    y = "Percentage",
    caption = adjustment_caption
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    legend.position = "top"
  )



# BMI

df_bmi_plot <- as.data.frame(tab_bmi)

colnames(df_bmi_plot) <- c("BMI", "Outcome", "Count")

ggplot(df_bmi_plot, aes(x = BMI, y = Count, fill = Outcome)) +
  geom_col(
    position = "fill",
    width = 0.65
  ) +
  geom_text(
    aes(
      label = paste0(
        sprintf(
          "%.2f",
          Count / ave(Count, BMI, FUN = sum) * 100
        ),
        "%"
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
    title = "Diabetes by BMI",
    x = "BMI category",
    y = "Percentage",
    caption = adjustment_caption
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    legend.position = "top"
  )



#compare predictors to each other

# Function to run chi-square + Cramer's V
compare_categories <- function(var1, var2, name1, name2) {
  
  tab <- table(var1, var2)
  chi <- chisq.test(tab)
  
  data.frame(
    Variable_1 = name1,
    Variable_2 = name2,
    Chi_square = round(unname(chi$statistic), 3),
    df = unname(chi$parameter),
    p_value = round(chi$p.value, 4),
    Cramers_V = round(cramerV(tab), 3)
  )
}


# Run all comparisons
results <- rbind(
  
  compare_categories(
    pregnancies_bmi_age$age_cat,
    pregnancies_bmi_age$Outcome,
    "Age category",
    "Outcome"
  ),
  
  compare_categories(
    pregnancies_bmi_age$pregnancy_cat,
    pregnancies_bmi_age$Outcome,
    "Pregnancy category",
    "Outcome"
  ),
  
  compare_categories(
    pregnancies_bmi_age$bmi_cat,
    pregnancies_bmi_age$Outcome,
    "BMI category",
    "Outcome"
  ),
  
  compare_categories(
    pregnancies_bmi_age$age_cat,
    pregnancies_bmi_age$pregnancy_cat,
    "Age category",
    "Pregnancy category"
  ),
  
  compare_categories(
    pregnancies_bmi_age$age_cat,
    pregnancies_bmi_age$bmi_cat,
    "Age category",
    "BMI category"
  ),
  
  compare_categories(
    pregnancies_bmi_age$pregnancy_cat,
    pregnancies_bmi_age$bmi_cat,
    "Pregnancy category",
    "BMI category"
  )
)

results$Strength <- cut(
  results$Cramers_V,
  breaks = c(-Inf, 0.1, 0.3, 0.5, Inf),
  labels = c(
    "Very weak",
    "Weak",
    "Moderate",
    "Strong"
  )
)

results$Significant <- ifelse(
  results$p_value < 0.05,
  "Yes",
  "No"
)

results


#results show correlation between age andnumber of pregnacies
# Make sure variables are treated as categorical factors
pregnancies_bmi_age$age_cat <- factor(pregnancies_bmi_age$age_cat)
pregnancies_bmi_age$pregnancy_cat <- factor(pregnancies_bmi_age$pregnancy_cat)

# Outcome should be binary
# If Outcome is already coded as 0/1, this is fine:
pregnancies_bmi_age$Outcome <- as.numeric(
  as.character(pregnancies_bmi_age$Outcome)
)


# ------------------------------------------------------------
# 1. Multivariable logistic regression
# ------------------------------------------------------------

model_age_preg <- glm(
  Outcome ~ age_cat + pregnancy_cat,
  data = pregnancies_bmi_age,
  family = binomial
)

# Full model output
summary(model_age_preg)


# ------------------------------------------------------------
# 2. Overall model significance
# ------------------------------------------------------------

null_model <- glm(
  Outcome ~ 1,
  data = pregnancies_bmi_age,
  family = binomial
)

anova(
  null_model,
  model_age_preg,
  test = "Chisq"
)


# ------------------------------------------------------------
# 3. Test each predictor after adjusting for the other
# ------------------------------------------------------------

adjusted_tests <- drop1(
  model_age_preg,
  test = "Chisq"
)

adjusted_tests


# ------------------------------------------------------------
# 4. Adjusted odds ratios and 95% confidence intervals
# ------------------------------------------------------------

library(broom)

adjusted_OR <- tidy(
  model_age_preg,
  exponentiate = TRUE,
  conf.int = TRUE
)

adjusted_OR


# ------------------------------------------------------------
# 5. Make a cleaner odds-ratio table
# ------------------------------------------------------------

adjusted_OR_table <- adjusted_OR[, c(
  "term",
  "estimate",
  "conf.low",
  "conf.high",
  "p.value"
)]

names(adjusted_OR_table) <- c(
  "Predictor",
  "Adjusted_OR",
  "CI_lower",
  "CI_upper",
  "p_value"
)

# Round values
adjusted_OR_table$Adjusted_OR <- round(
  adjusted_OR_table$Adjusted_OR, 3
)

adjusted_OR_table$CI_lower <- round(
  adjusted_OR_table$CI_lower, 3
)

adjusted_OR_table$CI_upper <- round(
  adjusted_OR_table$CI_upper, 3
)

adjusted_OR_table$p_value <- round(
  adjusted_OR_table$p_value, 4
)

adjusted_OR_table


# ------------------------------------------------------------
# 6. Create a clean table for the overall adjusted tests
# ------------------------------------------------------------

adjusted_test_table <- data.frame(
  Predictor = c(
    "Age category",
    "Pregnancy category"
  ),
  
  df = c(
    adjusted_tests["age_cat", "Df"],
    adjusted_tests["pregnancy_cat", "Df"]
  ),
  
  LRT = c(
    adjusted_tests["age_cat", "LRT"],
    adjusted_tests["pregnancy_cat", "LRT"]
  ),
  
  p_value = c(
    adjusted_tests["age_cat", "Pr(>Chi)"],
    adjusted_tests["pregnancy_cat", "Pr(>Chi)"]
  )
)

adjusted_test_table$LRT <- round(
  adjusted_test_table$LRT, 3
)

adjusted_test_table$p_value <- round(
  adjusted_test_table$p_value, 4
)

adjusted_test_table$Significant <- ifelse(
  adjusted_test_table$p_value < 0.05,
  "Yes",
  "No"
)

adjusted_test_table





# Difference in glucose levels between diabetic and non-diabetic groups

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

# Mann–Whitney U test
wilcox.test(
  Glucose ~ Outcome,
  data = df,
  exact = FALSE
)

# Effect size
df %>%
  rstatix::wilcox_effsize(
    Glucose ~ Outcome,
    ci = TRUE
  )

# Distribution by diabetes outcome
ggplot(df, aes(x = factor(Outcome), y = Glucose)) +
  geom_boxplot(outlier.shape = NA, width = 0.45) +
  geom_jitter(width = 0.12, height = 0, alpha = 0.35, size = 1.5) +
  labs(
    title = "Glucose Levels by Diabetes Outcome",
    x = "Diabetes Outcome",
    y = "Glucose",
    caption = adjustment_caption
  ) +
  scale_x_discrete(
    labels = c("0" = "Not diabetic",
               "1" = "Diabetic")
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(size = 7)
  )

# Difference in pregnancy counts between diabetic and non-diabetic groups

# Descriptive statistics
df %>%
  group_by(Outcome) %>%
  summarise(
    n = sum(!is.na(Pregnancies)),
    Median = median(Pregnancies, na.rm = TRUE),
    Q1 = quantile(Pregnancies, 0.25, na.rm = TRUE),
    Q3 = quantile(Pregnancies, 0.75, na.rm = TRUE),
    IQR = IQR(Pregnancies, na.rm = TRUE)
  )

# Mann–Whitney U test
wilcox.test(
  Pregnancies ~ Outcome,
  data = df,
  exact = FALSE
)

# Effect size
df %>%
  rstatix::wilcox_effsize(
    Pregnancies ~ Outcome,
    ci = TRUE
  )

# Distribution by diabetes outcome
ggplot(df, aes(x = factor(Outcome), y = Pregnancies)) +
  geom_boxplot(outlier.shape = NA, width = 0.45) +
  geom_jitter(width = 0.12, height = 0, alpha = 0.35, size = 1.5) +
  labs(
    title = "Pregnancies by Diabetes Outcome",
    x = "Diabetes Outcome",
    y = "Number of Pregnancies",
    caption = adjustment_caption
  ) +
  scale_x_discrete(
    labels = c("0" = "Not diabetic",
               "1" = "Diabetic")
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(size = 7)
  )








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
    y = "Adjusted Odds Ratio (95% CI)",
    #zero-value replacement change
    caption = adjustment_caption
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
    y = "Adjusted Odds Ratio (95% CI)",
    #zero-value replacement change
    caption = adjustment_caption
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
    y = "Adjusted Odds Ratio (95% CI)",
    #zero-value replacement change
    caption = adjustment_caption
  ) +
  theme_minimal()



#----------
#Training a model
#-----------------
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

#zero-value replacement change
valid_predictions <- !is.na(predictTest) & !is.na(testSplit$Outcome)
#zero-value replacement change
# Prediction accuracy and the confusion matrix exclude only test records without a usable prediction.

# Confusion matrix
#zero-value replacement change
cat(
  "Predictions evaluated for", sum(valid_predictions),
  "of", nrow(testSplit), "test records.\n"
)

table(
  Predicted = predictTest[valid_predictions],
  Actual = testSplit$Outcome[valid_predictions]
)


#accuracy of model predictions
#zero-value replacement change
round(mean(predictTest[valid_predictions] == testSplit$Outcome[valid_predictions]) * 100)

#baseline prediction
round(
  max(prop.table(table(testSplit$Outcome))) * 100
)












#----------------------
#decision tree and plot
#----------------------
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
  #zero-value replacement change
  main = paste("Decision Tree", adjustment_caption, sep = "\n"),
  tweak = 1.1
)

