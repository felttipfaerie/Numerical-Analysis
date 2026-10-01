# Coding-assistant note: OpenAI Codex assisted with coding. The project owner made the key decisions on how to segment and manage the data.
# Dataset note: Uses the Health Data SPSS dataset. It is intentionally excluded from Git; obtain it from the project owner and place it at unit-8/Health Data.sav. The project does not record a public download source for this file.
library(haven)
Health_Data <- read_sav("Health Data.sav")
View(Health_Data)

#mode median mean
d1 <- Health_Data
summary(d1$age)
range(d1$age, na.rm = TRUE)

#fuction to get mode
get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

get_mode(d1$age)



# diabetes vs dbp  no significant difference found

# median - 83 vs 82
tapply(d1$dbp,
       d1$diabetes,
       median,
       na.rm = TRUE)


#box plot difference between systolic blood pressure by diabeates - seems visually insignificant
boxplot(sbp ~ diabetes,
        +         data = d1,
        +         main = "systolic blood pressure by diabetes",
        +         xlab = "systolic blood pressure",
        +         ylab = "diabetes",
        +         col = "lightblue")

#wilcox test
wilcox.test(sbp ~ diabetes,
            data = d1,
            exact = FALSE)

#t test

d1$diabetes <- factor(d1$diabetes)

t.test(sbp ~ diabetes, data = d1)

# occupation data does not suggest strong correlation
tapply(d1$dbp,
       d1$occupation,
       median,
       na.rm = TRUE)

# Anova - no significant difference
model <- aov(sbp ~ occupation, data = d1)
summary(model)

# Welch’s ANOVA - no significant difference
oneway.test(sbp ~ occupation,
          data = d1,
          var.equal = FALSE)
# box plot
boxplot(sbp ~ occupation,
        data = d1,
        main = "Systolic blood pressure by Occupation",
        xlab = "Occupation",
        ylab = "Systolic blood pressure",
        col = "lightblue")
