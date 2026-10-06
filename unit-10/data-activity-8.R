# Coding-assistant note: OpenAI Codex assisted with coding. The project owner made the key decisions on how to segment and manage the data.
# Dataset note: Uses the Sample Superstore sales training data at unit-8/seminar/train.csv in this repository. A public compatible download is available at https://www.kaggle.com/datasets/vivek468/superstore-dataset-final

library(haven)
Health_Data <- read_sav("~/GitHub/Numerical Analysis/unit-8/Health Data.sav")

df <- Health_Data

#correclation test spearman for rank, pearson test for values - both suggest strong correlation
cor(df$dbp, df$sbp, method = "spearman")

cor(df$dbp, df$sbp, method = "pearson")

cor.test(df$dbp, df$sbp, method = "spearman")

cor.test(df$dbp, df$sbp, method = "pearson")


#scattter plot
plot(df$dbp, df$sbp,
     xlab = "Diastolic Blood Pressure (DBP)",
     ylab = "Systolic Blood Pressure (SBP)",
     main = "Pearson Scatter Plot of DBP vs SBP")

abline(lm(sbp ~ dbp, data = df))

plot(df$dbp, df$sbp,
     xlab = "Diastolic Blood Pressure",
     ylab = "Systolic Blood Pressure",
     main = "Spearman Scatter Plot of DBP vs")

lines(lowess(df$dbp, df$sbp))