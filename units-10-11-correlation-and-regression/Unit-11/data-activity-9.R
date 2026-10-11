library(haven)
Health_Data <- read_sav("~/GitHub/Numerical Analysis/units-6-8-seminars-and-visualisation/unit-8/Health Data.sav")

df <- Health_Data

model <- lm(dbp ~ sbp, data = df)
summary(model)

#p-value is considerably less than 0.05, as such must reject the null hypothisis that the two variables are un related
#strong evidence
# Multiple R-squared:  0.7171 shows a 72% explained by sbp
#F(1, 208) = 527.2, p < .001 F>1 model is statistically significant
