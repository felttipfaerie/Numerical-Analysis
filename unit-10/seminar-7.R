library(readr)
Covid19_India_Jan_20_Mar_20_ <- read_csv("~/GitHub/Numerical Analysis/unit-9/covid-data-set/Covid19 India (Jan 20 - Mar 20).csv")
df <- Covid19_India_Jan_20_Mar_20_

names(df)

#total confirmed cases

df_total_confirmed <- df

df_total_confirmed$total_confirmed <- rowSums(
  df[, c("ConfirmedIndianNational", "ConfirmedForeignNational")],
  na.rm = TRUE
)

df_total_confirmed$ConfirmedIndianNational <- NULL
df_total_confirmed$ConfirmedForeignNational <- NULL
df_total_confirmed$Sno <- NULL

# Shapiro–Wilk normality test
#data does not apear to be normally distributed!!
for (col in names(df)) {
  
  if (is.numeric(df[[col]])) {
    
    x <- na.omit(df[[col]])
    
    if (length(x) >= 3 && length(x) <= 5000) {
      
      test <- shapiro.test(x)
      
      if (test$p.value > 0.05) {
        cat(
          col, ": Normally distributed",
          "(W =", round(test$statistic, 5),
          ", p =", round(test$p.value, 5), ")\n"
        )
      } else {
        cat(
          col, ": NOT normally distributed",
          "(W =", round(test$statistic, 5),
          ", p =", round(test$p.value, 5), ")\n"
        )
      }
    }
  }
}

#scatter plot observation
pairs.panels(
  df_total_confirmed,
  method = "kendall",     # correlation method
  hist.col = "grey",      # histogram colour
  density = TRUE,         # show density curves
  ellipses = FALSE,       # remove correlation ellipses
  scale = TRUE,
  cex.cor = 2.5,
  main = "Fresh Data Inspection",
  pch = 19
)


#pearson correlation
#Total Confirmed Cured
cor.test(
  df_total_confirmed$total_confirmed,
  df_total_confirmed$Cured,
  method = "pearson"
)

#Total COnfirmed Dead
cor.test(
  df_total_confirmed$total_confirmed,
  df_total_confirmed$Deaths,
  method = "pearson"
)

#Cured and Deaths
cor.test(
  df_total_confirmed$Cured,
  df_total_confirmed$Deaths,
  method = "pearson"
)


#linear regression
model <- lm(
  Deaths ~ total_confirmed,
  data = df_total_confirmed
)

summary(model)

coef(model)
