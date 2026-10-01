# Coding-assistant note: OpenAI Codex assisted with coding. The project owner made the key decisions on how to segment and manage the data.
# Dataset note: Uses CSEW 2013-14 SPSS microdata (UK Data Service study 7619). It is intentionally excluded from Git; obtain it from the UK Data Service or project owner and place it at unit-9/UKDA-7619-spss/spss/spss25/csew_apr13mar14_nvf.sav. Public study documentation: https://doc.ukdataservice.ac.uk/doc/7619/mrdoc/UKDA/UKDA_Study_7619_Information.htm; data access may require UK Data Service registration.
library(haven)
csew_apr13mar14_nvf <- read_sav("UKDA-7619-spss/spss/spss25/csew_apr13mar14_nvf.sav")
df <- csew_apr13mar14_nvf
table(df$sex,df$walkdark)
prop.table(table(df$sex,df$walkdark))
prop.table(table(df$sex,df$walkdark), 2)
round(prop.table(table(df$sex,df$walkdark), 2), 2)

#BY COLUMNS
round(prop.table(table(df$sex,df$walkdark), 2)*100, 2)

#by rows
round(prop.table(table(df$sex,df$walkdark), 1)*100, 2)

#overview of data
labels_df <- data.frame(
  variable = names(df),
  label = sapply(df, function(x) {
    lab <- attr(x, "label")
    if (is.null(lab) || length(lab) == 0) {
      NA_character_
    } else {
      as.character(lab)[1]
    }
  }),
  stringsAsFactors = FALSE
)

View(labels_df)



# bar plot for comparison walk home
barplot(
  t(tab),
  beside = TRUE,
  col = c("darkgreen", "lightgreen", "orange", "red"),
  xlab = "Sex",
  ylab = "Percentage",
  main = "Perceived safety when walking alone after dark",
  legend.text = colnames(tab),
  args.legend = list(x = "topright")
)

#chi squared test 
# extremly statistically significant difference between men and
tab_counts <- table(df$sex, df$walkdark)

chisq.test(tab_counts)


# Find variables
cause_vars <- grep("^causec2", names(df), value = TRUE)
source_vars <- grep("^impcrl2", names(df), value = TRUE)

# Function to get SPSS labels
get_label <- function(v) {
  lab <- attr(df[[v]], "label")
  if (is.null(lab) || length(lab) == 0) v else as.character(lab[1])
}

cause_labels <- sapply(cause_vars, get_label)
source_labels <- sapply(source_vars, get_label)

# Remove non-substantive responses
keep_cause <- !grepl("refused|don't know|do not prompt|none",
                     cause_labels, ignore.case = TRUE)

keep_source <- !grepl("refused|don't know|do not prompt|none",
                      source_labels, ignore.case = TRUE)

cause_vars <- cause_vars[keep_cause]
cause_labels <- cause_labels[keep_cause]

source_vars <- source_vars[keep_source]
source_labels <- source_labels[keep_source]

# Shorten cause labels
cause_labels <- sub(
  "^Major causes of crime in Britain today:\\s*",
  "",
  cause_labels
)

# Shorten source labels
source_labels <- sub(
  "^Where got impression about crime levels in your LOCAL AREA:\\s*",
  "",
  source_labels
)

# Percentage selecting each cause
cause_pct <- sapply(cause_vars, function(v) {
  x <- as.numeric(df[[v]])
  mean(x == 1, na.rm = TRUE) * 100
})

# Percentage selecting each source
source_pct <- sapply(source_vars, function(v) {
  x <- as.numeric(df[[v]])
  mean(x == 1, na.rm = TRUE) * 100
})

# -----------------------------
# Plot causes
# -----------------------------

par(mar = c(5, 12, 4, 2))
ord <- order(cause_pct, decreasing = TRUE)

barplot(
  cause_pct,
  names.arg = cause_labels,
  horiz = TRUE,
  las = 1,
  xlab = "Percentage selecting option",
  main = "Perceived major causes of crime",
  cex.names = 0.75
)

# -----------------------------
# Plot information sources
# -----------------------------

par(mar = c(5, 12, 4, 2))
ord2 <- order(source_pct, decreasing = TRUE)

barplot(
  source_pct,
  names.arg = source_labels,
  horiz = TRUE,
  las = 1,
  xlab = "Percentage selecting option",
  main = "Sources of impressions about local crime",
  cex.names = 0.75
)
