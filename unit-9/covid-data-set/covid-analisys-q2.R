# Coding-assistant note: OpenAI Codex assisted with coding. The project owner made the key decisions on how to segment and manage the data.
# Dataset note: Uses the COVID-19 India CSV and India boundary shapefile in unit-9/covid-data-set/. Keep the .csv, .shp, .dbf, .prj, .shx, and .cpg files together. Public COVID-19 India CSV archive: https://data.covid19india.org/documentation/csv/; public replacement boundary data: https://gadm.org/download_country.html
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)


# This script is run from unit-9/covid-data-set, so the CSV is in the current working directory.
Covid19_India_Jan_20_Mar_20_ <- read_csv("Covid19 India (Jan 20 - Mar 20).csv")

df <- Covid19_India_Jan_20_Mar_20_

# combine Indian and foreign confirmed cases
confirmed_df <- data.frame(
  ConfirmedCases = df[["ConfirmedIndianNational"]] +
    df[["ConfirmedForeignNational"]],
  Cured = df[["Cured"]]
)

# split reports into severity levels
severity_df <- confirmed_df

severity_df$Severity <- cut(
  severity_df$ConfirmedCases,
  breaks = c(0, 1, 3, 9, Inf),
  labels = c("Low", "Moderate", "High", "Severe"),
  include.lowest = TRUE
)

#check
table(severity_df$Cured)

# create recovery status
severity_df$RecoveryStatus <- ifelse(
  severity_df$Cured == 0,
  "No Recovery Recorded",
  "Recovered"
)

# cross-table: severity vs recovery status
severity_recovery_table <- table(
  severity_df$Severity,
  severity_df$RecoveryStatus
)

percentage_table <- round(
  prop.table(severity_recovery_table) * 100,
  0
)

addmargins(percentage_table, margin = 1)

#colour map matrix
# add total row to plot data
total_df <- plot_df %>%
  group_by(RecoveryStatus) %>%
  summarise(
    Percentage = sum(Percentage)
  ) %>%
  mutate(
    Severity = "Total"
  )

plot_df_5 <- bind_rows(
  plot_df,
  total_df
)

# set row order
plot_df_5$Severity <- factor(
  plot_df_5$Severity,
  levels = c(
    "Low",
    "Moderate",
    "High",
    "Severe",
    "Total"
  )
)



colnames(plot_df) <- c(
  "Severity",
  "RecoveryStatus",
  "Percentage"
)

ggplot(plot_df, aes(
  x = RecoveryStatus,
  y = Severity,
  fill = Percentage
)) +
  geom_tile(
    colour = "white",
    linewidth = 1.2
  ) +
  geom_text(
    aes(
      label = paste0(round(Percentage, 0), "%"),
      colour = Percentage > 50
    ),
    size = 5,
    fontface = "bold"
  ) +
  scale_fill_gradient(
    low = "lightblue",
    high = "darkblue"
  ) +
  scale_colour_manual(
    values = c("black", "white"),
    guide = "none"
  ) +
  labs(
    title = "Severity Level vs Recovery Status",
    x = "Recovery Status",
    y = "Case Severity Level",
    fill = "Percentage"
  ) +
  theme_minimal()
